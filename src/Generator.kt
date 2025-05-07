import java.lang.classfile.*
import java.lang.constant.*
import java.lang.reflect.AccessFlag


data class Context(val codeBuilder: CodeBuilder)


fun generateClassFile(name: String, expressions: List<Expression>): ByteArray {
    val classDescriptor = ClassDesc.of(name)
    val bytes = ClassFile.of().build(classDescriptor) { classBuilder ->
        generateEmptyConstructor(classBuilder)
        generateStaticConstructor(classBuilder, expressions)
        generateFile(classBuilder, expressions)
    }

    return bytes
}

fun generateFile(classBuilder: ClassBuilder, expressions: List<Expression>) {
    for (expression in expressions) {
        when (expression) {
            is Expression.Function -> generateFunction(classBuilder, expression)
            is Expression.Let -> generateStaticFieldDescription(classBuilder, expression)
            // Nothing to do for imports
            is Expression.Import -> {}
            else -> error("Generating expression " + expression + " not allowed at top level of a file")
        }
    }
}

fun generateEmptyConstructor(classBuilder: ClassBuilder) {
    classBuilder
        .withFlags(ClassFile.ACC_PUBLIC)
        .withMethod(
            ConstantDescs.INIT_NAME,
            ConstantDescs.MTD_void,
            ClassFile.ACC_PUBLIC
        ) { methodBuilder ->
            methodBuilder.withCode { codeBuilder ->
                codeBuilder.aload(0)
                    .invokespecial(
                        ConstantDescs.CD_Object,
                        ConstantDescs.INIT_NAME, ConstantDescs.MTD_void
                    )
                    .return_()
            }
        }


}

// generate a static field for a let expression
// the actual value is set in the static constructor
// IDEA: inline some constants like int or string directly in method calls
fun generateStaticFieldDescription(classBuilder: ClassBuilder, expression: Expression.Let) {
    val descriptor = getClassDescriptor(expression.name.type)
    classBuilder.withField(expression.name.identifier, descriptor, ClassFile.ACC_STATIC + ClassFile.ACC_PUBLIC)
}

fun generateStaticConstructor(classBuilder: ClassBuilder, expressions: List<Expression>) {
    classBuilder.withMethod(
        ConstantDescs.CLASS_INIT_NAME,
        ConstantDescs.MTD_void,
        ClassFile.ACC_STATIC
    ) { methodBuilder ->
        methodBuilder.withCode { codeBuilder ->
            val context = Context(codeBuilder)

            for (expression in expressions) {
                if (expression is Expression.Let) {
                    generateStaticFieldAssignment(context, expression)
                }
            }

            codeBuilder.return_()
        }
    }
}

fun generateStaticFieldAssignment(context: Context, let: Expression.Let) {
    if (let.name.type == Type.Concrete("void")) {
        error("Cannot set static field " + let.name + " with a void type")
    }

    generateExpression(context, let.expression)
    val type = getClassDescriptor(let.name.type)
    val ownerType = getOwnerType(let)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)
    context.codeBuilder.putstatic(ownerTypeDescriptor, let.name.identifier, type)
}

fun generateFunction(classBuilder: ClassBuilder, function: Expression.Function) {
    val name = function.name.identifier
    val methodTypeDescriptor = getMethodTypeDescriptor(function.name)
    val returnType = (function.name.type as Type.Arrow).returnType
    val flags = ClassFile.ACC_PUBLIC + ClassFile.ACC_STATIC
    classBuilder.withMethod(name, methodTypeDescriptor, flags) { methodBuilder ->
        methodBuilder.withCode { codeBuilder ->
            val context = Context(codeBuilder)
            generateBlock(context, function.body)

            if (!endsWithReturn(function.body)) {
                if (returnType == Type.Concrete("void")) {
                    codeBuilder.return_()
                } else {
                    error("Function " + function.name + " needs to return a value")
                }
            }
        }
    }
}

fun endsWithReturn(expressions: List<Expression>): Boolean {
    val lastExpression = expressions.lastOrNull()
    return when (lastExpression) {
        is Expression.Return -> true
        is Expression.If ->
            lastExpression.elseBranch != null
                    && endsWithReturn(lastExpression.thenBranch)
                    && endsWithReturn(lastExpression.elseBranch)

        else -> false
    }
}

fun generateBlock(context: Context, expressions: List<Expression>) {
    for (expression in expressions) {
        when (expression) {
            is Expression.Call -> generateCall(context, expression)
            is Expression.If -> generateIf(context, expression)
            is Expression.Let -> generateLet(context, expression)
            is Expression.Return -> generateReturn(context, expression)
            else -> error("Generating expression " + expression + " not allowed inside block")
        }
    }
}

fun generateReturn(context: Context, expression: Expression.Return) {
    if (expression.expression != null) {
        generateExpression(context, expression.expression)
        val type = readType(expression.expression)
        val typeKind = getTypeKind(type)
        context.codeBuilder.return_(typeKind)
    } else {
        context.codeBuilder.return_()
    }
}

fun generateIf(startContext: Context, ifExpression: Expression.If) {
    generateExpression(startContext, ifExpression.condition)
    if (ifExpression.elseBranch != null) {
        startContext.codeBuilder.ifThenElse({
            val context = Context(it)
            generateBlock(context, ifExpression.thenBranch)
        }, {
            val context = Context(it)
            generateBlock(context, ifExpression.elseBranch)
        })
    } else {
        startContext.codeBuilder.ifThen {
            val context = Context(it)
            generateBlock(context, ifExpression.thenBranch)
        }
    }
}

fun generateLet(context: Context, let: Expression.Let) {
    generateExpression(context, let.expression)
    val typeKind = getTypeKind(let.name.type)
    if (typeKind == TypeKind.VoidType) {
        error("Cannot assign field " + let.name + " with a void type")
    }

    val index = (let.info as Info.Local).index
    context.codeBuilder.storeLocal(typeKind, index)
}

fun generateCall(context: Context, call: Expression.Call) {
    when (call.function) {
        is Expression.Variable -> generateVariableCall(context, call.function, call.arguments)
        is Expression.Dot -> generateDotCall(context, call.function, call.arguments)
        else -> error("Expression " + pretty(call) + " cannot be called directly")
    }
}

fun generateVariableCall(context: Context, variable: Expression.Variable, arguments: List<Expression>) {
    val methodTypeDescriptor = getMethodTypeDescriptor(variable.name)

    // TODO imported functions
    // Here we basically assume, that calling a function always has the current class as owner
    // However, when importing, a different class will be the owner
    val ownerType = getOwnerType(variable)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)

    for (expression in arguments) {
        generateExpression(context, expression)
    }

    context.codeBuilder.invokestatic(ownerTypeDescriptor, variable.name.identifier, methodTypeDescriptor)
}

fun generateDotCall(context: Context, dot: Expression.Dot, arguments: List<Expression>) {
    val methodTypeDescriptor = getMethodTypeDescriptor(dot.name)
    val ownerType = readType(dot.expression)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)

    generateExpression(context, dot.expression)

    for (expression in arguments) {
        generateExpression(context, expression)
    }

    // TODO invokedynamic
    // research how to build DynamicCallSiteDesc
    // calls like list.forEach({ println(it) })
    // are translated into a static method
    // fn someLambda(it) { println(it) }
    // and the forEach path becomes
    // loadlocal
    // InvokeDynamic with some function info on someLambda
    // invokevirtual forEach
    if (isPrimitive(ownerType)) {
        generatePrimitiveCall(context, ownerTypeDescriptor, dot.name.identifier)
    } else if (isVirtual(dot)) {
        // Often a call is virtual, but in some cases the return is an interface, requiring invokeinterface
        // for example for `List.of("hi", "there").toString()`
        if (isInterface(ownerType)) {
            context.codeBuilder.invokeinterface(ownerTypeDescriptor, dot.name.identifier, methodTypeDescriptor)
        } else {
            context.codeBuilder.invokevirtual(ownerTypeDescriptor, dot.name.identifier, methodTypeDescriptor)
        }
    } else {
        // Necessary for static interface methods like List.of()
        val isInterface = isInterface(ownerType)
        context.codeBuilder.invokestatic(ownerTypeDescriptor, dot.name.identifier, methodTypeDescriptor, isInterface)
    }
}


// TODO doubles
// TODO conversion functions
// TODO logical functions
fun generatePrimitiveCall(context: Context, ownerTypeDescriptor: ClassDesc, identifier: String) {
    if (ownerTypeDescriptor.descriptorString() == "I") {
        when (identifier) {
            "equals" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPEQ, { it.iconst_1() }, { it.iconst_0() })
            "notEqual" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPNE, { it.iconst_1() }, { it.iconst_0() })
            "greaterOrEqual" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPGE, { it.iconst_1() }, { it.iconst_0() })
            "greater" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPGT, { it.iconst_1() }, { it.iconst_0() })
            "lesserOrEqual" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPLE, { it.iconst_1() }, { it.iconst_0() })
            "lesser" -> context.codeBuilder.ifThenElse(Opcode.IF_ICMPLT, { it.iconst_1() }, { it.iconst_0() })
            "plus" -> context.codeBuilder.iadd()
            "minus" -> context.codeBuilder.isub()
            "times" -> context.codeBuilder.imul()
            "div" -> context.codeBuilder.idiv()
            "rem" -> context.codeBuilder.irem()
            else -> error("Unknown primitive int call " + identifier)
        }
    } else if (ownerTypeDescriptor.descriptorString() == "L") {
        when (identifier) {
            "equals" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFEQ, { it.iconst_1() }, { it.iconst_0() })
            "notEqual" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFNE, { it.iconst_1() }, { it.iconst_0() })
            "greaterOrEqual" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFGE, { it.iconst_1() }, { it.iconst_0() })
            "greater" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFGT, { it.iconst_1() }, { it.iconst_0() })
            "lesserOrEqual" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFLE, { it.iconst_1() }, { it.iconst_0() })
            "lesser" -> context.codeBuilder.lcmp().ifThenElse(Opcode.IFLT, { it.iconst_1() }, { it.iconst_0() })
            "plus" -> context.codeBuilder.ladd()
            "minus" -> context.codeBuilder.lsub()
            "times" -> context.codeBuilder.lmul()
            "div" -> context.codeBuilder.ldiv()
            "rem" -> context.codeBuilder.lrem()
            else -> error("Unknown primitive long call " + identifier)
        }
    } else if (ownerTypeDescriptor.descriptorString() == "F") {
        when (identifier) {
            // fcmpl and fcmpg seem switched in these branches, but lead to the correct output in the disassembly
            "equals" -> context.codeBuilder.fcmpg().ifThenElse(Opcode.IFEQ, { it.iconst_1() }, { it.iconst_0() })
            "notEqual" -> context.codeBuilder.fcmpg().ifThenElse(Opcode.IFNE, { it.iconst_1() }, { it.iconst_0() })
            "greaterOrEqual" -> context.codeBuilder.fcmpl().ifThenElse(Opcode.IFGE, { it.iconst_1() }, { it.iconst_0() })
            "greater" -> context.codeBuilder.fcmpl().ifThenElse(Opcode.IFGT, { it.iconst_1() }, { it.iconst_0() })
            "lesserOrEqual" -> context.codeBuilder.fcmpg().ifThenElse(Opcode.IFLE, { it.iconst_1() }, { it.iconst_0() })
            "lesser" -> context.codeBuilder.fcmpg().ifThenElse(Opcode.IFLT, { it.iconst_1() }, { it.iconst_0() })
            "plus" -> context.codeBuilder.fadd()
            "minus" -> context.codeBuilder.fsub()
            "times" -> context.codeBuilder.fmul()
            "div" -> context.codeBuilder.fdiv()
            "rem" -> context.codeBuilder.frem()
            else -> error("Unknown primitive float call " + identifier)
        }
    } else if (ownerTypeDescriptor.descriptorString() == "D") {
        when (identifier) {
            "equals" -> context.codeBuilder.dcmpg().ifThenElse(Opcode.IFEQ, { it.iconst_1() }, { it.iconst_0() })
            "notEqual" -> context.codeBuilder.dcmpg().ifThenElse(Opcode.IFNE, { it.iconst_1() }, { it.iconst_0() })
            "greaterOrEqual" -> context.codeBuilder.dcmpl().ifThenElse(Opcode.IFGE, { it.iconst_1() }, { it.iconst_0() })
            "greater" -> context.codeBuilder.dcmpl().ifThenElse(Opcode.IFGT, { it.iconst_1() }, { it.iconst_0() })
            "lesserOrEqual" -> context.codeBuilder.dcmpg().ifThenElse(Opcode.IFLE, { it.iconst_1() }, { it.iconst_0() })
            "lesser" -> context.codeBuilder.dcmpg().ifThenElse(Opcode.IFLT, { it.iconst_1() }, { it.iconst_0() })
            "plus" -> context.codeBuilder.dadd()
            "minus" -> context.codeBuilder.dsub()
            "times" -> context.codeBuilder.dmul()
            "div" -> context.codeBuilder.ddiv()
            "rem" -> context.codeBuilder.drem()
            else -> error("Unknown primitive double call " + identifier)
        }
    } else if (ownerTypeDescriptor.descriptorString() == "Z") {
        when (identifier) {
            "not" -> context.codeBuilder.ifThenElse(Opcode.IFEQ, { it.iconst_1() }, { it.iconst_0() })
            else -> error("Unknown primitive bool call " + identifier)
        }
    }
}

fun isInterface(type: Type): Boolean {
    val path = (type as Type.Concrete).name
    val classModel = getClassFile(path)
    return classModel.flags().has(AccessFlag.INTERFACE)
}

fun generateLiteral(context: Context, literal: Literal) {
    when (literal) {
        is Literal.BooleanLiteral -> if (literal.bool) {
            context.codeBuilder.iconst_1()
        } else {
            context.codeBuilder.iconst_0()
        }

        is Literal.StringLiteral -> context.codeBuilder.loadConstant(literal.string as ConstantDesc)
        is Literal.IntLiteral -> context.codeBuilder.loadConstant(literal.number as ConstantDesc)
        is Literal.DoubleLiteral -> context.codeBuilder.loadConstant(literal.number as ConstantDesc)
        is Literal.FloatLiteral -> context.codeBuilder.loadConstant(literal.number as ConstantDesc)
        is Literal.LongLiteral -> context.codeBuilder.loadConstant(literal.number as ConstantDesc)
    }
}

fun generateExpression(context: Context, expression: Expression) {
    when (expression) {
        is Expression.Lit -> generateLiteral(context, expression.literal)
        is Expression.Call -> generateCall(context, expression)
        is Expression.Variable -> generateVariable(context, expression)
        is Expression.Dot -> generateDot(context, expression)
        else -> error("Generating expression " + expression + " not allowed at this level")
    }
}

fun generateVariable(context: Context, variable: Expression.Variable) {
    when (variable.info) {
        is Info.Local -> {
            val typeKind = getTypeKind(variable.name.type)
            if (typeKind == TypeKind.VoidType) {
                error("Cannot get a field " + variable.name + " with a void type")
            }

            context.codeBuilder.loadLocal(typeKind, variable.info.index)
        }

        is Info.Static -> {
            val fieldType = getClassDescriptor(variable.name.type)
            val ownerType = variable.info.ownerType
            val ownerTypeDescriptor = getClassDescriptor(ownerType)
            context.codeBuilder.getstatic(ownerTypeDescriptor, variable.name.identifier, fieldType)
        }

        is Info.Outside -> {
            // Nothing to generate for static name references
            // for example for System.out, System does not need to be generated
        }
    }
}

fun generateDot(context: Context, dot: Expression.Dot) {
    val fieldType = getClassDescriptor(dot.name.type)
    val ownerType = readType(dot.expression)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)
    generateExpression(context, dot.expression)
    if (isVirtual(dot)) {
        context.codeBuilder.getfield(ownerTypeDescriptor, dot.name.identifier, fieldType)
    } else {
        context.codeBuilder.getstatic(ownerTypeDescriptor, dot.name.identifier, fieldType)
    }
}

fun getClassDescriptor(type: Type): ClassDesc {
    return when (type) {
        is Type.Concrete -> when (type.name) {
            "void" -> ConstantDescs.CD_void
            "int" -> ConstantDescs.CD_int
            "long" -> ConstantDescs.CD_long
            "float" -> ConstantDescs.CD_float
            "double" -> ConstantDescs.CD_double
            "short" -> ConstantDescs.CD_short
            "byte" -> ConstantDescs.CD_byte
            "char" -> ConstantDescs.CD_char
            "bool" -> ConstantDescs.CD_boolean
            "string" -> ConstantDescs.CD_String
            "Any" -> ConstantDescs.CD_Object
            else ->
                if (type.name.startsWith("[") && type.name.endsWith("]")) {
                    // TODO handle arrays recursively
                    val innerName = type.name.substring(1, type.name.length - 1)
                    ClassDesc.ofInternalName(innerName).arrayType()
                } else {
                    // ofInternalName works on names with slashes
                    ClassDesc.ofInternalName(type.name)
                }
        }

        else -> error("Cannot turn type " + type + " into concrete type")
    }
}

fun getTypeKind(type: Type): TypeKind {
    return when (type) {
        is Type.Concrete -> when (type.name) {
            "void" -> TypeKind.VoidType
            "int" -> TypeKind.IntType
            "long" -> TypeKind.LongType
            "float" -> TypeKind.FloatType
            "double" -> TypeKind.DoubleType
            "short" -> TypeKind.ShortType
            "byte" -> TypeKind.ByteType
            "char" -> TypeKind.CharType
            "bool" -> TypeKind.BooleanType
            else -> TypeKind.ReferenceType
        }

        else -> error("Cannot turn type " + type + " into type kind")
    }
}

fun isVirtual(dot: Expression.Dot): Boolean {
    return when (dot.expression) {
        is Expression.Variable -> dot.expression.info !is Info.Outside
        else -> true
    }
}

fun getMethodTypeDescriptor(name: Name): MethodTypeDesc {
    return when (name.type) {
        is Type.Arrow -> MethodTypeDesc.of(
            getClassDescriptor(name.type.returnType),
            name.type.parameterTypes.map { getClassDescriptor(it) })

        is Type.Concrete -> error("Unexpected type " + name.type.name + " for function " + name.identifier)
    }
}

fun getOwnerType(expression: Expression): Type {
    return when (expression) {
        is Expression.Let -> if (expression.info is Info.Static) { expression.info.ownerType } else { error("Cannot read owner type of " + expression) }
        is Expression.Variable -> if (expression.info is Info.Static) { expression.info.ownerType } else { error("Cannot read owner type of " + expression) }
        else -> error("Cannot read owner type of " + expression)
    }
}