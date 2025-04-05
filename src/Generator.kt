import java.lang.constant.ConstantDescs
import java.lang.classfile.*
import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDesc
import java.lang.constant.MethodTypeDesc
import java.nio.file.Files
import java.nio.file.Paths


data class Context(val codeBuilder: CodeBuilder, val classDescriptor: ClassDesc, val locals: List<Name>)


fun writeClassFile(name: String, expressions: List<Expression>) {
    val classDescriptor = ClassDesc.of(name)
    val bytes = ClassFile.of().build(classDescriptor) { classBuilder ->
        classBuilder
            .withFlags(ClassFile.ACC_PUBLIC)
            .withMethod(
                ConstantDescs.INIT_NAME, ConstantDescs.MTD_void,
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

        generateStaticConstructor(classBuilder, classDescriptor, expressions)
        generateFile(classBuilder, classDescriptor, expressions)
    }

    Files.write(Paths.get(name + ".class"), bytes)
}

fun generateFile(classBuilder: ClassBuilder, classDescriptor: ClassDesc, expressions: List<Expression>) {
    for (expression in expressions) {
        when (expression) {
            is Expression.Function -> generateFunction(classBuilder, classDescriptor, expression)
            is Expression.Let -> generateStaticFieldDescription(classBuilder, expression)
            // Nothing to do for imports
            is Expression.Import -> {}
            else -> TODO("Expression at top level not allowed " + expression)
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

fun generateStaticConstructor(classBuilder: ClassBuilder, classDescriptor: ClassDesc, expressions: List<Expression>) {
    classBuilder.withMethod(
        ConstantDescs.CLASS_INIT_NAME,
        ConstantDescs.MTD_void,
        ClassFile.ACC_STATIC
    ) { methodBuilder ->
        methodBuilder.withCode { codeBuilder ->
            val context = Context(codeBuilder, classDescriptor, listOf())

            for (expression in expressions) {
                if (expression is Expression.Let) {
                    generateStaticFieldAssignment(context, expression)
                }
            }

            codeBuilder.return_()
        }
    }
}

fun generateStaticFieldAssignment(context: Context, expression: Expression.Let) {
    generateExpression(context, expression.expression)
    val type = getClassDescriptor(expression.name.type)
    context.codeBuilder.putstatic(context.classDescriptor, expression.name.identifier, type)
}

fun generateFunction(classBuilder: ClassBuilder, classDescriptor: ClassDesc, function: Expression.Function) {
    val name = function.name.identifier
    val methodTypeDescriptor = getMethodTypeDescriptor(function.name)
    val flags = ClassFile.ACC_PUBLIC + ClassFile.ACC_STATIC
    classBuilder.withMethod(name, methodTypeDescriptor, flags) { methodBuilder ->
        methodBuilder.withCode { codeBuilder ->
            val locals = function.parameters + gatherLocals(function.body)
            val context = Context(codeBuilder, classDescriptor, locals)
            generateBlock(context, function.body)

            // TODO check if return is void
            if (!endsWithReturn(function.body)) {
                codeBuilder.return_()
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

fun gatherLocals(expressions: List<Expression>): List<Name> {
    var locals = listOf<Name>()
    for (expression in expressions) {
        when (expression) {
            is Expression.If -> {
                val thenLocals = gatherLocals(expression.thenBranch)
                val elseLocals = if (expression.elseBranch != null) {
                    gatherLocals(expression.elseBranch)
                } else {
                    listOf()
                }
                locals = locals + thenLocals + elseLocals
            }

            is Expression.Let -> {
                locals = locals + expression.name
            }

            else -> {}
        }
    }

    return locals
}

fun generateBlock(context: Context, expressions: List<Expression>) {
    for (expression in expressions) {
        when (expression) {
            is Expression.Call -> generateCall(context, expression)
            is Expression.If -> generateIf(context, expression)
            is Expression.Let -> generateLet(context, expression)
            is Expression.Return -> generateReturn(context, expression)
            else -> TODO()
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

// TODO probably has to return the context so that we can keep track of changes
// TODO avoid adding locals several times
fun generateIf(startContext: Context, ifExpression: Expression.If) {
    if (ifExpression.elseBranch != null) {
        startContext.codeBuilder.ifThenElse({
            val context = Context(it, startContext.classDescriptor, startContext.locals)
            generateBlock(context, ifExpression.thenBranch)
        }, {
            val context = Context(it, startContext.classDescriptor, startContext.locals)
            generateBlock(context, ifExpression.elseBranch)
        })
    } else {
        startContext.codeBuilder.ifThen {
            val context = Context(it, startContext.classDescriptor, startContext.locals)
            generateBlock(context, ifExpression.thenBranch)
        }
    }
}

fun generateLet(context: Context, let: Expression.Let) {
    generateExpression(context, let.expression)
    val index = context.locals.indexOfFirst { let.name.identifier == it.identifier }
    val typeKind = getTypeKind(let.name.type)
    context.codeBuilder.storeLocal(typeKind, index)
}

fun generateCall(context: Context, call: Expression.Call) {
    when (call.function) {
        is Expression.Variable -> generateVariableCall(context, call.function, call.arguments)
        is Expression.Dot -> generateDotCall(context, call.function, call.arguments)
        else -> TODO()
    }
}

fun generateVariableCall(context: Context, variable: Expression.Variable, arguments: List<Expression>) {
    val methodTypeDescriptor = getMethodTypeDescriptor(variable.name)
    // TODO when importing functions, a variable could also come from a different class
    val ownerTypeDescriptor = context.classDescriptor

    for (expression in arguments) {
        generateExpression(context, expression)
    }

    context.codeBuilder.invokestatic(ownerTypeDescriptor, variable.name.identifier, methodTypeDescriptor)
}

fun generateDotCall(context: Context, dot: Expression.Dot, arguments: List<Expression>) {
    val methodTypeDescriptor = getMethodTypeDescriptor(dot.name)
    val ownerType = readType(dot.expression)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)

    // TODO invokeinterface
    if (isStatic(dot)) {
        for (expression in arguments) {
            generateExpression(context, expression)
        }

        context.codeBuilder.invokestatic(ownerTypeDescriptor, dot.name.identifier, methodTypeDescriptor)
    } else {
        // generate the expression part, for example
        // the System.out in System.out.println
        generateExpression(context, dot.expression)
        for (expression in arguments) {
            generateExpression(context, expression)
        }

        context.codeBuilder.invokevirtual(ownerTypeDescriptor, dot.name.identifier, methodTypeDescriptor)
    }
}

fun generateExpression(context: Context, expression: Expression) {
    when (expression) {
        is Expression.BooleanLiteral -> if (expression.bool) {
            context.codeBuilder.iconst_1()
        } else {
            context.codeBuilder.iconst_0()
        }

        is Expression.Call -> generateCall(context, expression)
        is Expression.Variable -> generateVariable(context, expression)
        is Expression.Dot -> generateDot(context, expression)
        is Expression.If -> TODO()
        is Expression.Let -> TODO()
        is Expression.NumberLiteral -> context.codeBuilder.ldc(expression.number as ConstantDesc)
        is Expression.Return -> TODO()
        is Expression.StringLiteral -> context.codeBuilder.ldc(expression.string as ConstantDesc)
        // Not allowed
        is Expression.Function -> TODO()
        is Expression.Import -> TODO()
    }
}

fun generateVariable(context: Context, variable: Expression.Variable) {
    val index = context.locals.indexOfFirst { variable.name.identifier == it.identifier }
    if (index != -1) {
        val typeKind = getTypeKind(variable.name.type)
        context.codeBuilder.loadLocal(typeKind, index)
    } else {
        val fieldType = getClassDescriptor(variable.name.type)
        // For now every variable is assumed to be a static field access
        val ownerTypeDescriptor = context.classDescriptor
        context.codeBuilder.getstatic(ownerTypeDescriptor, variable.name.identifier, fieldType)
    }
}


fun generateDot(context: Context, dot: Expression.Dot) {
    val fieldType = getClassDescriptor(dot.name.type)
    val ownerType = readType(dot.expression)
    val ownerTypeDescriptor = getClassDescriptor(ownerType)
    if (isStatic(dot)) {
        context.codeBuilder.getstatic(ownerTypeDescriptor, dot.name.identifier, fieldType)
    } else {
        generateExpression(context, dot.expression)
        context.codeBuilder.getfield(ownerTypeDescriptor, dot.name.identifier, fieldType)
    }
}

fun getClassDescriptor(t: Type): ClassDesc {
    return when (t) {
        is Type.Concrete -> when (t.name) {
            "void" -> ConstantDescs.CD_void
            "int" -> ConstantDescs.CD_int
            "long" -> ConstantDescs.CD_long
            "float" -> ConstantDescs.CD_float
            "double" -> ConstantDescs.CD_double
            "short" -> ConstantDescs.CD_short
            "byte" -> ConstantDescs.CD_byte
            "char" -> ConstantDescs.CD_char
            "boolean" -> ConstantDescs.CD_boolean
            "Any" -> ConstantDescs.CD_Object
            else ->
                if (t.name.startsWith("[") && t.name.endsWith("]")) {
                    // TODO handle array or arrays
                    val innerName = t.name.substring(1, t.name.length - 1)
                    ClassDesc.ofInternalName(innerName).arrayType()
                } else {
                    // ofInternalName works on names with slashes
                    ClassDesc.ofInternalName(t.name)
                }
        }
        else -> TODO("Cannot turn into concrete type")
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
            "boolean" -> TypeKind.BooleanType
            else -> TypeKind.ReferenceType
        }
        else -> TODO("Cannot turn into type kind")
    }
}

// TODO determine if variable is static from environment
fun isStatic(dot: Expression.Dot): Boolean {
    return when (dot.expression) {
        is Expression.Variable -> dot.expression.name.identifier.first().isUpperCase()
        else -> false
    }
}

fun getMethodTypeDescriptor(name: Name): MethodTypeDesc {
    return when (name.type) {
        is Type.Arrow -> MethodTypeDesc.of(getClassDescriptor(name.type.returnType), name.type.parameterTypes.map { getClassDescriptor(it) })
        is Type.Concrete -> TODO("Unexpected type " + name.type.name + " for function " + name.identifier)
    }
}