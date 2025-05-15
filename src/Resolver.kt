import java.lang.classfile.*
import java.lang.constant.ConstantDescs
import java.lang.reflect.AccessFlag
import java.nio.file.Files
import java.nio.file.Paths

data class Environment(val types: Map<String, Type>)

fun Environment.addMany(types: List<Pair<String, Type>>): Environment {
    return Environment(this.types + types)
}

fun Environment.add(key: String, value: Type): Environment {
    return Environment(this.types + Pair(key, value))
}

val baseEnvironment = mapOf(
    Pair("string", stringType),
    Pair("Parameters", Type.Concrete("Parameters")),
    Pair("emptyArray", Type.Arrow(Type.Concrete("EmptyArray"), listOf())),
)

fun resolveFile(expressions: List<Expression>): List<Expression> {
    var environment = Environment(baseEnvironment)

    return expressions.map { expression ->
        when (expression) {
            is Expression.Function -> {
                val resolved = resolveFunction(expression, environment)
                environment = environment.add(resolved.name.identifier, resolved.name.type)
                resolved
            }

            is Expression.Import -> {
                val path = expression.path.joinToString("/")
                val classFile = getClassFile(path)
                // TODO decide how to add classes to the env
                environment = environment.add(expression.path.last(), Type.Concrete(path))
                expression
            }

            is Expression.Let -> {
                val resolved = resolveLet(expression, environment)
                environment = environment.add(resolved.name.identifier, resolved.name.type)
                resolved
            }

            else -> error("Non top level expression " + expression + " at top level")
        }
    }
}

fun resolveLet(let: Expression.Let, environment: Environment): Expression.Let {
    val resolvedExpression = resolveExpression(let.expression, environment)
    val type = readType(resolvedExpression)
    val resolvedName = Name(let.name.identifier, type)
    return Expression.Let(resolvedName, resolvedExpression, let.info)
}

fun resolveExpression(expression: Expression, environment: Environment): Expression {
    return when (expression) {
        is Expression.Lit -> expression
        is Expression.Call -> resolveCall(expression, environment)
        is Expression.Dot -> resolveDot(expression, environment)
        is Expression.Variable -> resolveVariable(expression, environment)
        else -> error("Expression " + expression + " not allowed at this level")
    }
}

fun resolveReturn(expression: Expression.Return, environment: Environment): Expression.Return {
    return if (expression.expression != null) {
        val resolvedExpression = resolveExpression(expression.expression, environment)
        Expression.Return(resolvedExpression)
    } else {
        expression
    }
}

fun resolveIf(ifExpression: Expression.If, environment: Environment): Expression.If {
    val condition = resolveExpression(ifExpression.condition, environment)
    val thenBranch = resolveBlock(ifExpression.thenBranch, environment)
    val elseBranch = if (ifExpression.elseBranch != null) {
        resolveBlock(ifExpression.elseBranch, environment)
    } else {
        null
    }

    return Expression.If(
        condition,
        thenBranch,
        elseBranch
    )
}

fun resolveCall(call: Expression.Call, environment: Environment): Expression.Call {
    return when (call.function) {
        // The arguments are passed as parameter, because some functions, like comparison operators,
        // need the arguments for rewriting
        is Expression.Dot -> resolveDotCall(call.function, call.arguments, environment)
        is Expression.Variable -> resolveVariableCall(call.function, call.arguments, environment)
        else -> error("Cannot call " + call.function + " because it is not a function")
    }
}

fun readType(expression: Expression): Type {
    return when (expression) {
        is Expression.Lit -> readType(expression.literal)
        is Expression.Call -> {
            val arrowType = readType(expression.function)
            if (arrowType is Type.Arrow) {
                return arrowType.returnType
            } else {
                error("The type of a call " + expression + " has to be an arrow type")
            }
        }
        is Expression.Dot -> expression.name.type
        is Expression.Variable -> expression.name.type
        else -> error("Cannot read type of expression " + expression)
    }
}

fun readType(literal: Literal): Type {
    return when (literal) {
        is Literal.BooleanLiteral -> Type.Concrete("bool")
        is Literal.StringLiteral -> stringType
        is Literal.IntLiteral -> Type.Concrete("int")
        is Literal.DoubleLiteral -> Type.Concrete("double")
        is Literal.FloatLiteral -> Type.Concrete("float")
        is Literal.LongLiteral -> Type.Concrete("long")
    }
}

fun resolveConstructor(variable: Expression.Variable, types: List<Type>, environment: Environment): Expression.Variable {
    val type = environment.types[variable.name.identifier] ?: error("Not found " + variable.name.identifier)
    val initType = getInitType(type, types)
    // the constructor has return type void, but here we treat it as returning the object
    val constructorType = Type.Arrow(type, initType.parameterTypes)
    return Expression.Variable(Name(variable.name.identifier, constructorType), variable.info)
}

fun emptyArrayWorkaround(expression: Expression, expectedType: Type): Expression {
    if (expression is Expression.Call && expression.function is Expression.Variable && expression.function.name.identifier == "emptyArray") {
        val name = Name(expression.function.name.identifier, Type.Arrow(expectedType, listOf()))
        return Expression.Call(Expression.Variable(name, expression.function.info), expression.arguments)
    }

    return expression
}

fun resolveVariableCall(variable: Expression.Variable, arguments: List<Expression>, environment: Environment): Expression.Call {
    val resolvedArguments = arguments.map { resolveExpression(it, environment) }
    val argumentTypes = resolvedArguments.map { readType(it) }

    // TODO find overloads with resolved argument types
    val resolvedVariable = if (variable.info is Info.Outside) {
        resolveConstructor(variable, argumentTypes, environment)
    } else {
        resolveVariable(variable, environment)
    }

    val functionType = readType(resolvedVariable)

    if (functionType !is Type.Arrow) {
        error("Type is not a function type")
    }

    if (!zipAccepts(functionType.parameterTypes, argumentTypes)) {
        error("Type of arguments " + argumentTypes + " did not match type of parameters " + functionType.parameterTypes)
    }

    val fixedArguments = resolvedArguments.zip(functionType.parameterTypes, ::emptyArrayWorkaround)
    return Expression.Call(resolvedVariable, fixedArguments)
}

// TODO primitive calls in functions like
// `fun eq(a) { return a == 2 }` or `fun pl(a) { a + 2 }`
// should turn parameters into their primitive types
fun resolveDotCall(dot: Expression.Dot, arguments: List<Expression>, environment: Environment): Expression.Call {
    val resolvedArguments = arguments.map { resolveExpression(it, environment) }

    val resolvedExpression = resolveExpression(dot.expression, environment)
    val leftType = readType(resolvedExpression)

    // Special cases for operators
    if (!isPrimitive(leftType) && resolvedArguments.count() == 1) {
        if (isComparison(dot.name.identifier)) {
            return makeComparison(dot.name.identifier, resolvedExpression, resolvedArguments.first())
        } else if (dot.name.identifier == "notEqual") {
            return makeNotEqual(resolvedExpression, resolvedArguments.first())
        } else if (dot.name.identifier == "plus" && leftType == stringType) {
            // TODO separate concat operator?
            return makeConcat(resolvedExpression, resolvedArguments.first())
        }
    }

    val argumentTypes = resolvedArguments.map { readType(it) }
    val functionType = getMethodType(leftType, dot.name.identifier, argumentTypes)
    val newName = Name(dot.name.identifier, functionType)
    val resolvedDot = Expression.Dot(resolvedExpression, newName)

    val fixedArguments = resolvedArguments.zip(functionType.parameterTypes, ::emptyArrayWorkaround)
    return Expression.Call(resolvedDot, fixedArguments)
}

// rewrite a.greaterOrEqual(b) to a.compareTo(b) >= 0
fun makeComparison(identifier: String, left: Expression, right: Expression): Expression.Call {
    val compareToType = Type.Arrow(Type.Concrete("int"), listOf(any))
    val intCompareType = Type.Arrow(Type.Concrete("int"), listOf(Type.Concrete("int")))
    val compareTo = Expression.Call(Expression.Dot(left, Name("compareTo", compareToType)), listOf(right))
    return Expression.Call(
        Expression.Dot(compareTo, Name(identifier, intCompareType)),
        listOf(Expression.Lit(Literal.IntLiteral(0)))
    )
}

// rewrite a.notEqual(b) to a.equals(b).not()
fun makeNotEqual(left: Expression, right: Expression): Expression.Call {
    val equalsType = Type.Arrow(Type.Concrete("bool"), listOf(any))
    val notType = Type.Arrow(Type.Concrete("bool"), listOf())
    val equalsPart = Expression.Call(Expression.Dot(left, Name("equals", equalsType)), listOf(right))
    return Expression.Call(Expression.Dot(equalsPart, Name("not", notType)), listOf())
}

fun makeConcat(left: Expression, right: Expression): Expression.Call {
    val concatType = Type.Arrow(stringType, listOf(stringType))
    return Expression.Call(Expression.Dot(left, Name("concat", concatType)), listOf(right))
}

fun isPrimitive(type: Type): Boolean {
    return listOf(
        Type.Concrete("int"),
        Type.Concrete("long"),
        Type.Concrete("float"),
        Type.Concrete("double"),
        Type.Concrete("bool")
    ).contains(type)
}

fun isPrimitiveOperation(identifier: String): Boolean {
    return listOf(
        "plus", "minus",
        "times", "div",
        "rem",
        "equals", "notEquals",
        "lesser", "lesserOrEqual",
        "greater", "greaterOrEqual").contains(identifier)
}

fun isComparison(identifier: String): Boolean {
    return listOf("lesser", "lesserOrEqual", "greater", "greaterOrEqual").contains(identifier)
}

fun resolveDot(dot: Expression.Dot, environment: Environment): Expression {
    val resolvedExpression = resolveExpression(dot.expression, environment)
    val leftType = readType(resolvedExpression)
    val accessorType = getAccessorType(leftType, dot.name.identifier)
    val newName = Name(dot.name.identifier, accessorType)
    return Expression.Dot(resolvedExpression, newName)
}

fun getMethodType(type: Type, accessor: String, argumentTypes: List<Type>): Type.Arrow {
    // TODO read pseudo class file instead of specifying all those primitive calls in code
    if (isPrimitive(type)) {
        return getPrimitiveMethodType(type, accessor)
    }

    val path = (type as Type.Concrete).name
    val options = getMethods(path).filter { it.methodName().stringValue() == accessor }
    val overloads = options.map { convertMethodType(it.methodType().stringValue()) }

    val methodType = overloads.firstOrNull { zipAccepts(it.parameterTypes, argumentTypes) }

    return methodType
        ?: error(
            "Cannot find fitting overload for "
                    + accessor + " with arguments "
                    + argumentTypes + " in "
                    + overloads + " for owner "
                    + type
        )
}

fun getInitType(type: Type, argumentTypes: List<Type>): Type.Arrow {
    return getMethodType(type, ConstantDescs.INIT_NAME, argumentTypes)
}

fun getPrimitiveMethodType(type: Type, accessor: String): Type.Arrow {
    return when (accessor) {
        "plus", "minus", "times", "div", "rem" -> Type.Arrow(type, listOf(type))
        "equals" -> Type.Arrow(Type.Concrete("bool"), listOf(type))
        "notEqual" -> Type.Arrow(Type.Concrete("bool"), listOf(type))
        "compareTo" -> Type.Arrow(Type.Concrete("int"), listOf(type))
        "lesserOrEqual" -> Type.Arrow(Type.Concrete("int"), listOf(type))
        "lesser" -> Type.Arrow(Type.Concrete("int"), listOf(type))
        "greaterOrEqual" -> Type.Arrow(Type.Concrete("int"), listOf(type))
        "greater" -> Type.Arrow(Type.Concrete("int"), listOf(type))
        "not" -> Type.Arrow(Type.Concrete("bool"), listOf())
        "and", "or" -> Type.Arrow(Type.Concrete("bool"), listOf(Type.Concrete("bool")))
        else -> error("Can't find method type for primitive " + type + " " + accessor)
    }
}

fun zipAccepts(parameterTypes: List<Type>, argumentTypes: List<Type>): Boolean {
    return parameterTypes.count() == argumentTypes.count()
            && parameterTypes.zip(argumentTypes, { a, b -> accepts(a, b) }).all { it }
}

// if a function accepts an object as parameter, for example equals(Object o)
// then it accepts any reference parameter
fun accepts(parameterType: Type, argumentType: Type): Boolean {
    return parameterType == argumentType
        || (!isPrimitive(argumentType) && parameterType == any)
        || (argumentType is Type.Arrow && isFunctionInterface(parameterType))
        || (isArrayType(argumentType) && parameterType == arrayOfAny)
        || (isArrayType(parameterType) && argumentType == emptyArrayType)
}

val classFileCache = mutableMapOf<String, ClassModel>()

fun getClassFile(path: String): ClassModel {
    return classFileCache.getOrPut(path, { findClassFile(path) })
}

fun findClassFileBytes(path: String): ByteArray {
    val bytes = if (path == "Any") {
        ClassLoader.getSystemResourceAsStream("java/lang/Object.class")?.readAllBytes()
    } else if (path == "string") {
        ClassLoader.getSystemResourceAsStream("java/lang/String.class")?.readAllBytes()
    } else if (path.startsWith("java/")) {
        ClassLoader.getSystemResourceAsStream(path + ".class")?.readAllBytes()
    } else if (path.startsWith("kotlin/")) {
        Any::class.java.getResourceAsStream(path + ".class")?.readAllBytes()
    } else {
        // Allow reading archives and add a library folder
        Files.readAllBytes(Paths.get(path + ".class"))
    }

    if (bytes == null) {
        error("Cannot open path " + path)
    }

    println("Found " + path)

    return bytes
}

fun findClassFile(path: String): ClassModel {
    val bytes = findClassFileBytes(path)
    val classFile = ClassFile.of().parse(bytes)
    return classFile
}

// returns the methods including parent and interface methods
fun getMethods(path: String): List<MethodModel> {
    val classFile = getClassFile(path)
    return getMethods(classFile)
}

fun getMethods(classModel: ClassModel): List<MethodModel> {
    val superclassPaths = classModel.superclass().map { it.name().stringValue() }
    val interfacePaths = classModel.interfaces().map { it.name().stringValue() }
    val superclassMethods = superclassPaths.map { getMethods(it) }.orElse(emptyList())
    val interfaceMethods = interfacePaths.flatMap { getMethods(it) }
    return classModel.methods() + superclassMethods + interfaceMethods
}

// TODO non-static field access
// public fields should be modeled with a getter
// so instead of someObject.publicField, it should become someObject.publicField() or someObject.getPublicField()
fun getAccessorType(type: Type, accessor: String): Type {
    val path = (type as Type.Concrete).name
    val classFile = getClassFile(path)
    val options = classFile.fields().filter {
        it.flags().has(AccessFlag.STATIC) && it.fieldName().stringValue() == accessor
    }

    if (!options.any()) {
        error("No options for " + accessor + " and type " + type)
    }

    val stringValue = options.first().fieldType().stringValue()
    return convertFieldType(stringValue)
}

fun resolveVariable(variable: Expression.Variable, environment: Environment): Expression.Variable {
    val type = environment.types[variable.name.identifier] ?: error("Not found " + variable.name.identifier)
    return Expression.Variable(Name(variable.name.identifier, type), variable.info)
}

fun resolveBlock(expressions: List<Expression>, startEnvironment: Environment): List<Expression> {
    var environment = startEnvironment

    val resolvedExpressions = expressions.map {
        when (it) {
            is Expression.Return -> resolveReturn(it, environment)
            is Expression.If -> resolveIf(it, environment)
            is Expression.Let -> {
                val resolved = resolveLet(it, environment)
                environment = environment.add(resolved.name.identifier, resolved.name.type)
                resolved
            }

            else -> resolveExpression(it, environment)
        }
    }

    return resolvedExpressions
}

fun getReturnType(expressions: List<Expression>): Type? {
    val types = expressions.mapNotNull { expression ->
        when (expression) {
            is Expression.Return ->
                expression.expression?.let { readType(it) } ?: Type.Concrete("void")

            is Expression.If ->
                getReturnType(expression.thenBranch) ?: expression.elseBranch?.let { getReturnType(it) }

            else -> null
        }
    }

    // TODO find lower bound for the return type

    // Prefer concrete types
    return types.firstOrNull { it != any } ?: types.firstOrNull()
}

fun resolveAnnotation(expression: Expression, environment: Environment): Expression {
    if (expression is Expression.Call && isTypeAnnotation(expression)) {
        return Expression.Call(expression.function, expression.arguments.map { resolveAnnotation(it, environment) })
    }

    return resolveExpression(expression, environment)
}

fun isTypeAnnotation(expression: Expression): Boolean {
    return (expression is Expression.Call
        && expression.function is Expression.Variable
        && expression.function.name.identifier == "Parameters")
}

fun resolveFunction(function: Expression.Function, environment: Environment): Expression.Function {
    val resolvedAnnotations = function.annotations.map { resolveAnnotation(it, environment) }

    val typeAnnotation = resolvedAnnotations.firstOrNull { isTypeAnnotation(it) }
    val parameters = if (typeAnnotation != null) {
        val types = (typeAnnotation as Expression.Call).arguments.map { readType(it) }
        function.parameters.zip(types, { left, right -> Name(left.identifier, right) })
    } else {
        function.parameters
    }

    // TODO recursion
    // val returnType = any
    // val functionType = Type.Arrow(returnType, parameterPairs.map { it.second })
    // environment[function.name.identifier] = functionType
    val localEnvironment = environment.addMany(parameters.map { Pair(it.identifier, it.type) })

    val resolvedBody = resolveBlock(function.body, localEnvironment)
    val returnType = getReturnType(resolvedBody) ?: Type.Concrete("void")
    val functionType = Type.Arrow(returnType, parameters.map { it.type })

    // TODO infer parameter types
    val resolvedName = Name(function.name.identifier, functionType)
    return Expression.Function(resolvedName, parameters, resolvedBody, resolvedAnnotations)
}