import java.lang.classfile.*
import java.lang.reflect.AccessFlag


typealias Environment = Map<String, Type>


fun resolveFile(expressions: List<Expression>): List<Expression> {
    var environment = mapOf<String, Type>()

    return expressions.map { expression ->
        when (expression) {
            is Expression.Function -> {
                val resolved = resolveFunction(expression, environment)
                environment = environment + Pair(resolved.name.identifier, resolved.name.type)
                resolved
            }

            is Expression.Import -> {
                val path = expression.path.joinToString("/")
                val classFile = getClassFile(path)
                // TODO decide how to add classes to the env
                environment = environment + Pair(expression.path.last(), Type.Concrete(path))
                expression
            }

            is Expression.Let -> {
                val resolved = resolveLet(expression, environment)
                environment = environment + Pair(resolved.name.identifier, resolved.name.type)
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
    return Expression.Let(resolvedName, resolvedExpression)
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
    val resolvedArguments = call.arguments.map { resolveExpression(it, environment) }

    return when (call.function) {
        is Expression.Dot -> resolveDotCall(call.function, environment, resolvedArguments)
        is Expression.Variable -> resolveVariableCall(call.function, environment, resolvedArguments)
        else -> error("Non call " + call.function + " in call")
    }
}

fun readType(expression: Expression): Type {
    return when (expression) {
        is Expression.Lit -> readType(expression.literal)
        is Expression.Call -> (readType(expression.function) as Type.Arrow).returnType
        is Expression.Dot -> expression.name.type
        is Expression.Variable -> expression.name.type
        else -> error("Cannot read type of expression " + expression)
    }
}

fun readType(literal: Literal): Type {
    return when (literal) {
        is Literal.BooleanLiteral -> Type.Concrete("bool")
        is Literal.StringLiteral -> Type.Concrete("java/lang/String")
        is Literal.IntLiteral -> Type.Concrete("int")
        is Literal.DoubleLiteral -> Type.Concrete("double")
        is Literal.FloatLiteral -> Type.Concrete("float")
        is Literal.LongLiteral -> Type.Concrete("long")
    }
}

fun resolveVariableCall(
    variable: Expression.Variable,
    environment: Environment,
    resolvedArguments: List<Expression>
): Expression.Call {
    // TODO find overloads with resolved argument types
    val resolvedVariable = resolveVariable(variable, environment)
    val functionType = readType(resolvedVariable)
    val argumentTypes = resolvedArguments.map { readType(it) }
    if (functionType is Type.Arrow) {
        if (argumentTypes.count() != functionType.parameterTypes.count()) {
            error("Parameters did not match " + argumentTypes + " " + functionType.parameterTypes)
        }
    }

    return Expression.Call(resolvedVariable, resolvedArguments)
}

fun resolveDotCall(
    dot: Expression.Dot,
    environment: Environment,
    resolvedArguments: List<Expression>
): Expression.Call {
    val resolvedExpression = resolveExpression(dot.expression, environment)
    val leftType = readType(resolvedExpression)

    if (!isPrimitive(leftType) && resolvedArguments.count() == 1) {
        if (isComparison(dot.name.identifier)) {
            return makeComparison(dot.name.identifier, resolvedExpression, resolvedArguments.first())
        } else if (dot.name.identifier == "notEquals") {
            return makeNotEquals(resolvedExpression, resolvedArguments.first())
        }
    }

    val argumentTypes = resolvedArguments.map { readType(it) }
    val accessorType = getMethodType(leftType, dot.name.identifier, argumentTypes)
    val newName = Name(dot.name.identifier, accessorType)
    return Expression.Call(Expression.Dot(resolvedExpression, newName), resolvedArguments)
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

// rewrite a.notEquals(b) to a.equals(b).not()
fun makeNotEquals(left: Expression, right: Expression): Expression.Call {
    val equalsType = Type.Arrow(Type.Concrete("bool"), listOf(any))
    val notType = Type.Arrow(Type.Concrete("bool"), listOf())
    val equalsPart = Expression.Call(Expression.Dot(left, Name("equals", equalsType)), listOf(right))
    return Expression.Call(Expression.Dot(equalsPart, Name("not", notType)), listOf())
}

fun isPrimitive(type: Type): Boolean {
    return listOf(
        Type.Concrete("int"), Type.Concrete("long"),
        Type.Concrete("float"), Type.Concrete("double"),
        Type.Concrete("bool")
    ).contains(type)
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

fun getMethodType(type: Type, accessor: String, argumentTypes: List<Type>): Type {
    // TODO read pseudo class file for primitive types
    //  instead of specifing all those primitive calls in code
    if (isPrimitive(type)) {
        return when (accessor) {
            "plus", "minus" -> Type.Arrow(type, listOf(type))
            "equals" -> Type.Arrow(Type.Concrete("bool"), listOf(type))
            "not" -> Type.Arrow(Type.Concrete("bool"), listOf())
            "compareTo" -> Type.Arrow(Type.Concrete("int"), listOf(type))
            "lesserOrEqual" -> Type.Arrow(Type.Concrete("int"), listOf(type))
            "lesser" -> Type.Arrow(Type.Concrete("int"), listOf(type))
            "greaterOrEqual" -> Type.Arrow(Type.Concrete("int"), listOf(type))
            "greater" -> Type.Arrow(Type.Concrete("int"), listOf(type))
            else -> error("Can't find method type for primitive " + type + " " + accessor)
        }
    }

    val path = (type as Type.Concrete).name
    val classFile = getClassFile(path)
    val options = classFile.methods().filter { it.methodName().stringValue() == accessor }
    val overloads = options.map { convertMethodType(it.methodType().stringValue()) }

    // TODO improve handling of equals
    //  equals is a little bit special, because for example "Hi" == "there" will turn into
    // "Hi".equals("there") and then we can't find an overload, because the argument is string and not object
    // Solutions would be
    // * auto conversion "Hi".equals(toObject("there"))
    // * special case for equals, we might need this anyway for primitive types
    // * some form of subtyping. If we can't find a function with a string argument,
    //      turn it into object and search again. However, this could explode quickly
    if (accessor == "equals") {
        return overloads.first()
    }

    val methodType = overloads.firstOrNull { it.parameterTypes == argumentTypes }

    return methodType
        ?: error("Cannot find fitting overload for " + accessor + " with arguments " + argumentTypes + " in " + overloads)
}

fun getClassFile(path: String): ClassModel {
    // TODO find class by path, and allow non-system imports like import kotlin.io.Console
    val stream = if (path == "Any") {
        ClassLoader.getSystemResourceAsStream("java/lang/Object.class")
    } else if (path.startsWith("java/")) {
        ClassLoader.getSystemResourceAsStream(path + ".class")
    } else if (path.startsWith("kotlin/")) {
        Any::class.java.getResourceAsStream(path + ".class")
    } else {
        error("Path not searchable " + path)
    }

    val classFile = ClassFile.of().parse(stream.readAllBytes())
    return classFile
}

// TODO non-static field access
fun getAccessorType(type: Type, accessor: String): Type {
    val name = (type as Type.Concrete).name
    val path = if (name == "Any") {
        "java/lang/Object"
    } else {
        name
    }

    val classFile = getClassFile(path)
    val options = classFile.fields().filter {
        it.flags().has(AccessFlag.STATIC) && it.fieldName().stringValue() == accessor
    }

    val stringValue = options.first().fieldType().stringValue()
    return convertFieldType(stringValue)
}

fun resolveVariable(variable: Expression.Variable, environment: Environment): Expression.Variable {
    val type = environment[variable.name.identifier] ?: error("Not found " + variable.name.identifier)
    return Expression.Variable(Name(variable.name.identifier, type))
}

fun resolveBlock(expressions: List<Expression>, startEnvironment: Environment): List<Expression> {
    var environment = startEnvironment

    val resolvedExpressions = expressions.map {
        when (it) {
            is Expression.Return -> resolveReturn(it, environment)
            is Expression.If -> resolveIf(it, environment)
            is Expression.Let -> {
                val resolved = resolveLet(it, environment)
                environment = environment + Pair(resolved.name.identifier, resolved.name.type)
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

    // TODO check if the return types conform with each other
    return types.firstOrNull()
}

fun resolveFunction(function: Expression.Function, environment: Environment): Expression.Function {
    val parameters = if (function.name.identifier == "main") {
        println("Changing parameters for main function")
        listOf(Name("args", Type.Concrete("[java/lang/String]")))
    } else {
        function.parameters
    }

    // TODO recursion
    // val returnType = any
    // val functionType = Type.Arrow(returnType, parameterPairs.map { it.second })
    // environment[function.name.identifier] = functionType
    val localEnvironment = environment + parameters.map { Pair(it.identifier, it.type) }

    val resolvedBody = resolveBlock(function.body, localEnvironment)
    val returnType = getReturnType(resolvedBody) ?: Type.Concrete("void")
    val functionType = Type.Arrow(returnType, parameters.map { it.type })

    // TODO infer parameter types
    // we could go through the expression in a first step and add all obvious annotations
    // like let x = "hello" then x is string
    // and a new unification variable for everything unspecified

    val resolvedName = Name(function.name.identifier, functionType)
    return Expression.Function(resolvedName, parameters, resolvedBody)
}

fun exploreClass(path: String) {
    val classFile = getClassFile(path)
    val methodTypes = classFile.methods().map { it.methodName().stringValue() + " " + convertMethodType(it.methodType().stringValue()) }
    val fieldTypes = classFile.fields().map { it.fieldName().stringValue() + " " + convertFieldType(it.fieldType().stringValue()) }
    (methodTypes + fieldTypes).forEach { println(it) }
}