import java.lang.classfile.*
import java.lang.reflect.AccessFlag


typealias Environment = Map<String, Type>


fun resolveFile(expressions: List<Expression>): List<Expression> {
    var environment = mapOf<String, Type>()

    return expressions.map {
        when (it) {
            is Expression.Function -> {
                val resolved = resolveFunction(it, environment)
                environment = environment + Pair(resolved.name.identifier, resolved.name.type)
                resolved
            }

            is Expression.Import -> {
                val path = it.path.joinToString("/")
                val classFile = getClassFile(path)
                // TODO decide how to add classes to the env
                environment = environment + Pair(it.path.last(), Type.Concrete(path))
                it
            }

            is Expression.Let -> {
                val resolved = resolveLet(it, environment)
                environment = environment + Pair(resolved.name.identifier, resolved.name.type)
                resolved
            }

            else -> TODO()
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
        is Expression.BooleanLiteral -> expression
        is Expression.NumberLiteral -> expression
        is Expression.StringLiteral -> expression
        is Expression.Call -> resolveCall(expression, environment)
        is Expression.Dot -> resolveDot(expression, environment)
        is Expression.Variable -> resolveVariable(expression, environment)
        is Expression.If -> TODO()
        is Expression.Let -> TODO()
        is Expression.Return -> TODO()
        is Expression.Function -> TODO()
        is Expression.Import -> TODO()
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
    val elseBranch = resolveBlock(ifExpression.elseBranch ?: TODO(), environment)

    return Expression.If(
        condition,
        thenBranch,
        elseBranch
    )
}

fun resolveCall(call: Expression.Call, environment: Environment): Expression.Call {
    val resolvedArguments = call.arguments.map { resolveExpression(it, environment) }
    val argumentTypes = resolvedArguments.map { readType(it) }
    val resolvedExpression = when (call.function) {
        is Expression.Dot -> resolveDotCall(call.function, environment, argumentTypes)
        // TODO add a variant of resolveVariable that takes argumentTypes as parameter
        // to find the correct overload
        is Expression.Variable -> resolveVariable(call.function, environment)
        else -> resolveExpression(call.function, environment)
    }

    return Expression.Call(resolvedExpression, resolvedArguments)
}

fun readType(expression: Expression): Type {
    return when (expression) {
        is Expression.BooleanLiteral -> Type.Concrete("boolean")
        is Expression.Call -> (readType(expression.function) as Type.Arrow).returnType
        is Expression.Dot -> expression.name.type
        is Expression.StringLiteral -> Type.Concrete("java/lang/String")
        is Expression.Variable -> expression.name.type
        else -> TODO()
    }
}

fun resolveDotCall(dot: Expression.Dot, environment: Environment, argumentTypes: List<Type>): Expression {
    val resolvedExpression = resolveExpression(dot.expression, environment)
    val leftType = readType(resolvedExpression)
    val accessorType = getMethodType(leftType, dot.name.identifier, argumentTypes)
    val newName = Name(dot.name.identifier, accessorType)
    return Expression.Dot(resolvedExpression, newName)
}

fun resolveDot(dot: Expression.Dot, environment: Environment): Expression {
    val resolvedExpression = resolveExpression(dot.expression, environment)
    val leftType = readType(resolvedExpression)
    val accessorType = getAccessorType(leftType, dot.name.identifier)
    val newName = Name(dot.name.identifier, accessorType)
    return Expression.Dot(resolvedExpression, newName)
}

fun getMethodType(type: Type, accessor: String, argumentTypes: List<Type>): Type {
    val path = (type as Type.Concrete).name
    val classFile = getClassFile(path)
    val options = classFile.methods().filter { it.methodName().stringValue() == accessor }
    val overloads = options.map { convertMethodType(it.methodType().stringValue()) }
    val methodType = overloads.firstOrNull { it.parameterTypes == argumentTypes }

    return methodType ?: throw Exception("Cannot find fitting overload for " + argumentTypes + " in " + overloads)
}

fun getClassFile(path: String): ClassModel {
    println("Looking for " + path)
    // TODO find class by path, and allow non-system imports like import kotlin.io.Console
    val stream = if (path.startsWith("java/")) {
        ClassLoader.getSystemResourceAsStream(path + ".class")
    } else if (path.startsWith("kotlin/")) {
        Any::class.java.getResourceAsStream(path + ".class")
    } else {
        throw Exception("Path not searchable " + path)
    }

    val classFile = ClassFile.of().parse(stream.readAllBytes())
    return classFile
}

// TODO non-static field access
fun getAccessorType(type: Type, accessor: String): Type {
    val path = (type as Type.Concrete).name
    val classFile = getClassFile(path)
    val options =
        classFile.fields().filter { it.flags().has(AccessFlag.STATIC) && it.fieldName().stringValue() == accessor }
    val stringValue = options.first().fieldType().stringValue()
    return convertFieldType(stringValue)
}

fun resolveVariable(variable: Expression.Variable, environment: Environment): Expression.Variable {
    val type = environment[variable.name.identifier] ?: throw Exception("Not found " + variable.name.identifier)
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
    val types = expressions.mapNotNull {
        when (it) {
            is Expression.Return -> if (it.expression != null) {
                readType(it.expression)
            } else {
                Type.Concrete("void")
            }

            is Expression.If -> getReturnType(it.thenBranch) ?: (if (it.elseBranch != null) {
                getReturnType(it.elseBranch)
            } else {
                null
            })

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