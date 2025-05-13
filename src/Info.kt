sealed class Info {
    data class Local(val index: Int) : Info()
    data class Outside(val unit: Unit) : Info()
    data class Static(val ownerType: Type) : Info()
}

val unsetInfo = Info.Outside(Unit)

val baseStatics = listOf(Name("emptyArray", any))

fun annotateFile(file: String, expressions: List<Expression>): List<Expression> {
    val current = Type.Concrete(file)
    val statics = baseStatics + gatherStatics(expressions)
    errorOnDuplicates(statics)

    return expressions.map { expression ->
        when (expression) {
            is Expression.Function -> annotateLocalsFunction(expression, statics, current)
            is Expression.Let -> annotateLocals(expression, listOf(), statics, current)
            is Expression.Import -> expression
            else -> error("Unexpected expression at top level " + expression)
        }
    }
}

fun <T> errorOnDuplicates(list: List<T>) {
    for (group in list.groupBy({ it })) {
        if (group.value.count() > 1) {
            error("The variable " + group.key + " was declared multiple times ")
        }
    }
}

fun annotateLocalsFunction(function: Expression.Function, statics: List<Name>, current: Type): Expression {
    val parameters = if (function.name.identifier == "main") {
        println("Changing parameters for main function")
        listOf(Name("args", Type.Concrete("[java/lang/String]")))
    } else {
        function.parameters
    }

    val locals = function.parameters + gatherLocals(function.body)
    errorOnDuplicates(locals + statics)
    val annotated = function.body.map { annotateLocals(it, locals, statics, current) }
    return Expression.Function(function.name, parameters, annotated, function.annotations)
}

fun annotateLocals(expression: Expression, locals: List<Name>, statics: List<Name>, current: Type): Expression {
    return when (expression) {
        is Expression.Variable -> {
            val index = locals.indexOfFirst { it.identifier == expression.name.identifier }
            if (index > -1) {
                Expression.Variable(expression.name, Info.Local(index))
            } else if (statics.contains(expression.name)) {
                Expression.Variable(expression.name, Info.Static(current))
            } else { expression }
        }
        is Expression.Let -> {
            val index = locals.indexOfFirst { it.identifier == expression.name.identifier }
            val annotated = annotateLocals(expression.expression, locals, statics, current)
            if (index > -1) {
                Expression.Let(expression.name, annotated, Info.Local(index))
            } else if (statics.contains(expression.name)) {
                Expression.Let(expression.name, annotated, Info.Static(current))
            } else {
                Expression.Let(expression.name, annotated, Info.Static(current))
            }
        }

        is Expression.Call -> Expression.Call(
            annotateLocals(expression.function, locals, statics, current),
            expression.arguments.map { annotateLocals(it, locals, statics, current) })
        is Expression.Dot -> Expression.Dot(annotateLocals(expression.expression, locals, statics, current), expression.name)
        is Expression.If -> {
            val cond = annotateLocals(expression.condition, locals, statics, current)
            val thenBranch = expression.thenBranch.map { annotateLocals(it, locals, statics, current) }
            val elseBranch = expression.elseBranch?.map { annotateLocals(it, locals, statics, current) }
            Expression.If(cond, thenBranch, elseBranch)
        }
        is Expression.Return ->
            if (expression.expression != null) {
                Expression.Return(annotateLocals(expression.expression, locals, statics, current))
            } else {
                expression
            }
        else -> expression
    }
}

fun gatherStatics(expressions: List<Expression>): List<Name> {
    return expressions.mapNotNull { expression ->
        when (expression) {
            is Expression.Let -> expression.name
            is Expression.Function -> expression.name
            else -> null
        }
    }
}

// TODO avoid adding locals several times
fun gatherLocals(expressions: List<Expression>): List<Name> {
    return expressions.flatMap { expression ->
        when (expression) {
            is Expression.If -> {
                val thenLocals = gatherLocals(expression.thenBranch)
                val elseLocals = if (expression.elseBranch != null) {
                    gatherLocals(expression.elseBranch)
                } else {
                    listOf()
                }

                thenLocals + elseLocals
            }

            is Expression.Let -> listOf(expression.name)
            else -> listOf()
        }
    }
}