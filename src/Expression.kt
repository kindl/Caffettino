sealed class Expression {
    data class StringLiteral(val string: String) : Expression()
    data class NumberLiteral(val number: Double) : Expression()
    data class BooleanLiteral(val bool: Boolean) : Expression()
    data class Call(val function: Expression, val arguments: List<Expression>) : Expression()
    data class Variable(val name: Name) : Expression()
    data class Dot(val expression: Expression, val name: Name) : Expression()
    data class Return(val expression: Expression?) : Expression()
    data class If(val condition: Expression, val thenBranch: List<Expression>, val elseBranch: List<Expression>?) :
        Expression()

    data class Import(val path: List<String>) : Expression()
    data class Let(val name: Name, val expression: Expression) : Expression()
    data class Function(val name: Name, val parameters: List<Name>, val body: List<Expression>) : Expression()
}

fun pretty(e: Expression): String {
    return when (e) {
        is Expression.BooleanLiteral -> TODO()
        is Expression.Call -> pretty(e.function) + "(" + e.arguments.joinToString { pretty(it) } + ")"
        is Expression.Dot -> pretty(e.expression) + "." + e.name.identifier
        is Expression.Function ->
            ("fn " + e.name.identifier + "(" + e.parameters.joinToString { it.identifier } + ") {\n"
                    + e.body.joinToString("\n", transform = { "\t" + pretty(it) }) + "\n}")

        is Expression.If -> TODO()
        is Expression.Import -> "import " + e.path.joinToString(".")
        is Expression.Let -> "let " + e.name.identifier + " = " + pretty(e.expression)
        is Expression.NumberLiteral -> TODO()
        is Expression.Return -> "return " + e.expression?.let { pretty(it) }
        is Expression.StringLiteral -> "\"" + e.string + "\""
        is Expression.Variable -> e.name.identifier
    }
}


data class Name(val identifier: String, val type: Type)


val next: Parser<Collection<Token>, Token> = satisfy { true }

fun <T> filterToken(f: (Token) -> T?): Parser<Collection<Token>, T> {
    return mapNullable(f, next)
}

fun token(s: String): Parser<Collection<Token>, String> {
    return filterToken {
        when (it) {
            is Token.FixedToken -> if (it.string == s) {
                s
            } else {
                null
            }

            else -> null
        }
    }
}

val stringLiteral: Parser<Collection<Token>, Expression> = filterToken {
    when (it) {
        is Token.StringToken -> Expression.StringLiteral(it.string)
        else -> null
    }
}

val numberLiteral: Parser<Collection<Token>, Expression> = filterToken {
    when (it) {
        is Token.NumberToken -> Expression.NumberLiteral(it.number)
        else -> null
    }
}

val identifier = filterToken {
    when (it) {
        is Token.IdentifierToken -> it.string
        else -> null
    }
}

val name = map({ Name(it, any) }, identifier)

val variable: Parser<Collection<Token>, Expression> = map({ Expression.Variable(it) }, name)

val booleanLiteral: Parser<Collection<Token>, Expression> =
    or(
        replace(Expression.BooleanLiteral(true), token("true")),
        replace(Expression.BooleanLiteral(false), token("false"))
    )

fun <T> parens(p: Parser<Collection<Token>, T>): Parser<Collection<Token>, T> {
    return map3({ _, r, _ -> r }, token("("), p, token(")"))
}

fun <T> braces(p: Parser<Collection<Token>, T>): Parser<Collection<Token>, T> {
    return map3({ _, r, _ -> r }, token("{"), p, token("}"))
}

// TODO operators
val expr = defer { composedExpression }

val callFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ arguments: List<Expression> -> { e: Expression -> Expression.Call(e, arguments) } }, parens(many(expr)))

val dotFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ name -> { e: Expression -> Expression.Dot(e, name) } }, second(token("."), name))

val primaryExpression: Parser<Collection<Token>, Expression> =
    choice(listOf(booleanLiteral, stringLiteral, numberLiteral, variable, parens(expr)))

val composedExpression =
    map2({ e, fns -> fns.fold(e, { acc, r -> r(acc) }) }, primaryExpression, many(or(dotFn, callFn)))

val call: Parser<Collection<Token>, Expression> = mapNullable({
    when (it) {
        is Expression.Call -> it
        else -> null
    }
}, composedExpression)

val let: Parser<Collection<Token>, Expression> = map2(
    { name, expression -> Expression.Let(name, expression) },
    second(token("let"), name), second(token("="), expr)
)

val ret: Parser<Collection<Token>, Expression> = map(
    { expression -> Expression.Return(expression) },
    second(token("return"), optional(expr))
)

// The block could be adapted to allow return only at the end
val block = defer { braces(many(bodyExpression)) }

val ifExpression: Parser<Collection<Token>, Expression> = map3(
    { condition, th, el -> Expression.If(condition, th, el) },
    second(token("if"), parens(expr)), block, second(token("else"), block)
)

val bodyExpression = choice(listOf(let, call, ret, ifExpression))

val function: Parser<Collection<Token>, Expression> = map3(
    { name, parameters, expressions -> Expression.Function(name, parameters, expressions) },
    second(token("fn"), name), parens(sepByTrailing(name, token(","))), block
)

val import: Parser<Collection<Token>, Expression> = map(
    { identifiers -> Expression.Import(identifiers) },
    second(token("import"), sepBy1(identifier, token(".")))
)

val topExpression = choice(listOf(import, let, function))

fun parse(string: String): List<Expression>? {
    val tokens = lex(string)
    return tokens?.let { parseTilEnd(many(topExpression), it) }
}
