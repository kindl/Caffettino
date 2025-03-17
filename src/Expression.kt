sealed class Expression {
    data class StringLiteral(val string: String) : Expression()
    data class NumberLiteral(val number: Double) : Expression()
    data class BooleanLiteral(val bool: Boolean) : Expression()
    data class Call(val function: Expression, val arguments: List<Expression>) : Expression()
    data class Variable(val name: String) : Expression()
    data class Dot(val expression: Expression, val name: String) : Expression()
    data class Function(val name: String, val parameters: List<String>, val body: List<Expression>) : Expression()
    data class Let(val name: String, val expression: Expression) : Expression()
    data class Import(val path: List<String>) : Expression()
    data class Return(val expression: Expression) : Expression()
}


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

val variable: Parser<Collection<Token>, Expression> = filterToken {
    when (it) {
        is Token.IdentifierToken -> Expression.Variable(it.string)
        else -> null
    }
}

val identifier = filterToken {
    when (it) {
        is Token.IdentifierToken -> it.string
        else -> null
    }
}

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
    map({ id -> { e: Expression -> Expression.Dot(e, id) } }, second(token("."), identifier))

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
    { id, expression -> Expression.Let(id, expression) },
    second(token("let"), identifier), second(token("="), expr)
)

val ret: Parser<Collection<Token>, Expression> = map(
    { expression -> Expression.Return(expression) },
    second(token("return"), expr)
)

val bodyExpression = choice(listOf(let, call, ret))

val function: Parser<Collection<Token>, Expression> = map3(
    { id, parameters, expressions -> Expression.Function(id, parameters, expressions) },
    second(token("fn"), identifier), parens(sepByTrailing(identifier, token(","))), braces(many(bodyExpression))
)

val import: Parser<Collection<Token>, Expression> = map(
    { identifiers -> Expression.Import(identifiers) },
    second(token("import"), sepBy1(identifier, token(".")))
)

val topExpression = choice(listOf(function, let, import))

fun parse(string: String): List<Expression>? {
    val tokens = lex(string)
    return tokens?.let { parseTilEnd(many(topExpression), it) }
}
