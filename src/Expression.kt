sealed class Expression {
    data class Lit(val literal: Literal) : Expression()
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

sealed class Literal {
    data class StringLiteral(val string: String) : Literal()
    data class DoubleLiteral(val number: Double) : Literal()
    data class IntLiteral(val number: Int) : Literal()
    data class FloatLiteral(val number: Float) : Literal()
    data class LongLiteral(val number: Long) : Literal()
    data class BooleanLiteral(val bool: Boolean) : Literal()
}

fun pretty(l: Literal): String {
    return when (l) {
        // TODO escape
        is Literal.StringLiteral -> "\"" + l.string + "\""
        is Literal.BooleanLiteral -> l.bool.toString()
        is Literal.DoubleLiteral -> l.number.toString()
        is Literal.FloatLiteral -> l.number.toString()
        is Literal.IntLiteral -> l.number.toString()
        is Literal.LongLiteral -> l.number.toString()
    }
}

fun pretty(e: Expression): String {
    return when (e) {
        is Expression.Call -> pretty(e.function) + "(" + e.arguments.joinToString { pretty(it) } + ")"
        is Expression.Dot -> pretty(e.expression) + "." + e.name.identifier
        is Expression.Function ->
            ("fn " + e.name.identifier + "(" + e.parameters.joinToString { it.identifier } + ") {\n"
                    + e.body.joinToString("\n", transform = { "\t" + pretty(it) }) + "\n}")

        // TODO
        is Expression.If -> "if (" + pretty(e.condition) + ") {\n"
        is Expression.Import -> "import " + e.path.joinToString(".")
        is Expression.Let -> "let " + e.name.identifier + " = " + pretty(e.expression)
        is Expression.Lit -> pretty(e.literal)
        is Expression.Return -> "return " + e.expression?.let { pretty(it) }
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
        is Token.StringToken -> Expression.Lit(Literal.StringLiteral(it.string))
        else -> null
    }
}

val numberLiteral: Parser<Collection<Token>, Expression> = filterToken {
    when (it) {
        is Token.IntToken -> Expression.Lit(Literal.IntLiteral(it.number))
        is Token.LongToken -> Expression.Lit(Literal.LongLiteral(it.number))
        is Token.FloatToken -> Expression.Lit(Literal.FloatLiteral(it.number))
        is Token.DoubleToken -> Expression.Lit(Literal.DoubleLiteral(it.number))
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
        replace(Expression.Lit(Literal.BooleanLiteral(true)), token("true")),
        replace(Expression.Lit(Literal.BooleanLiteral(false)), token("false"))
    )

fun <T> parens(p: Parser<Collection<Token>, T>): Parser<Collection<Token>, T> {
    return map3({ _, r, _ -> r }, token("("), p, token(")"))
}

fun <T> braces(p: Parser<Collection<Token>, T>): Parser<Collection<Token>, T> {
    return map3({ _, r, _ -> r }, token("{"), p, token("}"))
}

// val expr = defer { composedExpression }
val expr: Parser<Collection<Token>, Expression> = defer { orExpression }

val callFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ arguments: List<Expression> -> { e: Expression -> Expression.Call(e, arguments) } }, parens(many(expr)))

val dotFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ name -> { e: Expression -> Expression.Dot(e, name) } }, second(token("."), name))

val primaryExpression: Parser<Collection<Token>, Expression> =
    choice(listOf(booleanLiteral, stringLiteral, numberLiteral, variable, parens(expr)))

val composedExpression =
    map2({ e, fns -> fns.fold(e, { acc, r -> r(acc) }) }, primaryExpression, many(or(dotFn, callFn)))

fun makeOperator(operator: String, left: Expression, right: Expression): Expression {
    val name = operatorToFunction(operator)
    return Expression.Call(Expression.Dot(left, Name(name, any)), listOf(right))

    // TODO push this rewrite into resolve, so that int comparisons do not get rewritten
    // for example a < b should stay as lesser and not become a.compareTo(b) < 0
}

fun operatorToFunction(operator: String): String {
    return when (operator) {
        "+" -> "plus"
        "-" -> "minus"
        "*" -> "times"
        "/" -> "div"
        "%" -> "rem"
        "==" -> "equals"
        "!=" -> "notEquals"
        ">=" -> "greaterOrEqual"
        "<=" -> "lesserOrEqual"
        "<" -> "lesser"
        ">" -> "greater"
        "&&" -> "and"
        "||" -> "or"
        else -> error("Unknown operator " + operator)
    }
}

fun makeUnary(operator: String, expression: Expression): Expression {
    return Expression.Call(Expression.Dot(expression, Name(operator, any)), listOf())
}

// TODO High to lower
// composedExpression
// unary ! - Right to left
// * / %
// + -
// < <= > >=
// == !=
// &&
// ||
// ?: Right to left

fun <S, T, U> leftAssoc(op: Parser<S, T>, p: Parser<S, U>, f: (T, U, U) -> U): Parser<S, U> {
    return map2(
        { start, pairs -> pairs.fold(start, { acc, (left, right) -> f(left, acc, right) }) },
        p,
        many(map2({ a, b -> Pair(a, b) }, op, p))
    )
}

val unaryExpression: Parser<Collection<Token>, Expression> =
    defer { choice(listOf(unaryMinusExpression, unaryPlusExpression, notExpression, composedExpression)) }

val unaryMinusExpression = map({ makeUnary("unaryMinus", it) }, second(token("-"), unaryExpression))

val unaryPlusExpression = map({ makeUnary("unaryPlus", it) }, second(token("-"), unaryExpression))

val notExpression = map({ makeUnary("not", it) }, second(token("!"), unaryExpression))

val mulExpression =
    leftAssoc(choice(listOf(token("*"), token("/"), token("%"))), unaryExpression, ::makeOperator)

val plusExpression =
    leftAssoc(choice(listOf(token("+"), token("-"))), mulExpression, ::makeOperator)

val compareExpression =
    leftAssoc(choice(listOf(token("<="), token("<"), token(">"), token(">="))), plusExpression, ::makeOperator)

val equalsExpression =
    leftAssoc(choice(listOf(token("=="), token("!="))), compareExpression, ::makeOperator)

val andExpression =
    leftAssoc(token("&&"), equalsExpression, ::makeOperator)

val orExpression =
    leftAssoc(token("||"), andExpression, ::makeOperator)


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

val elsePart = defer { second(token("else"), or(block, map({ listOf(it) }, ifExpression))) }

val ifExpression: Parser<Collection<Token>, Expression> = map3(
    { condition, th, el -> Expression.If(condition, th, el) },
    second(token("if"), parens(expr)), block, elsePart
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
