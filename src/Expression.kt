sealed class Expression {
    data class Lit(val literal: Literal) : Expression()
    data class Call(val function: Expression, val arguments: List<Expression>) : Expression()
    data class Variable(val name: Name, val info: Info) : Expression()
    data class Dot(val expression: Expression, val name: Name) : Expression()
    data class Return(val expression: Expression?) : Expression()
    data class If(val condition: Expression, val thenBranch: List<Expression>, val elseBranch: List<Expression>?) :
        Expression()

    data class Import(val path: List<String>) : Expression()
    data class Let(val name: Name, val expression: Expression, val info: Info) : Expression()
    data class Function(
        val name: Name,
        val parameters: List<Name>,
        val body: List<Expression>,
        val annotations: List<Expression>
    ) : Expression()
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
        is Literal.StringLiteral -> "\"" + escape(l.string) + "\""
        is Literal.BooleanLiteral -> l.bool.toString()
        is Literal.DoubleLiteral -> l.number.toString()
        is Literal.FloatLiteral -> l.number.toString() + "f"
        is Literal.IntLiteral -> l.number.toString()
        is Literal.LongLiteral -> l.number.toString() + "L"
    }
}

fun escape(string: String): String {
    return string
        .replace("\n", "\\n")
        .replace("\t", "\\t")
        .replace("\r", "\\r")
        .replace("\b", "\\b")
        .replace("\"", "\\\"")
        .replace("\\", "\\\\")
        .replace("\'", "\\\'")
}

fun pretty(e: Expression): String {
    return when (e) {
        is Expression.Call -> pretty(e.function) + "(" + e.arguments.joinToString { pretty(it) } + ")"
        is Expression.Dot -> pretty(e.expression) + "." + e.name.identifier
        is Expression.Function ->
            ("fn " + e.name.identifier + "(" + e.parameters.joinToString { it.identifier } + ") {\n"
                    + e.body.joinToString("\n", transform = { "\t" + pretty(it) }) + "\n}")

        is Expression.If ->
            "if (" + pretty(e.condition) + ") {\n" + (e.thenBranch.joinToString(
                "\n",
                transform = { "\t" + pretty(it) })) + "\n} else {\n" + (e.elseBranch?.joinToString(
                "\n",
                transform = { "\t" + pretty(it) })) + "\n}"

        is Expression.Import -> "import " + e.path.joinToString(".")
        is Expression.Let -> "let " + e.name.identifier + " = " + pretty(e.expression)
        is Expression.Lit -> pretty(e.literal)
        is Expression.Return -> "return " + e.expression?.let { pretty(it) }
        is Expression.Variable -> e.name.identifier
    }
}


data class Name(val identifier: String, val type: Type)


val next: Parser<Collection<Token>, Token> = satisfy { true }

fun <T> mapNullableNext(f: (Token) -> T?): Parser<Collection<Token>, T> {
    return mapNullable(f, next)
}

fun token(s: String): Parser<Collection<Token>, String> {
    return mapNullableNext {
        when (it) {
            is Token.FixedToken ->
                if (it.string == s) {
                    s
                } else {
                    null
                }

            else -> null
        }
    }
}

val stringLiteral: Parser<Collection<Token>, Expression> = mapNullableNext {
    when (it) {
        is Token.StringToken -> Expression.Lit(Literal.StringLiteral(it.string))
        else -> null
    }
}

val numberLiteral: Parser<Collection<Token>, Expression> = mapNullableNext {
    when (it) {
        is Token.IntToken -> Expression.Lit(Literal.IntLiteral(it.number))
        is Token.LongToken -> Expression.Lit(Literal.LongLiteral(it.number))
        is Token.FloatToken -> Expression.Lit(Literal.FloatLiteral(it.number))
        is Token.DoubleToken -> Expression.Lit(Literal.DoubleLiteral(it.number))
        else -> null
    }
}

val identifier = mapNullableNext {
    when (it) {
        is Token.IdentifierToken -> it.string
        else -> null
    }
}

val name = map({ Name(it, any) }, identifier)

val variable: Parser<Collection<Token>, Expression> = map({ Expression.Variable(it, unsetInfo) }, name)

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

val expr: Parser<Collection<Token>, Expression> = defer { orExpression }

val callFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ arguments -> { e: Expression -> Expression.Call(e, arguments) } },
        parens(sepByTrailing(expr, token(","))))

val dotFn: Parser<Collection<Token>, (Expression) -> Expression> =
    map({ name -> { e: Expression -> Expression.Dot(e, name) } },
        second(token("."), name))

val templateStringLiteralPart: Parser<Collection<Token>, Expression> = mapNullableNext {
    when (it) {
        is Token.TemplateStringPartToken -> Expression.Lit(Literal.StringLiteral(it.string))
        else -> null
    }
}

// Wraps expression inside a template string with a toString call
val expressionWithToString: Parser<Collection<Token>, Expression> =
    map({ Expression.Call(Expression.Dot(it, Name("toString", any)), listOf()) }, expr)

val templateStringLiteral: Parser<Collection<Token>, Expression> =
    map3(
        { _, parts, _ -> parts.reduce { a, b -> makeConcat(a, b) } },
        satisfy { it is Token.TemplateStringBeginToken },
        many(or(templateStringLiteralPart, expressionWithToString)),
        satisfy { it is Token.TemplateStringEndToken })

val literal = choice(listOf(templateStringLiteral, booleanLiteral, stringLiteral, numberLiteral))

val primaryExpression: Parser<Collection<Token>, Expression> =
    choice(listOf(literal, variable, parens(expr)))

val composedExpression =
    map2({ e, fns -> fns.fold(e, { acc, r -> r(acc) }) }, primaryExpression, many(or(dotFn, callFn)))

fun makeOperator(operator: String, left: Expression, right: Expression): Expression {
    val function = operatorToFunction(operator)
    return Expression.Call(Expression.Dot(left, Name(function, any)), listOf(right))
}

fun operatorToFunction(operator: String): String {
    return when (operator) {
        "+" -> "plus"
        "-" -> "minus"
        "*" -> "times"
        "/" -> "div"
        "%" -> "rem"
        "==" -> "equals"
        "!=" -> "notEqual"
        ">=" -> "greaterOrEqual"
        "<=" -> "lesserOrEqual"
        "<" -> "lesser"
        ">" -> "greater"
        "&&" -> "and"
        "||" -> "or"
        else -> error("Unknown operator " + operator)
    }
}

fun unaryOperatorToFunction(operator: String): String {
    return when (operator) {
        "+" -> "unaryPlus"
        "-" -> "unaryMinus"
        "!" -> "not"
        else -> error("Unknown operator " + operator)
    }
}

fun makeUnary(operator: String, expression: Expression): Expression {
    val function = unaryOperatorToFunction(operator)
    return Expression.Call(Expression.Dot(expression, Name(function, any)), listOf())
}

// TODO Test precedences
// unary ! - +
// * / %
// + -
// < <= > >=
// == !=
// &&
// ||
// ?: Right to left

fun <S, T, U> leftAssoc(f: (T, U, U) -> U, p: Parser<S, U>, op: Parser<S, T>): Parser<S, U> {
    return map2(
        { start, pairs -> pairs.fold(start, { acc, (left, right) -> f(left, acc, right) }) },
        p,
        many(map2({ a, b -> Pair(a, b) }, op, p))
    )
}

val unaryExpression: Parser<Collection<Token>, Expression> =
    defer { choice(listOf(unaryMinusExpression, unaryPlusExpression, notExpression, composedExpression)) }

val unaryMinusExpression = map2(::makeUnary, token("-"), unaryExpression)

val unaryPlusExpression = map2(::makeUnary, token("+"), unaryExpression)

val notExpression = map2(::makeUnary, token("!"), unaryExpression)

val mulExpression =
    leftAssoc(::makeOperator, unaryExpression, choice(listOf(token("*"), token("/"), token("%"))))

val plusExpression =
    leftAssoc(::makeOperator, mulExpression, choice(listOf(token("+"), token("-"))))

val compareExpression =
    leftAssoc(::makeOperator, plusExpression, choice(listOf(token("<="), token("<"), token(">"), token(">="))))

val equalsExpression =
    leftAssoc(::makeOperator, compareExpression, choice(listOf(token("=="), token("!="))))

val andExpression =
    leftAssoc(::makeOperator, equalsExpression, token("&&"))

val orExpression =
    leftAssoc(::makeOperator, andExpression, token("||"))


val call: Parser<Collection<Token>, Expression> = mapNullable({
    when (it) {
        is Expression.Call -> it
        else -> null
    }
}, composedExpression)

val let: Parser<Collection<Token>, Expression> = map2(
    { name, expression -> Expression.Let(name, expression, unsetInfo) },
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

val annotation: Parser<Collection<Token>, Expression> =
    map2({ v, c ->
        if (c == null) {
            v
        } else {
            c(v)
        }
    }, second(token("@"), variable), optional(callFn))

val function: Parser<Collection<Token>, Expression> = map4(
    { annotations, name, parameters, expressions -> Expression.Function(name, parameters, expressions, annotations) },
    many(annotation),
    second(token("fn"), name),
    parens(sepByTrailing(name, token(","))),
    block
)

val import: Parser<Collection<Token>, Expression> = map(
    { identifiers -> Expression.Import(identifiers) },
    second(token("import"), sepBy1(identifier, token(".")))
)

val topExpression = choice(listOf(import, let, function))

fun parse(string: String): List<Expression>? {
    val tokens = lex(string)
    return tokens?.let { parseCompletely(many(topExpression), it) }
}