sealed class Token {
    data class StringToken(val string: String) : Token()
    data class IntToken(val number: Int) : Token()
    data class LongToken(val number: Long) : Token()
    data class FloatToken(val number: Float) : Token()
    data class DoubleToken(val number: Double) : Token()
    data class IdentifierToken(val string: String) : Token()
    data class FixedToken(val string: String) : Token()
    data class WhitespaceToken(val string: String) : Token()
    data class CommentToken(val string: String) : Token()
    data class TemplateStringBeginToken(val string: String) : Token()
    data class TemplateStringPartToken(val string: String) : Token()
    data class TemplateStringEndToken(val string: String) : Token()
}


fun isSignificant(t: Token): Boolean {
    return !(t is Token.WhitespaceToken || t is Token.CommentToken)
}

val escapeSeq =
    map({ toEscapeSeq(it.toString() ) }, second(satisfyChar { it == '\\' }, satisfyChar { "nrt\"\\{".contains(it) }))

val stringChar =
    map({ it.joinToString("") }, many(or(escapeSeq,takeWhile1 { it != '\"' })))

val templateStringChar =
    map({ it.joinToString("") }, many(or(escapeSeq,takeWhile1 { it != '\"' && it != '{' })))

fun toEscapeSeq(seq: String): String {
    return when (seq) {
        "\\" -> "\\"
        "\"" -> "\""
        "{" -> "{"
        "n" -> "\n"
        "r" -> "\r"
        "t" -> "\t"
        else -> error("Cannot escape " + seq)
    }
}

val stringToken: Parser<String, Token> =
    map3(
        { _, s, _ -> Token.StringToken(s) },
        satisfyChar { it == '\"' },
        stringChar,
        satisfyChar { it == '\"' })

val expressionPart =
    second(satisfyChar { it == '{' }, manyWithEnd(defer { lexeme }, satisfyChar { it == '}' }))

val templateStringBegin =
    map2({ _, _ -> Token.TemplateStringBeginToken("") }, satisfyChar { it == '$' }, satisfyChar { it == '\"' })

val templateStringEnd =
    replace(Token.TemplateStringEndToken(""), satisfyChar { it == '\"' })

val templateStringPart: Parser<String, Token> =
    map({ Token.TemplateStringPartToken(it) }, templateStringChar)

val templateString: Parser<String, List<Token>> =
    map4(
        { b, s, m, e -> listOf(b, s) + m.flatten() + listOf(e) },
        templateStringBegin,
        templateStringPart,
        many(map2({ e, s -> e + listOf(s) }, expressionPart, templateStringPart)),
        templateStringEnd
    )

val digits = takeWhile1 { it.isDigit() }

val digitsWithOptionalSign = map2(
    { sign, digs -> (sign?.toString().orEmpty()) + digs },
    optional(satisfyChar { it == '+' || it == '-' }),
    digits
)

val exponentPart = map2(
    { e, digs -> e + digs },
    satisfyChar { it.equals('e', true) },
    digitsWithOptionalSign
)

val dotPart = map2({ d, digs -> d + digs }, satisfyChar { it == '.' }, digits)

val specifier = satisfyChar { it == 'f' || it == 'L' }

val anyNumber: Parser<String, Token> = map4({ digs, dot, expo, spec ->
    val combined = digs + dot.orEmpty() + expo.orEmpty()
    if (spec == 'f') {
        Token.FloatToken(combined.toFloat())
    } else if (spec == 'L') {
        Token.LongToken(combined.toLong())
    } else if (dot == null) {
        Token.IntToken(combined.toInt())
    } else {
        Token.DoubleToken(combined.toDouble())
    }
}, digitsWithOptionalSign, optional(dotPart), optional(exponentPart), optional(specifier))

val dotsAndParens = listOf("@", ".", ",", ";", "(", ")", "[", "]", "{", "}")

val operators = listOf("==", "<=", ">=", "!=", "&&", "||", "!", "^", "?", ":", "+", "-", "*", "/", "%", "<", ">", "=")

val fixed: Parser<String, Token> =
    map({ Token.FixedToken(it) }, choice((dotsAndParens + operators).map { takeString(it) }))

val identifierOrFixed: Parser<String, Token> =
    map2(
        { first, other -> identifierOrFixedToken(first + other) },
        satisfyChar { it.isLetter() },
        takeWhile { it.isLetterOrDigit() })

val keywords = listOf("return", "let", "fn", "true", "false", "import", "if", "else")

val comment: Parser<String, Token> =
    map2({ _, c -> Token.CommentToken(c) }, takeString("//"), takeWhile { it != '\n' })

val whitespace: Parser<String, Token> =
    map({ a -> Token.WhitespaceToken(a) }, takeWhile1 { it.isWhitespace() })

fun identifierOrFixedToken(s: String): Token {
    return if (keywords.contains(s)) {
        Token.FixedToken(s)
    } else {
        Token.IdentifierToken(s)
    }
}

val lexeme = choice(listOf(comment, whitespace, stringToken, anyNumber, fixed, identifierOrFixed))

val lexemes = map({ it.flatten() }, many(or(map({ listOf(it) }, lexeme), templateString)))

fun lex(string: String): List<Token> {
    return parseStringCompletely(lexemes, string).filter { isSignificant(it) }
}
