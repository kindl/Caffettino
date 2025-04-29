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
}


fun isSignificant(t: Token): Boolean {
    return !(t is Token.WhitespaceToken || t is Token.CommentToken)
}

// TODO escape sequences
val stringToken: Parser<String, Token> =
    map3(
        { _, s, _ -> Token.StringToken(s) },
        satisfyChar { it == '\"' },
        takeWhile { it != '\"' },
        satisfyChar { it == '\"' })

val digits = mapNullable({
    if (it == "") {
        null
    } else {
        it
    }
}, takeWhile { it.isDigit() })
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

val dotsAndParens = listOf(".", ",", ";", "(", ")", "[", "]", "{", "}")
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
    map2({ a, b -> Token.WhitespaceToken(a + b) }, satisfyChar { it.isWhitespace() }, takeWhile { it.isWhitespace() })

fun identifierOrFixedToken(s: String): Token {
    return if (keywords.contains(s)) {
        Token.FixedToken(s)
    } else {
        Token.IdentifierToken(s)
    }
}

val lexeme = choice(listOf(comment, whitespace, stringToken, anyNumber, fixed, identifierOrFixed))

fun lex(string: String): List<Token>? {
    return parseStringTilEnd(many(lexeme), string)?.filter { isSignificant(it) }
}
