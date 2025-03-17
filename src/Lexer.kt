sealed class Token {
    data class StringToken(val string: String) : Token()
    data class NumberToken(val number: Double) : Token()
    data class IdentifierToken(val string: String) : Token()
    data class FixedToken(val string: String) : Token()
    data class WhitespaceToken(val string: String) : Token()
    data class CommentToken(val string: String) : Token()
}


fun isSignificant(t: Token) : Boolean {
    return !(t is Token.WhitespaceToken || t is Token.CommentToken)
}

// TODO escape sequences
val stringToken: Parser<String, Token> =
    map3(
        { _, s, _ -> Token.StringToken(s) },
        satisfyChar { it == '\"' },
        takeWhile { it != '\"' },
        satisfyChar { it == '\"' })

val numberToken: Parser<String, Token> =
    mapNullable({ it.toDoubleOrNull()?.let { Token.NumberToken(it) } }, takeWhile { it.isDigit() })

val dotsAndParens = listOf(".", ",", ";", "(", ")", "[", "]", "{", "}")
val operators = listOf("==", "<=", ">=", "!=", "&&", "||", "!", "^", "?", ":", "+", "-", "*", "/", "%", "<", ">", "=")

val fixed: Parser<String, Token> =
    map({ Token.FixedToken(it) }, choice((dotsAndParens + operators).map { takeString(it) }))

val identifierOrFixed: Parser<String, Token> =
    map2(
        { first, other -> identifierOrFixedToken(first + other) },
        satisfyChar { it.isLetter() },
        takeWhile { it.isLetterOrDigit() })

val keywords = listOf("return", "let", "fn", "true", "false", "import")

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

val lexeme = choice(listOf(comment, whitespace, stringToken, numberToken, fixed, identifierOrFixed))

fun lex(string: String): List<Token>? {
    return parseStringTilEnd(many(lexeme), string)?.filter { isSignificant(it) }
}
