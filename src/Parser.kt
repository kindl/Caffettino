class Parser<Input, Output>(val run: (Input) -> Pair<Input, Output>?)


fun <S, T, E> parseTilEnd(p: Parser<S, T>, input: S): T? where S : Iterable<E> {
    val result = p.run(input)
    if (result == null) {
        return null
    } else {
        val (rest, output) = result
        val first = rest.firstOrNull()
        if (first == null) {
            return output
        } else {
            println("Could not parse til end. Next token is $first")
            return null
        }
    }
}

fun <S, T> defer(f: () -> Parser<S, T>): Parser<S, T> {
    return Parser {
        f().run(it)
    }
}

fun <S, T> many(p: Parser<S, T>): Parser<S, List<T>> {
    return option(emptyList(), many1(p))
}

fun <S, T> many1(p: Parser<S, T>): Parser<S, List<T>> {
    return map2({ e, es -> listOf(e) + es }, p, option(emptyList(), defer { many1(p) }))
}

fun <S, T, U> map(f: (T) -> U, p: Parser<S, T>): Parser<S, U> {
    return Parser {
        p.run(it)?.let { (rest, obj) ->
            Pair(rest, f(obj))
        }
    }
}

fun <S, T> replace(default: T, p: Parser<S, T>): Parser<S, T> {
    return map({ default }, p)
}

fun <S, T> option(default: T, p: Parser<S, T>): Parser<S, T> {
    return Parser {
        p.run(it) ?: Pair(it, default)
    }
}

fun <S, T> or(p1: Parser<S, T>, p2: Parser<S, T>): Parser<S, T> {
    return Parser {
        p1.run(it) ?: p2.run(it)
    }
}

fun <S, T, U, V> map2(f: (T, U) -> V, p1: Parser<S, T>, p2: Parser<S, U>): Parser<S, V> {
    return Parser {
        p1.run(it)?.let { (rest1, result1) ->
            p2.run(rest1)?.let { (rest2, result2) ->
                Pair(rest2, f(result1, result2))
            }
        }
    }
}

fun <T> satisfy(predicate: (T) -> Boolean): Parser<Collection<T>, T> {
    return Parser { input ->
        input.firstOrNull()?.let {
            if (predicate(it)) {
                Pair(input.drop(1), it)
            } else {
                null
            }
        }
    }
}

fun satisfyChar(predicate: (Char) -> Boolean): Parser<String, Char> {
    return Parser { input ->
        input.firstOrNull()?.let {
            if (predicate(it)) {
                Pair(input.drop(1), it)
            } else {
                null
            }
        }
    }
}

fun takeWhile(predicate: (Char) -> Boolean): Parser<String, String> {
    return Parser { input ->
        Pair(input.dropWhile(predicate), input.takeWhile(predicate))
    }
}
