class Parser<Input, Output>(val run: (Input) -> Pair<Input, Output>?)


fun <S, T, E> parseTilEnd(p: Parser<S, T>, input: S): T? where S : Iterable<E> {
    val result = p.run(input)
    if (result == null) {
        return null
    } else {
        val (rest, output) = result
        val first = rest.firstOrNull()
        val second = rest.elementAtOrNull(1)
        if (first == null) {
            return output
        } else {
            println("Could not parse til end. Next token is $first $second")
            return null
        }
    }
}

fun <T> parseStringTilEnd(p: Parser<String, T>, input: String): T? {
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

fun <S, T, U> sepBy(p: Parser<S, T>, sep: Parser<S, U>): Parser<S, List<T>> {
    return option(emptyList(), sepBy1(p, sep))
}

fun <S, T, U> sepBy1(p: Parser<S, T>, sep: Parser<S, U>): Parser<S, List<T>> {
    return map2({ e, es -> listOf(e) + es }, p, many(map2({ _, r -> r }, sep, p)))
}

fun <S, T, U> sepByTrailing(p: Parser<S, T>, sep: Parser<S, U>): Parser<S, List<T>> {
    return map2({ e, _ -> e }, sepBy(p, sep), optional(sep))
}

fun <S, T, U> mapNullable(f: (T) -> U?, p: Parser<S, T>): Parser<S, U> {
    return Parser {
        p.run(it)?.let { (rest, obj) ->
            f(obj)?.let { result -> Pair(rest, result) }
        }
    }
}

fun <S, T, U> map(f: (T) -> U, p: Parser<S, T>): Parser<S, U> {
    return Parser {
        p.run(it)?.let { (rest, obj) ->
            Pair(rest, f(obj))
        }
    }
}

fun <S, T, U> second(p1: Parser<S, T>, p2: Parser<S, U>): Parser<S, U> {
    return map2({ _, e -> e }, p1, p2)
}

fun <S, T, U> replace(default: U, p: Parser<S, T>): Parser<S, U> {
    return map({ default }, p)
}

fun <S, T> option(default: T, p: Parser<S, T>): Parser<S, T> {
    return Parser {
        p.run(it) ?: Pair(it, default)
    }
}

fun <S, T> optional(p: Parser<S, T>): Parser<S, T?> {
    return Parser {
        p.run(it) ?: Pair(it, null)
    }
}

fun <S, T> or(p1: Parser<S, T>, p2: Parser<S, T>): Parser<S, T> {
    return Parser {
        p1.run(it) ?: p2.run(it)
    }
}

fun <S, T> choice(ps: Iterable<Parser<S, T>>): Parser<S, T> {
    return ps.reduce { acc, next -> or(acc, next) }
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

fun <S, T, U, V, W> map3(f: (T, U, V) -> W, p1: Parser<S, T>, p2: Parser<S, U>, p3: Parser<S, V>): Parser<S, W> {
    return Parser {
        p1.run(it)?.let { (rest1, result1) ->
            p2.run(rest1)?.let { (rest2, result2) ->
                p3.run(rest2)?.let { (rest3, result3) ->
                    Pair(rest3, f(result1, result2, result3))
                }
            }
        }
    }
}

fun <S, T, U, V, W, X> map4(
    f: (T, U, V, W) -> X,
    p1: Parser<S, T>,
    p2: Parser<S, U>,
    p3: Parser<S, V>,
    p4: Parser<S, W>
): Parser<S, X> {
    return Parser {
        p1.run(it)?.let { (rest1, result1) ->
            p2.run(rest1)?.let { (rest2, result2) ->
                p3.run(rest2)?.let { (rest3, result3) ->
                    p4.run(rest3)?.let { (rest4, result4) ->
                        Pair(rest4, f(result1, result2, result3, result4))
                    }
                }
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

fun takeString(s: String): Parser<String, String> {
    return Parser { input ->
        if (input.startsWith(s)) {
            Pair(input.drop(s.length), s)
        } else {
            null
        }
    }
}
