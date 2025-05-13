sealed class Type {
    data class Concrete(val name: String) : Type()
    data class Arrow(val returnType: Type, val parameterTypes: List<Type>) : Type()
}

val any = Type.Concrete("Any")
val arrayOfAny = Type.Concrete("[Any]")
val emptyArrayType = Type.Concrete("EmptyArray")
val stringType = Type.Concrete("string")

fun isArrayType(type: Type): Boolean {
    return type is Type.Concrete && type.name.startsWith("[")
}

fun isFunctionInterface(parameterType: Type): Boolean {
    return parameterType == Type.Concrete("java/util/function/Consumer")
        || parameterType == Type.Concrete("java/util/function/Function")
        || parameterType == Type.Concrete("java/util/function/Predicate")
}

fun makeConcrete(path: String): Type.Concrete {
    return when (path) {
        "java/lang/Object" -> any
        "java/lang/String" -> stringType
        else -> Type.Concrete(path)
    }
}

val parseI = map({ Type.Concrete("int") }, takeString("I"))
val parseJ = map({ Type.Concrete("long") }, takeString("J"))
val parseV = map({ Type.Concrete("void") }, takeString("V"))
val parseZ = map({ Type.Concrete("bool") }, takeString("Z"))
val parseB = map({ Type.Concrete("byte") }, takeString("B"))
val parseC = map({ Type.Concrete("char") }, takeString("C"))
val parseF = map({ Type.Concrete("float") }, takeString("F"))
val parseD = map({ Type.Concrete("double") }, takeString("D"))
val parseL = map3(
    { _, path, _ -> makeConcrete(path) },
    takeString("L"), takeWhile { it.isLetterOrDigit() || it == '/' || it == '$' }, takeString(";")
)

val parseT = choice(listOf(parseI, parseJ, parseV, parseZ, parseB, parseC, parseF, parseD, parseL))
val parseA = map({ Type.Concrete("[" + it.name + "]") }, second(takeString("["), parseT))
var parseK = or(parseA, parseT)

val parseM = map2(
    { parameterTypes, returnType -> Type.Arrow(returnType, parameterTypes) },
    second(takeString("("), many(parseK)), second(takeString(")"), parseK)
)


fun convertFieldType(stringValue: String): Type {
    return parseStringCompletely(parseK, stringValue) ?: error("Cannot parse field type " + stringValue)
}

fun convertMethodType(stringValue: String): Type.Arrow {
    return parseStringCompletely(parseM, stringValue) ?: error("Cannot parse method type " + stringValue)
}
