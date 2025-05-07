import java.nio.file.Files
import java.nio.file.Paths

fun generate(out: String, file: String, expressions: List<Expression>) {
    println("Generating " + file)
    val annotated = annotateFile(file, expressions)
    val resolved = resolveFile(annotated)
    val generated = generateClassFile(file, resolved)

    val outPath = Paths.get(out)
    Files.createDirectories(outPath)
    val writePath = Paths.get(out + "/" + file + ".class")
    Files.write(writePath, generated)
}

fun main(args: Array<String>) {
}