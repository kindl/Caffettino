import java.io.FileOutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.Attributes
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import java.util.jar.Manifest


fun generate(file: String, text: String): ByteArray {
    println("Generating " + file)
    val parsed = parseString(text)
    val annotated = annotateFile(file, parsed)
    val resolved = resolveFile(annotated)
    val generated = generateClassFile(file, resolved)
    return generated
}

fun generateAndWriteClassFile(out: String, file: String, text: String) {
    val generated = generate(file, text)
    val outPath = Path.of(out)
    Files.createDirectories(outPath)
    val writePath = Path.of(out + "/" + file + ".class")
    Files.write(writePath, generated)
}

fun generateAndWriteJar(out: String, file: String, text: String) {
    val generated = generate(file, text)

    val manifest = Manifest()
    manifest.mainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    // only needs class name, no ".class"
    manifest.mainAttributes.put(Attributes.Name.MAIN_CLASS, file)

    val writePath = out + "/" + file + ".jar"
    val jarOutputStream = JarOutputStream(FileOutputStream(writePath), manifest)
    val entry = JarEntry(file + ".class")
    jarOutputStream.putNextEntry(entry)
    jarOutputStream.write(generated)
    jarOutputStream.closeEntry()

    // TODO add imported classes to archive

    jarOutputStream.close()
}

fun main(args: Array<String>) {
    val command = args.firstOrNull()

    if (command == "compile") {
        val file = args[1]
        val path = Path.of(file)
        val text = Files.readString(path)
        generateAndWriteClassFile(".", file, text)
    } else if (command == "build") {
        val file = args[1]
        val path = Path.of(file)
        val text = Files.readString(path)
        generateAndWriteJar(".", file, text)
    } else {
        println("Unknown command " + command)
    }
}