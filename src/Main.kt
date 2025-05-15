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

fun JarOutputStream.putFile(file: String, bytes: ByteArray) {
    val entry = JarEntry(file + ".class")
    this.putNextEntry(entry)
    this.write(bytes)
    this.closeEntry()
}

fun generateAndWriteJar(out: String, file: String, text: String) {
    val generated = generate(file, text)

    val manifest = Manifest()
    manifest.mainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    // only needs class name, no ".class"
    manifest.mainAttributes.put(Attributes.Name.MAIN_CLASS, file)

    val writePath = out + "/" + file + ".jar"
    val jarOutputStream = JarOutputStream(FileOutputStream(writePath), manifest)
    jarOutputStream.putFile(file, generated)

    // add imported classes to archive
    for (cached in classFileCache) {
        val key = cached.key
        if (!key.startsWith("java/") && key != "string" && key != "Any") {
            val bytes = findClassFileBytes(key)
            jarOutputStream.putFile(key, bytes)
            println("Added imported " + key + " to jar")
        }
    }

    jarOutputStream.close()
    println("Produced " + writePath)
}

fun main(args: Array<String>) {
    val command = args.firstOrNull()

    if (command == "compile") {
        val file = args[1]
        val path = Path.of(file)
        val text = Files.readString(path)
        generateAndWriteClassFile(".", file, text)
    } else if (command == "bundle") {
        val file = args[1]
        val path = Path.of(file)
        val text = Files.readString(path)
        generateAndWriteJar(".", file, text)
    } else {
        println("Unknown command " + command)
    }
}