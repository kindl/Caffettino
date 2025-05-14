import org.junit.jupiter.api.Test
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.*


class TestRun {
    val out = "out/generated"

    // TODO parameterize so that each file appears as a separate test
    @Test
    fun `test valid`() {
        val files = Files.walk(Path.of("test/valid/"))

        for (file in files) {
            if (file.extension != "ct") {
                continue
            }

            val text = Files.readString(file)
            generateAndWriteClassFile(out, file.nameWithoutExtension, text)
        }
    }

    @Test
    fun `test invalid`() {
        val files = Files.walk(Path.of("test/invalid/"))

        for (file in files) {
            if (file.extension != "ct") {
                continue
            }

            val text = Files.readString(file)
            try {
                generateAndWriteClassFile(out, file.nameWithoutExtension, text)
            } catch (exception: Exception) {
                println("Successfully errored with message " + exception.message)
                continue
            }

            error("File " + file.nameWithoutExtension + " was supposed to fail.")
        }
    }

    /*
    @Test
    fun `test jar`() {
        val text = Files.readString(Path.of("test/valid/Server.ct"))
        generateAndWriteJar(out, "Server", text)
    }

    // Useful for inspecting class files
    @Test
    fun `explore class`() {
        // exploreTypes("java/util/List")
        // exploreMethods("ForEachJava.class")
        exploreMethods("ForEachJava.class")
    }

    fun exploreMethods(path: String) {
        val bytes = Files.readAllBytes(Path.of(path))
        val classFile = ClassFile.of().parse(bytes)
        for (m in classFile.methods()) {
            println("--------" + m.methodName().stringValue())
            val elements = m.code().get().elements()

            for (e in elements) {
                println(e)
                if (e is InvokeDynamicInstruction) {
                    println("M" + e.bootstrapMethod())
                    println("A" + e.bootstrapArgs())
                    println("N" + e.name().stringValue())
                    println("T" + e.type().stringValue())
                }
            }
        }
    }

    fun exploreTypes(path: String) {
        val classFile = getClassFile(path)
        val methodTypes = classFile.methods().map { it.methodName().stringValue() + " " + convertMethodType(it.methodType().stringValue()) + " " + it }
        val fieldTypes = classFile.fields().map { it.fieldName().stringValue() + " " + convertFieldType(it.fieldType().stringValue()) }
        (methodTypes + fieldTypes).forEach { println(it) }
    }
    */
}