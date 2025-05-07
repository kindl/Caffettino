import org.junit.jupiter.api.Test
import java.nio.file.Files
import java.nio.file.Path


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
            val parsed = parse(text)
            if (parsed != null) {
                generate(out, file.nameWithoutExtension, parsed)
            } else {
                error("Could not parse " + file.nameWithoutExtension)
            }
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
            val parsed = parse(text)
            if (parsed != null) {
                try {
                    generate(out, file.nameWithoutExtension, parsed)
                } catch (exception: Exception) {
                    return
                }

                error("File " + file.nameWithoutExtension + " was supposed to fail.")
            } else {
                error("Could not parse " + file.nameWithoutExtension)
            }
        }
    }

    // Useful for inspecting class files
    //@Test
    fun `explore class`() {
        // exploreClass("java/util/List")
    }

    fun exploreClass(path: String) {
        val classFile = getClassFile(path)
        val methodTypes = classFile.methods().map { it.methodName().stringValue() + " " + convertMethodType(it.methodType().stringValue()) + " " + it }
        val fieldTypes = classFile.fields().map { it.fieldName().stringValue() + " " + convertFieldType(it.fieldType().stringValue()) }
        (methodTypes + fieldTypes).forEach { println(it) }
    }
}