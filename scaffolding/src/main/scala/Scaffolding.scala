import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.Console.*

object Scaffolding:
  @main def entrypoint(day: Int) =
    generateScaffoldingForDay(day)

  def generateScaffoldingForDay(day: Int) =
    val root         = createDirectory(Paths.get("."), s"d$day")
    val scalaSources = createDirectory(root, "src", "main", "scala")
    val resources    = createDirectory(root, "src", "main", "resources")

    val readme      = createFile(root, "README.md")
    val testInputP1 = createFile(resources, "input-test-p1.txt")
    val testInputP2 = createFile(resources, "input-test-p2.txt")
    val mainInput   = createFile(resources, "input.txt")

    val mainSource = createFile(scalaSources, s"d$day.scala")
    val sourceP1   = createFile(scalaSources, s"d${day}p1.scala")
    val sourceP2   = createFile(scalaSources, s"d${day}p2.scala")

    val replacementRules: Map[String, String] = Map(
      "day" -> day.toString()
    )

    val mainSourceContent = FileLoader.readFile("MainSourceTemplate.scala")
    val p1SourceContent   = FileLoader.readFile("p1SourceTemplate.scala")
    val p2SourceContent   = FileLoader.readFile("p2SourceTemplate.scala")

    writeScaffoldingCode(processTemplate(mainSourceContent, replacementRules), mainSource)
    writeScaffoldingCode(processTemplate(p1SourceContent, replacementRules), sourceP1)
    writeScaffoldingCode(processTemplate(p2SourceContent, replacementRules), sourceP2)

    println(s"$GREEN scaffolding generation for day $day DONE!")

  def processTemplate(input: String, replacementRules: Map[String, String]): String =
    replacementRules.foldLeft(input) { case (acc, (from, to)) => acc.replaceAll(s"__${from}__", to) }

  def writeScaffoldingCode(content: String, file: Path) =
    Files.write(file, content.getBytes())

  def createDirectory(root: Path, path: String*): Path =
    println(s"$YELLOW directory at $root for path: ${path.mkString("/")}")
    val pathSegments = path.toList
    Files.createDirectories(root.resolve(Paths.get(pathSegments.head, pathSegments.tail*)))

  def createFile(root: Path, filename: String): Path =
    println(s"$YELLOW file at $root with name: $filename")
    Files.createFile(root.resolve(filename))
