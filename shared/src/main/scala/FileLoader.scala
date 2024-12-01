import scala.io.Source

object FileLoader:
  def readFileLines(fileName: String): List[String] =
    Source.fromResource(fileName, getClass.getClassLoader).getLines.toList

  def readFile(fileName: String): String =
    Source.fromResource(fileName, getClass.getClassLoader).mkString
