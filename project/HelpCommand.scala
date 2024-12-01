import sbt.*
import Keys.*

object HelpCommand {
  def manual = Command.command("manual") { state =>
    println("-" * 20)
    println("Help:")
    println("""
      run test [1 | 2] - will run test part 1 or 2 or both (if no number is supplied)
      run main [1 | 2] - will run main part 1 or 2 or both (if no number is supplied)
      """)
    println("-" * 20)
    state
  }
}
