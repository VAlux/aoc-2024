@main def entrypoint(args: String*) =
  // inputs:
  val p1TestInput = FileLoader.readFileLines("input-test-p1.txt")
  val p2TestInput = FileLoader.readFileLines("input-test-p2.txt")
  val mainInput   = FileLoader.readFileLines("input.txt")

  println("Day 4:")
  println("-" * 20)

  args.headOption match
    case Some("test") =>
      args.last match
        case "1" => testP1(p1TestInput)
        case "2" => testP2(p2TestInput)
        case _   =>
          testP1(p1TestInput)
          testP2(p2TestInput)
    case Some("main") =>
      args.last match
        case "1" => runP1(mainInput)
        case "2" => runP2(mainInput)
        case _   =>
          runP1(mainInput)
          runP2(mainInput)
    case _            =>
      testP1(p1TestInput)
      testP2(p2TestInput)
      runP1(mainInput)
      runP2(mainInput)

  println("-" * 20)

def testP1(input: List[String]) =
  val p1TestResult = d4p1.solve(input)
  println(s"[TEST] P1: $p1TestResult")

def testP2(input: List[String]) =
  val p2TestResult = d4p2.solve(input)
  println(s"[TEST] P2: $p2TestResult")

def runP1(input: List[String]) =
  val p1Result = d4p1.solve(input)
  println(s"[ACTUAL] P1: $p1Result")

def runP2(input: List[String]) =
  val p2Result = d4p2.solve(input)
  println(s"[ACTUAL] P2: $p2Result")
