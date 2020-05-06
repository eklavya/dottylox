package in.saurabhrawat.dottylox

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader
import in.saurabhrawat.dottylox.parser.Parser
import in.saurabhrawat.dottylox.parser.AstPrinter

object Main:

  // var hadError = false

  def main(args: Array[String]): Unit = 
    if args.length > 1
      println("Usage: dottylox [script]")
      System.exit(64)
    else if args.length == 1
      runFile(args(0))
    else
      runPrompt()

  def runFile(file: String): Unit =
    val bytes = Files.readAllBytes(Paths.get(file))
    run(new String(bytes, Charset.defaultCharset())).left.foreach
        System.exit(65)

  def runPrompt(): Unit = 
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    var exited = false
    while (!exited)
      print("> ")
      val entered = reader.readLine
      if entered == "exit" then exited = true 
      else run(entered)
      // hadError = false

  def run(source: String): Either[_, Unit] =
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()
    println(tokens)
    val parser = Parser(tokens)
    parser.parse().map { ex =>
      println(AstPrinter.print(ex))
    }
  
  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def report(line: Int, where: String, message: String) =
    System.err.println(s"[line $line] Error $where: $message")
    // hadError = true