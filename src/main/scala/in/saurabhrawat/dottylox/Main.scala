package in.saurabhrawat.dottylox

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader
import in.saurabhrawat.dottylox.parser.Parser
import in.saurabhrawat.dottylox.parser.AstPrinter
import in.saurabhrawat.dottylox.interpreter.Interpreter
import in.saurabhrawat.dottylox.Error._
import in.saurabhrawat.dottylox.interpreter.Environment

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
    run(new String(bytes, Charset.defaultCharset()), Environment(), false).left.foreach {
        case ParseError => System.exit(65)
        case e@RuntimeError(_, _) => 
          runtimeError(e)
          System.exit(70)
    }

  def runPrompt(): Unit = 
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    var exited = false
    var env = Environment()
    while (!exited)
      print("> ")
      val entered = reader.readLine
      if entered == "exit" then exited = true 
      else 
        val ret = run(entered, env, true)
        ret.left.foreach {
          case ParseError =>
          case e@RuntimeError(_, _) => runtimeError(e)
        }
        ret.foreach { (newEnv, _) =>
          env = newEnv
        }


  def run(source: String, env: Environment, inRepl: Boolean): Either[Error, (Environment, Result)] =
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()
    // println(tokens)
    val parser = Parser(tokens)
    parser.parse().flatMap { stmts =>
      Interpreter.interpret(stmts, env, inRepl)
    }
    
  
  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def report(line: Int, where: String, message: String) =
    System.err.println(s"[line $line] Error $where: $message")