package in.saurabhrawat.dottylox

import org.junit.Test
import org.junit.Assert._
import in.saurabhrawat.dottylox.Scanner
import in.saurabhrawat.dottylox.parser.Parser
import in.saurabhrawat.dottylox.parser.Expr._
import in.saurabhrawat.dottylox.parser.AstPrinter
import in.saurabhrawat.dottylox.interpreter.Interpreter
import in.saurabhrawat.dottylox.interpreter.Environment

class InterpreterTest:

  def evalSource(source: String) =
    val scanner = Scanner(source)
    val parser = Parser(scanner.scanTokens())
    val stmts = parser.parse().right.get
    Interpreter.interpret(stmts, Environment(), false)

  @Test
  def eval1(): Unit =
    val source = "print (1 + 2) / 3 * 4;"
    assertEquals(evalSource(source).isRight, true)

  @Test
  def eval2(): Unit =
    val source = "print \"test\" + 1;"
    assertEquals(evalSource(source).isRight, true)

  @Test
  def eval3(): Unit =
    val source = """print "test" + "me";"""
    assertEquals(evalSource(source).isRight, true)
