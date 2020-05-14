package in.saurabhrawat.dottylox

import org.junit.Test
import org.junit.Assert._
import in.saurabhrawat.dottylox.Scanner
import in.saurabhrawat.dottylox.parser.Parser
import in.saurabhrawat.dottylox.parser.Expr._
import in.saurabhrawat.dottylox.parser.AstPrinter

class ParserTest:

  @Test
  def print(): Unit =
    val expr = Binary(                     
        Unary(                                    
            Token(TokenType.MINUS, "-", None, 1),      
            Literal(123)),                        
        Token(TokenType.STAR, "*", None, 1),           
        Grouping(                                 
            Literal(45.67)))
    assertEquals(AstPrinter.print(expr), "(* (- 123) (group 45.67))")

  @Test
  def parseTernary1(): Unit =
    val source = "1 == 2 ? 3 == 4 ? 20 : 30 : 40;"
    val scanner = Scanner(source)
    val parser = Parser(scanner.scanTokens())
    val stmts = parser.parse().right.get
    assertEquals(AstPrinter.printStmts(stmts), "Expression((? (== 1 2) (: (? (== 3 4) (: 20 30)) 40)))")

  @Test
  def parseTernary2(): Unit =
    val source = "var a = 1 == 2 ? 2 == 3 ? 3 == 4 ? 4 == 5 ? 20 : 30 : 40 : 5 == 6 ? 50 : 60;"
    val scanner = Scanner(source)
    val parser = Parser(scanner.scanTokens())
    val stmts = parser.parse().right.get
    assertEquals(AstPrinter.printStmts(stmts), "Var(a, (? (== 1 2) (? (== 2 3) (: (? (== 3 4) (: (? (== 4 5) (: 20 30)) 40)) (? (== 5 6) (: 50 60))))))")