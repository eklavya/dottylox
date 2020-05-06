package in.saurabhrawat.dottylox

import org.junit.Test
import org.junit.Assert._
import in.saurabhrawat.dottylox.Scanner

class ScannerTest:

  @Test 
  def comment(): Unit = 
    val source = "/* this is a comment with a * and a / */"
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    assertEquals(tokens.size() == 2, true)

  @Test
  def test1(): Unit = 
    val source = """// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators"""
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()
    assertEquals(tokens.size == 20, true)

  @Test
  def multilineComment(): Unit =
    val source = """/* this is a multiline comment
    here comes line no 1 *
    line number two /
    line 3 //
    line 4 var a = 10;*/
    var a = 10;"""
    val scanner = new Scanner(source)
    val tokens = scanner.scanTokens()
    assertEquals(tokens.size == 7, true)
