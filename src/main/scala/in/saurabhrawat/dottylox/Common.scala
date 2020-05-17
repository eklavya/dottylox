package in.saurabhrawat.dottylox

enum Error:
  case ParseError
  case RuntimeError(op: Token, erMsg: String)
  case ReturnVal(value: Result)

import Error._

type LiteralType = String | Double | Int | Boolean

type Result = Any

case class Token(tokenType: TokenType, lexeme: String, literal: Option[LiteralType], line: Int):
    override def toString() = s"$tokenType $lexeme $literal"


enum TokenType:
  // Single-character tokens.                      
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, 

  // One or two character tokens.                  
  BANG, BANG_EQUAL,                                
  EQUAL, EQUAL_EQUAL,                              
  GREATER, GREATER_EQUAL,                          
  LESS, LESS_EQUAL, QUESTION, COLON,                                

  // Literals.                                     
  IDENTIFIER, STRING, NUMBER, COMMENT,                    

  // Keywords.                                     
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,  
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,    

  EOF                     


def reportError(t: Token, erMsg: String) =
    if t.tokenType == TokenType.EOF
       Main.report(t.line, " at end", erMsg) 
    else
        Main.report(t.line, s" at '${t.lexeme}'", erMsg)

def runtimeError(err: RuntimeError) =
  println(s"${err.erMsg}\n[line ${err.op.line}]")
