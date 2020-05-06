package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox._
import in.saurabhrawat.dottylox.TokenType._
import Expr._
import java.util.ArrayList


class Parser(tokens: ArrayList[Token]):

    private var current = 0

    def expression() = comma()

    def comma() = getExpression(question, COMMA)

    def getExpression(getExp: () => Either[ParseError.type, Expr], ops: TokenType*): Either[ParseError.type, Expr] =
        var e = getExp()
        
        while matchOp(ops: _*) do
            val op = previous()
            e = e.flatMap { ex =>
                for
                    right <- getExp()
                yield
                    Binary(ex, op, right)
            }
        e

    def question(): Either[ParseError.type, Expr] = 
        var e = equality()
        
        if matchOp(QUESTION)
            val op = previous()
            e = e.flatMap { ex =>
                for
                    right <- choose()
                yield
                    Binary(ex, op, right)
            }
        e

    def choose() = 
        var e = question()
        
        if matchOp(COLON) 
            val op = previous()
            e = e.flatMap { ex =>
                for
                    right <- question()
                yield
                    Binary(ex, op, right)
            }
        e

    def equality() = getExpression(comparison, BANG_EQUAL, EQUAL_EQUAL)
        
    def matchOp(tTypes: TokenType*): Boolean =
        if tTypes.exists(check)
            advance()
            true
        else
            false

    def check(t: TokenType): Boolean =
        if isAtEnd() then false else peek().tokenType == t

    def advance(): Token =
        if !isAtEnd()
            current += 1
        previous()

    def isAtEnd(): Boolean =
        peek().tokenType == EOF

    def peek(): Token = tokens.get(current)

    def previous() = tokens.get(current - 1)

    def comparison() = getExpression(addition, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)
        
    def addition() = getExpression(multiplication, MINUS, PLUS)

    def multiplication() = getExpression(unary, SLASH, STAR)

    def unary(): Either[ParseError.type, Expr] =
        if matchOp(BANG, MINUS)
            val op = previous()
            for 
                right <- unary()
            yield
                Unary(op, right)
        else
            primary()

    def primary(): Either[ParseError.type, Expr] =
        val token = advance()
        token.tokenType match
            case FALSE => Right(Literal(false))
            case TRUE => Right(Literal(true))
            case NIL => Right(Expr.Nil)
            case n@NUMBER => Right(Literal(token.literal.get))
            case s@STRING => Right(Literal(token.literal.get))
            case LEFT_PAREN => 
                for
                    e <- expression()
                    _ <- consume(RIGHT_PAREN, "Expect ')' after expression.")
                yield
                    Grouping(e)
            case _ =>
                Left(error(peek(), "Expect expression."))

    def consume(t: TokenType, erMsg: String): Either[ParseError.type, Token] = 
        if check(t) then Right(advance())
        else Left(error(peek(), erMsg))

    def error(t: Token, erMsg: String): ParseError.type =
        reportError(t, erMsg)
        ParseError

    def synchronise(): Unit =
        advance()

        if !isAtEnd()
            if previous().tokenType == SEMICOLON
                ()
            else
                peek().tokenType match
                    case CLASS => 
                    case FUN => 
                    case VAR => 
                    case FOR => 
                    case IF => 
                    case WHILE => 
                    case PRINT => 
                    case RETURN => 
                    case _ => synchronise()

    def parse(): Either[ParseError.type, Expr] = expression()

    
    