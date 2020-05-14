package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox._
import in.saurabhrawat.dottylox.TokenType._
import Expr._
import java.util.ArrayList
import Stmt._
import in.saurabhrawat.dottylox.Error._
import in.saurabhrawat.dottylox.Error


class Parser(tokens: ArrayList[Token]):

    private var current = 0

    def expression() = assignment()

    def assignment(): Either[Error, Expr] = 
        val left = comma()
        if matchOp(EQUAL)
            val equals = previous()
            for
                rValue <- assignment()
                lValue <- left
                res <- lValue match
                            case Variable(name) =>
                                Right(Assign(name, rValue))
                            case _ => Left(error(equals, "invalid assignment"))
            yield res
        else left

    def comma() = getExpression(question, COMMA)

    def getExpression(getExp: () => Either[Error, Expr], ops: TokenType*): Either[Error, Expr] =
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

    def getLogicalExpression(getExp: () => Either[Error, Expr], ops: TokenType*): Either[Error, Expr] =
        var e = getExp()
        
        while matchOp(ops: _*) do
            val op = previous()
            e = e.flatMap { ex =>
                for
                    right <- getExp()
                yield
                    Logical(ex, op, right)
            }
        e

    def question(): Either[Error, Expr] = 
        var e = or()
        
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

    def or() = getLogicalExpression(and, OR)

    def and() = getLogicalExpression(equality, AND)

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

    def unary(): Either[Error, Expr] =
        if matchOp(BANG, MINUS)
            val op = previous()
            for 
                right <- unary()
            yield
                Unary(op, right)
        else
            primary()

    def primary(): Either[Error, Expr] =
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
            case PLUS | SLASH | STAR =>
                error(peek(), s"${token.lexeme} is not a unary operator. It requires both left and right expressions.")
                consume(NUMBER, "Expect a number.").map(_ => Nil)
            case IDENTIFIER => Right(Variable(previous()))
            case _ =>
                Left(error(peek(), "Expect expression."))

    def consume(t: TokenType, erMsg: String): Either[Error, Token] = 
        if check(t) then Right(advance())
        else Left(error(peek(), erMsg))

    def error(t: Token, erMsg: String): Error =
        reportError(t, erMsg)
        Error.ParseError

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

    def printStatement() =
        for
            e <- expression()
            _ <- consume(SEMICOLON, "Expected ';' after value.")
        yield Print(e)

    def expressionStatement() =
        for
            e <- expression()
            _ <- consume(SEMICOLON, "Expected ';' after value.")
        yield Expression(e)

    def statement() =
        if matchOp(PRINT)
            printStatement()
        else if matchOp(LEFT_BRACE)
            block()
        else if matchOp(IF)
            ifStmt()
        else if matchOp(WHILE)
            whileStmt()
        else if matchOp(FOR)
            forStmt()
        else expressionStatement()

    def varDeclaration() =
        for 
            name <- consume(IDENTIFIER, "Expected variable name here.")
            ex <- if matchOp(EQUAL) then expression() else Right(Nil)
            _ <- consume(SEMICOLON, "Expected ; after variable declaration.")
        yield 
            ex match
                case Nil => Var(name, None)
                case _ => Var(name, Some(ex))
            

    def declaration() =
        val d = if matchOp(VAR) then varDeclaration() else statement()
        if d.isLeft
            synchronise()
            Right(Expression(Nil))
        else d

    def block(stmts: Vector[Stmt] = Vector.empty): Either[Error, Stmt] =
        if !check(RIGHT_BRACE) && !isAtEnd()
            declaration().flatMap(d => block(stmts :+ d))
        else
            consume(RIGHT_BRACE, "Expected closing } after block").map { _ =>
                Block(stmts)
            }

    def ifStmt(): Either[Error, Stmt] =
        for
            _ <- consume(LEFT_PAREN, "Expect a condition after if starting with (")
            cond <- expression()
            _ <- consume(RIGHT_PAREN, "Expected )")
            thenBranch <- statement()
            res <- if matchOp(ELSE)
                        statement().map { elseBranch =>
                            If(cond, thenBranch, Some(elseBranch))
                        }
            else
                Right(If(cond, thenBranch, None))
        yield
            res

    def whileStmt(): Either[Error, Stmt] = 
        for
            _ <- consume(LEFT_PAREN, "Expect a condition after while starting with (")
            cond <- expression()
            _ <- consume(RIGHT_PAREN, "Expected )")
            stmt <- statement()
        yield While(cond, stmt)

    def forStmt(): Either[Error, Stmt] =
        for
            _ <- consume(LEFT_PAREN, "Expect a condition after while starting with (")
            init <- if matchOp(SEMICOLON)
                Right(Empty)
                else if matchOp(VAR)
                    varDeclaration()
                else expressionStatement()
            chk <- if check(SEMICOLON)
                Right(Nil)
                else expression()
            _ <- consume(SEMICOLON, "Expected ;")
            inc <- if check(RIGHT_PAREN)
                Right(Nil)
                else expression()
            _ <- consume(RIGHT_PAREN, "Expected )")
            body <- statement()
        yield Block(Vector(init, While(chk, Block(Vector(body, Expression(inc))))))

    def parse(stmts: Vector[Stmt] = Vector.empty): Either[Error, Vector[Stmt]] = 
        if !isAtEnd()
            for
                s <- declaration()
                res <- parse(stmts :+ s)
            yield res
        else Right(stmts)
            

    
    