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
        val left = question()
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
            call()

    def call() =
        def getChain(expr: Expr): Either[Error, Expr] =
            if matchOp(LEFT_PAREN)
                finishCall(expr).flatMap(getChain)
            else Right(expr)

        for
            expr <- primary()
            res <- getChain(expr)
        yield res
    
    def finishCall(callee: Expr): Either[Error, Expr] =
        def getArgs(args: Vector[Stmt]): Either[Error, Vector[Stmt]] =
            if args.length > 254
                Left(RuntimeError(peek(), "Cannot have more than 255 arguments"))
            else if matchOp(COMMA)
                val expr = if matchOp(FUN)
                        function("function", true)
                        else expression().map(e => Expression(e))
                expr.flatMap(e => getArgs(args :+ e))
            else Right(args)

        for
            args <- if !check(RIGHT_PAREN)
                for
                    first <- if matchOp(FUN)
                        function("function", true)
                        else expression().map(e => Expression(e))
                    all <- getArgs(Vector(first))
                yield all
                else Right(Vector.empty)
            paren <- consume(RIGHT_PAREN, "Expected ) after arguments")
        yield Call(callee, paren, args)


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
        else if matchOp(RETURN)
            returnStmt()
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
        val d = 
            if matchOp(VAR) 
                varDeclaration() 
            else if matchOp(FUN)
                function("function", false)
            else statement()
        if d.isLeft
            synchronise()
            Right(Expression(Nil))
        else d

    def returnStmt() =
        val keyword = previous()
        if !check(SEMICOLON)
            for
               e <- expression()
               _ <- consume(SEMICOLON, "Expected ; after return")
            yield Return(keyword, Some(e))
        else 
            consume(SEMICOLON, "Expected ; after return").map(_ => Return(keyword))
            
    def function(kind: String, anon: Boolean) =
        def getParams(params: Vector[Token] = Vector.empty): Either[Error, Vector[Token]] =
            if params.length > 255
                Left(RuntimeError(peek(), "Can not have more than 255 parameters"))
            else 
                consume(IDENTIFIER, "Expected parameter name").flatMap { p =>
                    if matchOp(COMMA)
                        getParams(params :+ p)
                    else Right(params :+ p)
                }

        for
            name <- if anon
                Right(Token(IDENTIFIER, "", None, peek().line))
                else consume(IDENTIFIER, s"Expected $kind name")
            _ <- consume(LEFT_PAREN, s"Expected ( after $kind name")
            params <- if !check(RIGHT_PAREN)
                getParams()
                else Right(Vector.empty)
            _ <- consume(RIGHT_PAREN, "Expected ) after parameters")
            _ <- consume(LEFT_BRACE, s"Expected { before $kind body")
            body <- block()
        yield 
            FuncStmt(name, params, body)
        
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
            

    
    