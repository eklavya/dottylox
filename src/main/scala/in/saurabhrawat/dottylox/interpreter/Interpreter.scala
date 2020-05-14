package in.saurabhrawat.dottylox.interpreter

import in.saurabhrawat.dottylox.parser.Expr._
import in.saurabhrawat.dottylox.parser.Expr
import in.saurabhrawat.dottylox.parser.Stmt._
import in.saurabhrawat.dottylox.parser.Stmt
import in.saurabhrawat.dottylox.TokenType._
import in.saurabhrawat.dottylox.Error
import in.saurabhrawat.dottylox.Error._
import java.util.ArrayList
import in.saurabhrawat.dottylox.Result
import in.saurabhrawat.dottylox._

object Interpreter:

    def eval(expr: Expr, env: Environment): Either[Error, Result] =
        expr match
            case Literal(lit) => Right(lit)
            case Grouping(ex) => eval(ex, env)
            case Unary(op, right) =>
                for
                    r <- eval(right, env)
                    res <- (op.tokenType, r) match
                        case (MINUS, r: Int) => Right(-r)
                        case (MINUS, r: Double) => Right(-r)
                        case (BANG, r) => Right(!isTruthy(r))
                        case (MINUS, r) => Left(RuntimeError(op, s"unsupported minus before ${r.getClass.getSimpleName}"))
                        case _ => Left(RuntimeError(op, "should never end up here"))
                yield res

            case Binary(left, op, right) =>
                if op.tokenType == QUESTION
                    for
                        l <- eval(left, env)
                        res <- right match
                            case Binary(v1, op, v2) =>
                                if op.tokenType == COLON 
                                    if isTruthy(l) then eval(v1, env) else eval(v2, env)
                                else eval(right, env)
                    yield res

                else
                    for
                        l <- eval(left, env)
                        r <- eval(right, env)
                        res <- op.tokenType match
                                case PLUS =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l + r)
                                        case (l: Int, r: Int) => Right(l + r)
                                        case (l: Double, r: Int) => Right(l + r)
                                        case (l: Int, r: Double) => Right(l + r)
                                        case (l: String, r) => Right(l + r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case MINUS =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l - r)
                                        case (l: Int, r: Int) => Right(l - r)
                                        case (l: Double, r: Int) => Right(l - r)
                                        case (l: Int, r: Double) => Right(l - r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case STAR =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l * r)
                                        case (l: Int, r: Int) => Right(l * r)
                                        case (l: Double, r: Int) => Right(l * r)
                                        case (l: Int, r: Double) => Right(l * r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case SLASH =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l / r)
                                        case (l: Int, r: Int) => 
                                            if r == 0 
                                                Left(RuntimeError(op, s"tried to divide $l by 0"))
                                            else Right(l / r)
                                        case (l: Double, r: Int) => Right(l / r)
                                        case (l: Int, r: Double) => Right(l / r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case GREATER =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l > r)
                                        case (l: Int, r: Int) => Right(l > r)
                                        case (l: Double, r: Int) => Right(l > r)
                                        case (l: Int, r: Double) => Right(l > r)
                                        case (l: String, r: String) => Right(l > r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case LESS =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l < r)
                                        case (l: Int, r: Int) => Right(l < r)
                                        case (l: Double, r: Int) => Right(l < r)
                                        case (l: Int, r: Double) => Right(l < r)
                                        case (l: String, r: String) => Right(l < r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case LESS_EQUAL =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l <= r)
                                        case (l: Int, r: Int) => Right(l <= r)
                                        case (l: Double, r: Int) => Right(l <= r)
                                        case (l: Int, r: Double) => Right(l <= r)
                                        case (l: String, r: String) => Right(l <= r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case GREATER_EQUAL =>
                                    (l, r) match
                                        case (l: Double, r: Double) => Right(l >= r)
                                        case (l: Int, r: Int) => Right(l >= r)
                                        case (l: Double, r: Int) => Right(l >= r)
                                        case (l: Int, r: Double) => Right(l >= r)
                                        case (l: String, r: String) => Right(l >= r)
                                        case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                                case EQUAL_EQUAL => Right(isEqual(l, r))
                                case BANG_EQUAL => Right(!isEqual(l, r))
                                case _ => Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between $l: ${l.getClass.getSimpleName} and $r: ${r.getClass.getSimpleName}"))
                    yield res
        
            case Variable(v) =>
                env.get(v).map(res => Right(res)).getOrElse(Left(RuntimeError(v, s"variable ${v.lexeme} is nil")))

            case Assign(name, expr) =>
                for
                    res <- eval(expr, env)
                    _ <- env.assign(name, Some(res))
                yield res

            case Logical(left, op, right) =>
                for
                    l <- eval(left, env)
                    res <- if op.tokenType == OR
                        if isTruthy(l) then Right(l) else eval(right, env)
                    else
                        if !isTruthy(l) then Right(l) else eval(right, env)
                yield res

            case Nil => Right(null)
    
    def evalStmt(stmt: Stmt, env: Environment, inRepl: Boolean): Either[Error, Environment] =
        stmt match
            case Print(expr) => 
                val out = eval(expr, env)
                out.foreach { res =>
                    println(res)
                }
                out.map(_ => env)

            case Expression(expr) => 
                val out = eval(expr, env)
                if inRepl
                    out.foreach(println)
                out.map(_ => env)

            case Var(name, expr) =>
                expr.fold(Right(env.define(name.lexeme, None))) { e =>
                    eval(e, env).map(res => env.define(name.lexeme, Some(res)))
                }

            case Block(stmts) =>
                interpret(stmts, Environment(Some(env)), inRepl)
            
            case If(cond, thenBranch, elseBranch) =>
                for
                    c <- eval(cond, env)
                    env <- if isTruthy(c) then evalStmt(thenBranch, env, inRepl)
                    else
                        elseBranch.map(e => evalStmt(e, env, inRepl)).getOrElse(Right(env))
                yield env

            case w@While(cond, stmt) =>
                for
                    c <- eval(cond, env)
                    e <- if isTruthy(c) 
                        evalStmt(stmt, env, inRepl).flatMap(_ => evalStmt(w, env, inRepl))
                        else Right(env)
                yield e


    def interpret(stmts: Vector[Stmt], env: Environment, inRepl: Boolean): Either[Error, Environment] =
        if stmts.isEmpty
            Right(env)
        else
            for
                _ <- evalStmt(stmts.head, env, inRepl)
                _ <- interpret(stmts.tail, env, inRepl)
            yield env
                        
    def isEqual(l: Any, r: Any) =
        if l == null && r == null then true
        else if l == null then false
        else l == r

    def isTruthy(a: Any): Boolean =
        if a == null then false
        else if a.isInstanceOf[Boolean] then a.asInstanceOf[Boolean]
        else true