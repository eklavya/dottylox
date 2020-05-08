package in.saurabhrawat.dottylox.interpreter

import in.saurabhrawat.dottylox.parser.Expr._
import in.saurabhrawat.dottylox.parser.Expr
import in.saurabhrawat.dottylox.TokenType._
import in.saurabhrawat.dottylox.RuntimeError

object Interpreter:

    def eval(expr: Expr): Either[RuntimeError, String | Int | Double | Boolean| Unit] =
        expr match
            case Literal(lit) => Right(lit)
            case Grouping(ex) => eval(ex)
            case Unary(op, right) =>
                for
                    r <- eval(right)
                    res <- (op.tokenType, r) match
                        case (MINUS, r: Int) => Right(-r)
                        case (MINUS, r: Double) => Right(-r)
                        case (BANG, r) => Right(!isTruthy(r))
                        case (MINUS, r) => Left(RuntimeError(op, s"unsupported minus before ${r.getClass.getSimpleName}"))
                        case _ => Left(RuntimeError(op, "should never end up here"))
                yield res

            case Binary(left, op, right) =>
                for
                    l <- eval(left)
                    r <- eval(right)
                    res <- op.tokenType match
                            case PLUS =>
                                (l, r) match
                                    case (l: Double, r: Double) => Right(l + r)
                                    case (l: Int, r: Int) => Right(l + r)
                                    case (l: Double, r: Int) => Right(l + r)
                                    case (l: Int, r: Double) => Right(l + r)
                                    case (l: String, r: String) => Right(l + r)
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
                                    case (l: Int, r: Int) => Right(l / r)
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
            
            case Nil => Right(())
                        
    def isEqual(l: Any, r: Any) =
        if l == null && r == null then true
        else if l == null then false
        else l == r

    def isTruthy(a: Any): Boolean =
        if a == null then false
        else if a.isInstanceOf[Boolean] then a.asInstanceOf[Boolean]
        else true