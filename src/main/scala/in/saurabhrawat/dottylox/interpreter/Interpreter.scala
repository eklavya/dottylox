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
                    res <- (l, r, op.tokenType) match
                        case (l: Double, r: Double, tt: PLUS.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Int, r: Int, tt: PLUS.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Double, r: Double, tt: MINUS.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Int, r: Int, tt: MINUS.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Double, r: Double, tt: STAR.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Int, r: Int, tt: STAR.type) =>
                            Right(numberOps(l, r, tt))
                        case (l: Double, r: Double, tt: SLASH.type) =>
                            Right(l / r)
                        case (l: Int, r: Int, tt: SLASH.type) =>
                            Right(l / r)
                        case (l: Double, r: Int, tt: PLUS.type) =>
                            Right(l / r)
                        case (l: Int, r: Double, tt: PLUS.type) =>
                            Right(l / r)
                        case (l: String, r: String, tt: PLUS.type) =>
                            Right(l + r)
                        case (l: Double, r: Double, tt: GREATER.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Double, r: Double, tt: GREATER_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Double, r: Double, tt: LESS.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Double, r: Double, tt: LESS_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Int, r: Int, tt: GREATER.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Int, r: Int, tt: GREATER_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Int, r: Int, tt: LESS.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: Int, r: Int, tt: LESS_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: String, r: String, tt: GREATER.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: String, r: String, tt: GREATER_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: String, r: String, tt: LESS.type) =>
                            Right(ordOps(l, r, tt))
                        case (l: String, r: String, tt: LESS_EQUAL.type) =>
                            Right(ordOps(l, r, tt))
                        case (l, r, EQUAL_EQUAL) =>
                            Right(isEqual(l, r))
                        case (l, r, BANG_EQUAL) =>
                            Right(!isEqual(l, r))
                        case (l, r, _) =>
                            Left(RuntimeError(op, s"unsupported operation ${op.lexeme} between l: ${l.getClass.getSimpleName} and r: ${r.getClass.getSimpleName}"))
                yield res
                        
    def numberOps[T: Numeric](i1: T, i2: T, op: PLUS.type | MINUS.type | STAR.type)(using ops: Numeric[T]): T =
        op match
            case op: PLUS.type => ops.plus(i1, i2)
            case op: MINUS.type => ops.minus(i1, i2)
            case op: STAR.type => ops.times(i1, i2)
            
    def ordOps[T](i1: T, i2: T, op: GREATER.type | LESS.type | LESS_EQUAL.type | GREATER_EQUAL.type)(using ops: Ordering[T]): Boolean =
        op match
            case op: GREATER.type => ops.gt(i1, i2)
            case op: GREATER_EQUAL.type => ops.gteq(i1, i2)
            case op: LESS.type => ops.lt(i1, i2)
            case op: LESS_EQUAL.type => ops.lteq(i1, i2)

    def isEqual(l: Any, r: Any) =
        if l == null && r == null then true
        else if l == null then false
        else l == r

    def isTruthy(a: Any): Boolean =
        if a == null then false
        else if a.isInstanceOf[Boolean] then a.asInstanceOf[Boolean]
        else true