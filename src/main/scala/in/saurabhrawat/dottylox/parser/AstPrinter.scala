package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.parser.Expr._

object AstPrinter:

    def print(expr: Expr): String =
        expr match
            case Binary(l, op, r) =>
                s"(${op.lexeme} ${print(l)} ${print(r)})"
            case Unary(op, r) =>
                s"(${op.lexeme} ${print(r)})"
            case Grouping(expr) =>
                s"(group ${print(expr)})"
            case Literal(lit) =>
                lit.toString
            case Nil => "null"