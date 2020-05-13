package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.parser.Expr._
import in.saurabhrawat.dottylox.parser.Stmt._

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
            case Variable(v) =>
                v.lexeme
            case Assign(name, value) =>
                s"(= ${name.lexeme} ${print(expr)})"
            case Nil => "null"

    def printStmts(stmts: Vector[Stmt]): String =
        stmts.map {
            case Print(expr) => s"Print(${print(expr)})"
            case Expression(expr) => s"Expression(${print(expr)})"
            case Var(name, expr) => s"Var(${name.lexeme}, ${expr.map(print).getOrElse("")})"
            case Block(sts) => s"Block(${printStmts(sts)})"
            }.mkString