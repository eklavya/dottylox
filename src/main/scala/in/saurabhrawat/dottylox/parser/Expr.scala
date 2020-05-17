package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.Token

enum Expr {
    case Binary(left: Expr, operator: Token, right: Expr)
    case Unary(operator: Token, right: Expr)
    case Grouping(expr: Expr)
    case Literal(lit: String | Double | Int | Boolean)
    case Variable(name: Token)
    case Assign(name: Token, value: Expr)
    case Logical(left: Expr, op: Token, right: Expr)
    case Call(callee: Expr, paren: Token, args: Vector[Stmt])
    case Nil
}