package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.Token

enum Expr {
    case Binary(left: Expr, operator: Token, right: Expr)
    case Unary(operator: Token, right: Expr)
    case Grouping(expr: Expr)
    case Literal(lit: String | Double | Int | Boolean)
    case Nil
}