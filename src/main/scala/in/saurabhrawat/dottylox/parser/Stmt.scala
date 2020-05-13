package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.Token

enum Stmt:
    case Expression(expr: Expr)
    case Print(expr: Expr)
    case Var(name: Token, init: Option[Expr] = None)
    case Block(stmts: Vector[Stmt])
    case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])