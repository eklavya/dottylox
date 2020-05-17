package in.saurabhrawat.dottylox.parser

import in.saurabhrawat.dottylox.Token

enum Stmt:
    case Expression(expr: Expr)
    case Print(expr: Expr)
    case Var(name: Token, init: Option[Expr] = None)
    case Block(stmts: Vector[Stmt])
    case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
    case While(condition: Expr, body: Stmt)
    case FuncStmt(name: Token, params: Vector[Token], body: Stmt)
    // case Lambda(params: Vector[Token], body: Stmt)
    case Return(keyword: Token, value: Option[Expr] = None)
    case Empty