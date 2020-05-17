package in.saurabhrawat.dottylox.interpreter

import in.saurabhrawat.dottylox.parser.Stmt.FuncStmt
import in.saurabhrawat.dottylox.Result
import in.saurabhrawat.dottylox.parser.Stmt
import in.saurabhrawat.dottylox.Error._

class LoxFunction(decl: FuncStmt, closure: Environment) extends LoxCallable:
    override def arity() = decl.params.size

    override def call(args: Vector[Result]) =
        val env = Environment(Some(closure))
        val funcEnv = decl.params.zip(args).foldLeft(env) { case (acc, (p, a)) =>
            acc.define(p.lexeme, Some(a))
        }
        val ret = Interpreter.evalStmt(decl.body, funcEnv, false)
        ret.fold({
            case ReturnVal(v) =>
                Right(v)
            case e => Left(e)
        }, x => Right(x._2))

    override def toString() = s"<fn ${decl.name.lexeme}>"