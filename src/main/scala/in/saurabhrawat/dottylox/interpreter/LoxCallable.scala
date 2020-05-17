package in.saurabhrawat.dottylox.interpreter

import in.saurabhrawat.dottylox.Result
import in.saurabhrawat.dottylox.Error

trait LoxCallable:
    def arity(): Int
    def call(args: Vector[Result]): Either[Error, Result]
