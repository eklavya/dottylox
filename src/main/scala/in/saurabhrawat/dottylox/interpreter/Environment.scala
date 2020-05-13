package in.saurabhrawat.dottylox.interpreter

import in.saurabhrawat.dottylox.Token
import in.saurabhrawat.dottylox.Result
import in.saurabhrawat.dottylox.Error._
import in.saurabhrawat.dottylox.Error

class Environment(enclosing: Option[Environment] = None):

    val variables = collection.mutable.Map.empty[String, Option[Result]]

    def define(name: String, value: Option[Result]): Environment =
        variables += (name -> value)
        this

    def get(name: Token): Option[Result] =
        variables.get(name.lexeme).orElse(enclosing.map(_.get(name))).flatten

    def assign(name: Token, value: Option[Result]): Either[Error, Environment] =
        if variables.contains(name.lexeme)
            Right(define(name.lexeme, value))
        else
            enclosing.map(_.assign(name, value)).getOrElse {
                Left(RuntimeError(name, s"Undefined variable ${name.lexeme}"))
            }