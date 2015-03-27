package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object Err extends InterpretterError 

	def evaluate(e: SExpr): Either[InterpretterError, Any] = e match {
		case Number(n) => Right(n)
		case Ident(s)  => Right(s)
		case Comb(es)  => Left(Err)
	}

}