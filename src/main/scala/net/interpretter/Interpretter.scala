package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object FailedIfIntepretation extends InterpretterError

	def evaluate(e: SExpr): Either[InterpretterError, Any] = e match {
		case Number(n) => Right(n)
		case Ident(s)  => Right(s)
		case Comb(es)  => es match {
			case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)
			case Ident(">") :: Number(x) :: Number(y) :: Nil => Right(x > y)
			case Ident("+") :: Number(x) :: Number(y) :: Nil => Right(x + y)
			case _ => ???
		}
	}

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr): Either[InterpretterError, Any] = 
		evaluate(test) match {
			case Right(true)  => evaluate(conseq)
			case Right(false) => evaluate(alt)
			case _			  => Left(FailedIfIntepretation) 
		}

}