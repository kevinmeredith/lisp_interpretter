package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object BadIfError   extends InterpretterError
	case object UnknownError extends InterpretterError

	def evaluate(e: SExpr): Either[InterpretterError, Any] = e match {
		case Number(n) => Right(n)
		case Ident(s)  => Right(s)
		case Comb(es)  => es match {
			case Ident(x) :: Nil 						     => Right(x)
			case Number(x) :: Nil 						     => Right(x)
			case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)			
			case Ident("quote") :: xs 						 => Right(xs.foldLeft("")(_ + _.toString))
			case Ident(">") :: Number(x) :: Number(y) :: Nil => Right(x > y)
			case Ident("+") :: Number(x) :: Number(y) :: Nil => Right(x + y)
			case Ident("=") :: Number(x) :: Number(y) :: Nil => Right(x == y)			
			case _ => Left(UnknownError)
		}
	}

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr): Either[InterpretterError, Any] = 
		evaluate(test) match {
			case Right(true)  => evaluate(conseq)
			case Right(false) => evaluate(alt)
			case _			  => Left(BadIfError) 
		}

}