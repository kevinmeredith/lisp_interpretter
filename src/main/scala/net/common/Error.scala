package net.common

object Error {

	sealed trait LispError
	case class ParseError(ex: Throwable) extends LispError
	
	sealed trait InterpretterError extends LispError
	case object BadIfError         extends InterpretterError
	case object BadDefineError     extends InterpretterError
	case object EmptyExpression	   extends InterpretterError
	case object ProcError		   extends InterpretterError
	case object NoVarExists		   extends InterpretterError
	case object SetError		   extends InterpretterError

	sealed trait MathError extends InterpretterError
	case class NotAnInt(x: String) extends MathError

}