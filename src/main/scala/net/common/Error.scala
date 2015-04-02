package net.common

import net.parser.AST.SExpr

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
	case object LambdaNotAllowed   extends InterpretterError
	
	sealed trait InvalidLambda     									 extends InterpretterError
	case class WrongNumArgs(fields: List[String], values: List[Any]) extends InvalidLambda
	case class InvalidArgument(x: SExpr)						     extends InvalidLambda
	case object DefineNotAllowed									 extends InvalidLambda
	case object BadLambda                                            extends InvalidLambda

	sealed trait MathError 		   extends InterpretterError
	case class NotAnInt(x: String) extends MathError

}