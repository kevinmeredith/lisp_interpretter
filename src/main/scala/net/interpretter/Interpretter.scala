package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object BadIfError        extends InterpretterError
	case object UnknownIdentError extends InterpretterError
	case object UnknownError      extends InterpretterError
	case object BadDefineError    extends InterpretterError

	val mathOps: Map[String, Function1[]] = Map(
		"+" => (_ + _),
		">" => (_ - _),
		"=" => (_ == _)
	)

	val boolOps: Map[String, Function2[Float, Float, Boolean]] = {
		">" => (_ > _),
	}

	def evaluate(e: SExpr)(map: Map[String, Any]): Either[InterpretterError, Any] = 
		e match {
			case Number(n) => Right(n)
			case Ident(s)  => Right(s)
			case Comb(es)  => es match {
				case Ident(x) :: Nil 						     => Right(x)
				case Number(x) :: Nil 						     => Right(x)
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)			
				case Ident("quote") :: xs 						 => Right(xs.foldLeft("")(_ + _.toString))
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident(">") :: Number(x) :: Number(y) :: Nil => Right(x > y)
				case Ident("+") :: Number(x) :: Number(y) :: Nil => Right(x + y)
				case Ident("=") :: Number(x) :: Number(y) :: Nil => Right(x == y)	
				case Ident(_) :: _							     => Left(UnknownIdentError)
				case (c @ Comb(_)) :: Nil						 => evaluate(c)(map)
				case _ 											 => Left(UnknownError)
			}
		}

	private def handleProc(proc: String, es: List[SExpr], map: Map[String, Any]): Either[InterpretterError, Any] = {
		mathOps.get(proc) match {
			case Some(op) => 
		}

	}

	private def handleDefine(v: String, exp: SExpr)(m: Map[String, Any]): Either[InterpretterError, Map[String, Any]] = 
		evaluate(exp)(m).right.map(result => m + (v -> result))

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr)(map: Map[String, Any]): Either[InterpretterError, Any] = 
		evaluate(test)(map) match {
			case Right(true)  => evaluate(conseq)(map)
			case Right(false) => evaluate(alt)(map)
			case _			  => Left(BadIfError) 
		}

}