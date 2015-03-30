package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object BadIfError         extends InterpretterError
	case object UnknownIdentError  extends InterpretterError
	case object UnknownError       extends InterpretterError
	case object BadDefineError     extends InterpretterError
	
	sealed trait MathError extends InterpretterError
	case class NotAnInt(x: String) extends MathError

	val plusZero: Either[MathError, Int] = Right(0)

	type MapToMonoidOp = Function1[Map[String, Any], Function2[Either[MathError, Int], SExpr, Either[MathError, Int]]]

	val mathOps: Map[String, MapToMonoidOp] = Map(
		"+" -> {(acc: Either[MathError, Int], elem: SExpr) => 
			evaluate(elem)(_: MapToMonoidOp) match {
				case Right(x) => for {
					a <- acc.right
					i <- validateInt(x.toString).right // toString here???
				} yield a + i
				case left => left
			}
		}
	)	

	// TODO: deal with Ordering -- typeclass `Ord` in Haskell
	// val : Map[String, Function2[Float, Float, Boolean]] = {
	// 	">" => (_ > _),
	// }

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
				case Ident(proc) :: xs							 => handleProc(proc, xs, map)
				// case Ident(">") :: Number(x) :: Number(y) :: Nil => Right(x > y)
				// case Ident("+") :: Number(x) :: Number(y) :: Nil => Right(x + y)
				// case Ident("=") :: Number(x) :: Number(y) :: Nil => Right(x == y)	
				// case Ident(_) :: _							     => Left(UnknownIdentError)
				//case c @ Comb(_) :: xs						     => evaluate(c)(map)
			}
		}

	// TODO: run the repl -- parsing + evaluating

	private def handleProc(proc: String, es: List[SExpr], map: Map[String, Any]): Either[InterpretterError, Any] = {
		mathOps.get(proc) match {
			case Some(f) => es.foldLeft(plusZero)(f(map))
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

	private def validateInt(x: String): Either[MathError, Int] = {
		try { 
			Right( x.toInt )
		}
		catch {
			case _: NumberFormatException => Left(NotAnInt(x))
		}
	}

}