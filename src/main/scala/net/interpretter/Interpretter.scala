package net.interpretter

import net.parser.AST._

object Interpretter {
	
	sealed trait InterpretterError
	case object BadIfError         extends InterpretterError
	case object UnknownIdentError  extends InterpretterError
	case object UnknownError       extends InterpretterError
	case object BadDefineError     extends InterpretterError
	case object EmptyExpression	   extends InterpretterError
	case object ProcError		   extends InterpretterError
	
	sealed trait MathError extends InterpretterError
	case class NotAnInt(x: String) extends MathError

	val plusZero: Either[InterpretterError, Any] = Right(0)

	type AddOp      = Function2[Either[InterpretterError, Any], SExpr, Either[InterpretterError, Any]]
	type MapToMonoidOp = Function1[Map[String, Any], AddOp]

	val mathOps: Map[String, MapToMonoidOp] = Map(
		"+" -> addFn
	)

	val compareOps: Map[String, Function2[List[SExpr], Map[String,Any], Either[InterpretterError, Boolean]]] = Map(
		">" -> gtFn	
	)

	private def addFn(m: Map[String, Any]): AddOp = {
		(acc: Either[InterpretterError, Any], elem: SExpr) => {
			evaluate(elem)(m) match {
				case Right(x) => for {
					a     <- acc.right
					a_int <- validateInt(a.toString).right
					i 	  <- validateInt(x.toString).right // TODO: toString here???
				} yield a_int + i
				case left => left
			}
		}
	}

	// Continuously increasing
	private def gtFn(es: List[SExpr], m: Map[String, Any]): Either[InterpretterError, Boolean] = {
		es match {
			case _ :: xs => {
				val is: Either[InterpretterError, List[(Int, Int)]] = for {
					e    <- es
					x    <- xs  // TODO: cleaner way to get tail?
					res  <- evaluate(e)(m).right
					resX <- evaluate(x)(m).right
					i    <- validateInt(res.toString).right
					iX   <- validateInt(resX.toString).right
					} yield (i, iX)
				is.right.map(_.forall(y => y._1 > y._2))
			}
			case Nil	=> Right(true)
		}
	} 

	// TODO: deal with Ordering -- typeclass `Ord` in Haskell
	// val : Map[String, Function2[Float, Float, Boolean]] = {
	// 	">" => (_ > _),
	// }

	def evaluate(e: SExpr)(map: Map[String, Any]): Either[InterpretterError, Any] = 
		e match {
			case Number(n) => Right(n)
			case Ident(s)  => Right(s)
			case Comb(es)  => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)			
				case Ident("quote") :: xs 						 => Right(xs.foldLeft("")(_ + _.toString))
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident(proc) :: xs							 => handleProc(proc, xs, map)
				case Nil										 => Left(EmptyExpression)
				case _											 => Left(ProcError)
			}
		}

	// TODO: run the repl -- parsing + evaluating

	private def handleProc(proc: String, es: List[SExpr], map: Map[String, Any]): Either[InterpretterError, Any] = {
		es match {
			case Nil    => Left(ProcError)
			case _ :: _ => 	mathOps.get(proc) match {
				case Some(f) if f == "+" => es.foldLeft(plusZero)(f(map))
				case Some(f) if f == ">" => gtFn(es, map)
				case None    => ???
			}
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

	private def validateInt(x: String): Either[InterpretterError, Int] = {
		try { 
			Right( x.toInt )
		}
		catch {
			case _: NumberFormatException => Left(NotAnInt(x))
		}
	}

}