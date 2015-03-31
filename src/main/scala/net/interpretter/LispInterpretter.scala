package net.interpretter

import net.parser.AST._

object LispInterpretter {
	
	sealed trait InterpretterError
	case object BadIfError         extends InterpretterError
	case object UnknownIdentError  extends InterpretterError
	case object UnknownError       extends InterpretterError
	case object BadDefineError     extends InterpretterError
	case object EmptyExpression	   extends InterpretterError
	case object ProcError		   extends InterpretterError
	case object NoVarExists		   extends InterpretterError
	
	sealed trait MathError extends InterpretterError
	case class NotAnInt(x: String) extends MathError

	val plusZero: Either[InterpretterError, Any] = Right(0)

	type AddOp = Function2[Either[InterpretterError, Any], SExpr, Either[InterpretterError, Any]]

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

	// Continuously decreasing (http://stackoverflow.com/q/29349946/409976)
	private def gtFn(es: List[SExpr], m: Map[String, Any]): Either[InterpretterError, Boolean] = {
		val evald: List[Either[InterpretterError, Any]] = es.map(evaluate(_)(m))
		val cs: List[Either[InterpretterError, Int]]    = evald.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[InterpretterError, List[Int]]  = f(cs)
		ints.right.map(increasing(_)) 
	}

	// DRY up - remove boilerplate from gtFn and eqFn
	private def eqFn(es: List[SExpr], m: Map[String, Any]): Either[InterpretterError, Boolean] = {
		val evald: List[Either[InterpretterError, Any]] = es.map(evaluate(_)(m))
		val cs: List[Either[InterpretterError, Int]]    = evald.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[InterpretterError, List[Int]]  = f(cs)
		ints.right.map(allEquals(_)) 
	}

	private def increasing(ys: List[Int]): Boolean = ys match {
		case Nil     => true
		case _ :: xs => ys.zip(xs).forall(y => y._1 > y._2)
	}

	private def allEquals[A](ys: List[A]): Boolean = ys match {
		case Nil     => true
		case _ :: xs => ys.zip(xs).forall(y => y._1 == y._2)
	}	

	private def f[A, B](es: List[Either[A, B]]): Either[A, List[B]] = 
		es.foldRight[Either[A, List[B]]](Right(Nil)) {
			(elem, acc) => elem match {
				case Right(x) => acc.right.map(y => x :: y)
				case Left(x)  => Left(x)
			}
		}

	def evaluate(e: SExpr)(map: Map[String, Any]): Either[InterpretterError, Any] = 
		e match {
			case Number(n) 					    => Right(n)
			case Ident(s) if (stringLiteral(s)) => Right(s)
			case Ident(s)  					    => getVar(s)(map)
			case Comb(es)  					    => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)			
				case Ident("quote") :: xs 						 => Right(xs.foldLeft("")(_ + _.toString))
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident(proc) :: xs							 => handleProc(proc, xs, map)
				case Nil										 => Left(EmptyExpression)
				case _											 => Left(ProcError)
			}
		}

	private def getVar(v: String)(map: Map[String, Any]): Either[InterpretterError, Any] = 
		map.get(v) match {
			case None    => Left(NoVarExists)
			case Some(x) => Right(x)
		}

	private def stringLiteral(x: String) = x.startsWith("\"") && x.endsWith("\"")

	// TODO: run the repl -- parsing + evaluating

	private def handleProc(proc: String, es: List[SExpr], map: Map[String, Any]): Either[InterpretterError, Any] = {
		proc match {
			case "+" => es.foldLeft(plusZero)(addFn(map))
			case ">" => gtFn(es, map)
			case "=" => eqFn(es, map)
			case _   => Left(ProcError)
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