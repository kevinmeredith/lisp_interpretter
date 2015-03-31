package net.interpretter

import net.parser.AST._

object LispInterpretter {
	
	import net.common.Error._

	val plusZero: Either[InterpretterError, Any] = Right(0)

	type M = Map[String, Any]

	def evaluate(e: SExpr)(map: Map[String, Any]): Either[(InterpretterError, M), (Any, M)] =
		e match {
			case Number(n) 					    => Right((n, map))
			case Ident(s) if (stringLiteral(s)) => Right((s,map))
			case Ident(s)  					    => getVar(s)(map)
			case Comb(es)  					    => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)
				case Ident("quote") :: xs 						 => Right((xs.foldLeft("")(_ + _.toString)), map)
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident(proc) :: xs							 => handleProc(proc, xs, map)
				case Nil										 => Left((EmptyExpression, map))
				case _											 => Left((ProcError, map))
			}
		}

	type AddOp = Function2[Either[InterpretterError, Any], SExpr, Either[InterpretterError, Any]]		

	private def addFn(m: Map[String, Any]): AddOp = {
		(acc: Either[InterpretterError, Any], elem: SExpr) => {
			evaluate(elem)(m) match {
				case Right((x, _)) => for {
					a     <- acc.right
					a_int <- validateInt(a.toString).right
					i 	  <- validateInt(x.toString).right // TODO: toString here???
				} yield a_int + i
				case Left((x, _)) => Left(x)
			} 
		}
	}

	// Continuously decreasing (http://stackoverflow.com/q/29349946/409976)
	private def gtFn(es: List[SExpr], m: Map[String, Any]): Either[InterpretterError, Boolean] = {
		val evald: List[Either[InterpretterError, Any]] = es.map(e => fst(evaluate(e)(m)))
		val cs: List[Either[InterpretterError, Int]]    = evald.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[InterpretterError, List[Int]]  = f(cs)
		ints.right.map(decreasing(_)) 
	}

	private def fst[A, B, C, D](xs: Either[(A, B), (C, D)]): Either[A, C] = xs match {
		case Right((x, y)) => Right(x)
		case Left((x, y))  => Left(x)
	}

	// DRY up - remove boilerplate from gtFn and eqFn
	private def eqFn(es: List[SExpr], m: Map[String, Any]): Either[InterpretterError, Boolean] = {
		val evald: List[Either[InterpretterError, Any]] = es.map(e => fst(evaluate(e)(m)))
		val cs: List[Either[InterpretterError, Int]]    = evald.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[InterpretterError, List[Int]]  = f(cs)
		ints.right.map(allEquals(_)) 
	}

	private def decreasing(ys: List[Int]): Boolean = ys match {
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

	private def getVar(v: String)(map: Map[String, Any]): Either[(InterpretterError, M), (Any, M)] = 
		map.get(v) match {
			case None    => Left((NoVarExists,map))
			case Some(x) => Right((x, map))
		}

	private def stringLiteral(x: String) = x.startsWith("\"") && x.endsWith("\"")

	// TODO: run the repl -- parsing + evaluating

	private def handleProc(proc: String, es: List[SExpr], map: Map[String, Any]): Either[(InterpretterError, M), (Any, M)] = {
		val evald: Either[InterpretterError, Any] = proc match {
			case "+" => es.foldLeft(plusZero)(addFn(map))
			case ">" => gtFn(es, map)
			case "=" => eqFn(es, map)
			case _   => Left(ProcError)
		}

		evald match {
			case Right(x)  => Right((x, map))
			case Left(err) => Left((err, map))
		}
	}

	private def handleDefine(v: String, exp: SExpr)(m: Map[String, Any]): Either[(InterpretterError, M), (Unit, M)] = 
		evaluate(exp)(m) match {
			case Right((result, m)) => Right((), m + (v -> result))
			case Left((err, _))     => Left((err, m))
		}

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr)(map: Map[String, Any]): Either[(InterpretterError, M), (Any, M)] = 
		evaluate(test)(map) match {
			case Right((true, m))  => evaluate(conseq)(m)
			case Right((false, m)) => evaluate(alt)(m)
			case _			       => Left((BadIfError), map)
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