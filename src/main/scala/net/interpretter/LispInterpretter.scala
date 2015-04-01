package net.interpretter

import net.parser.AST._

object LispInterpretter {
	
	import net.common.Error._

	val plusZero: Either[InterpretterError, Any] = Right(0)

	type M = Map[String, Any]

	def evaluate(e: SExpr)(map: M): Either[(InterpretterError, M), (Any, M)] =
		e match {
			case Number(n) 					    => Right((n, map))
			case Ident(s) if (stringLiteral(s)) => Right((s,map))
			case Ident(s)  					    => getVar(s)(map)
			case Comb(es)  					    => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)
				case Ident("quote") :: xs 						 => Right((xs.foldLeft("")(_ + _.toString)), map)
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident("set!") :: Ident(v) :: xs 			 => handleSet(v, map, xs)
				case Ident("lambda") :: xs 			             => handleLambda(xs, map)(_)
				case Ident(proc) :: xs							 => handleProc(proc, xs, map) // TODO: update for lambda
				case Nil										 => Left((EmptyExpression, map))
				case _											 => Left((ProcError, map))
			}
		}

// examples:
//(lambda (r) (* pi (* r r)))
//(lambda (r x) (* pi (* r x)))

	private def handleLambda(es: List[SExpr], map: M)(inputs: List[Any]): Either[(InterpretterError, M), (Any, M)] = es match {
		case Comb(es) :: fn :: Nil => handleFunction(es, map, fn)
		case _ 				       => Left((InvalidLambda, map))
	}

	private def handleFunction(es: List[SExpr], map: M, fn: SExpr)(inputs: List[Any]): Either[(InterpretterError, M), (Any, M)] = for {
		vars   <- getVars(es)
		locals <- getAppliedValues(vars, inputs) 
	} yield getFn(map, fn, locals)

	private def getAppliedValues(vars: List[String], inputs: List[Any]): Either[LambdaError, M] = {
		if(vars.length == inputs.length) {
			vars.zipWith(inputs).toMap
		}
		else {
			Left(WrongNumArgs(vars, inputs))
		}
	}

	private def getFn(map: M, fn: SExpr, locals: M): Either[(InterpretterError, M), (Any, M)] = for {
		(result, _) <- evaluate(fn)(map + locals).right // favor local variables over REPL globals
	} yield (result, map) // return original map since 'lambda' may not alter 

	private def getVars(es: List[SExpr]): Either[InvalidLambda, List[String]] = for {
		e <- es
		v <- extractIdent(e)
	} yield v

	private def extractIdent(x: SExpr): Either[InvalidLambda, String] = x match {
		case Ident(v) => Right(v)
		case _        => Left(BadLambda)
	}

	private def handleSet(v: String, map: M, es: List[SExpr]): Either[(InterpretterError, M), (Any, M)] = es match {
		case Ident(x) :: Nil    => Right(((),map + (v -> x)))
		case Number(x) :: Nil   => Right(((), map + (v -> x)))
		case (c @ Comb(_)) :: Nil => {
			evaluate(c)(map) match {
				case Right((x, m)) => Right(((), m + (v -> x)))
				case Left((x, m))  => Left((x, m))
			}
		}
		case _ 				    => Left((SetError, map))
	}

	type AddOp = Function2[Either[InterpretterError, Any], SExpr, Either[InterpretterError, Any]]		

	private def addFn(m: M): AddOp = {
		(acc: Either[InterpretterError, Any], elem: SExpr) => {
			evaluate(elem)(m) match {
				case Right((x, _)) => for {
					a     <- acc.right
					a_int <- validateInt(a.toString).right
					i 	  <- validateInt(x.toString).right 
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

	// TODO: DRY up - remove boilerplate from gtFn and eqFn
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

	// similar to Haskell's 'seq', but specific to `Either`
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