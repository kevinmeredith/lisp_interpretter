package net.interpretter

import net.parser.AST._

object LispInterpretter {
	
	import net.common.Error._

	val plusZero: Either[LispError, Any] = Right(0)

	def evaluate(e: SExpr)(map: M): EvalResult =
		e match {
			case Number(n) 					    => Right((Val(n), map))
			case Ident(s) if (stringLiteral(s)) => Right((Val(s),map))
			case Ident(s)  					    => getVar(s)(map)
			case Comb(es)  					    => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)
				case Ident("quote") :: xs 						 => Right((Val(xs.foldLeft("")(_ + _.toString)), map))
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident("set!") :: Ident(v) :: xs 			 => handleSet(v, map, xs)
				case Ident("lambda") :: xs 			             => Partial{(inputs: List[Any], map: M) => handleLambda(xs)(map)(inputs) }
				case Ident(proc) :: xs							 => handleProc(proc, xs, map) // TODO: update for lambda
				case Nil										 => Left((EmptyExpression, map))
				case _											 => Left((ProcError, map))
			}
		}

	// Using default inputs in the event that no `define` accompanies the `inputs`.
	private def handleLambda(es: List[SExpr])(map: M)(inputs: List[Any]): EvalResult = es match {
		case Comb(es) :: fn :: Nil => handleFunction(es, map, fn)(inputs)
		case _ 				       => Left((BadLambda, map))
	}

	private def handleFunction(es: List[SExpr], map: M, fn: SExpr)(inputs: List[Any]): EvalResult = {
		val vars: Either[InvalidLambda, List[String]] = getVars(es)
		vars match {
			case Right(xs) => {
				val locals: Either[InvalidLambda, M] = getAppliedValues(xs)(inputs) // assume that Partial's are prevented by Parser?
				locals match {
					case Right(lcls) => applyFn(map, fn, lcls)
					case Left(x)     => Left((x, map))
				}
			}
			case Left(x)   => Left((x, map))
		}
	}

	private def getAppliedValues(vars: List[String])(inputs: List[Any]): Either[InvalidLambda, M] = {
		if(vars.length == inputs.length) {
			Right(vars.zip(inputs.map(Val(_))).toMap)
		}
		else {
			Left(WrongNumArgs(vars, inputs))
		}
	}

	private def applyFn(map: M, fn: SExpr, locals: M): EvalResult = {
		evaluate(fn)(map ++ locals) match { // favor local variables over REPL globals
			case Complete(Right((Val(Op), _)))   => Left((DefineNotAllowed, map))  // return original map since 'lambda' may not update the REPL/global map
			case Complete(Right((x, _)))   		 => Right((x, map))  		      // return original map since 'lambda' may not update the REPL/global map
			case Complete(Left((err, _)))    	 => Left((err, map))
		}
	}

	private def excludeDefine(result: (Any, M)): EvalResult = 
		result match {
			case ((Op, m)) => Left((DefineNotAllowed, m))
			case ((x, m))  => Right((Val(x),m))
		}

	val initial: Either[InvalidLambda, List[String]] = Right(Nil)

	private def getVars(es: List[SExpr]): Either[InvalidLambda, List[String]] =	
		es.foldRight(initial){
			(elem, acc) => acc.right.flatMap{a => extractIdent(elem).right.map(_ :: a) }
		}

	private def extractIdent(x: SExpr): Either[InvalidLambda, String] = x match {
		case Ident(v) => Right(v)
		case _        => Left(BadLambda)
	}

	private def handleSet(v: String, map: M, es: List[SExpr]): EvalResult = es match {
		case Ident(x) :: Nil    => Right((Val(SetOp(v, x)),map + (v -> Val(x))))
		case Number(x) :: Nil   => Right((Val(SetOp(v, x)), map + (v -> Val(x))))
		case (c @ Comb(_)) :: Nil => {
			evaluate(c)(map) match {
				case Complete((Right((Val(x), m)))) => Right((Val(SetOp(v, x)), m + (v -> Val(x))))
				case Complete(Left((x, m)))         => Left((x, m))
				case Complete((Right(((f @ Fn(_)), _))))  => Right((Val(Lambda), map + (v -> f)))
			}
		}
		case _ 				    => Left((SetError, map))
	}

	type AddOp = Function2[Either[LispError, Any], SExpr, Either[LispError, Any]]		

	private def addFn(m: M): AddOp = {
		(acc: Either[LispError, Any], elem: SExpr) => {
			val result = evaluate(elem)(m)
			println(result)
			evaluate(elem)(m) match {
				case Complete(Right((Val(x), _))) => for {
					a     <- acc.right
					a_int <- validateInt(a.toString).right
					i 	  <- validateInt(x.toString).right 
				} yield a_int + i
				case Complete(Left((x, _))) => Left(x)
				case _ 			            => Left(LambdaNotAllowed)
			} 
		}
	}

	// Continuously decreasing (http://stackoverflow.com/q/29349946/409976)
	private def gtFn(es: List[SExpr], m: M): Either[LispError, Boolean] = {
		val evald: List[EvalResult] = es.map(evaluate(_)(m))
		val resultsOnly: List[Either[LispError, Any]] = evald.map(extractComplete)
		val cs: List[Either[LispError, Int]]    = resultsOnly.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[LispError, List[Int]]  = f(cs)
		ints.right.map(decreasing(_)) 
	}

	private def extractComplete(result: EvalResult): Either[LispError, Any] =	
		result match {
			case Complete(Right((x, _)))     => Right(x)
			case Complete(Left((err, _)))    => Left(err)
			case Complete(Right((Fn(_), _))) => Left(LambdaNotAllowed)
		}

	private def fst[A, B, C, D](xs: Either[(A, B), (C, D)]): Either[A, C] = xs match {
		case Right((x, y)) => Right(x)
		case Left((x, y))  => Left(x)
	}

	// TODO: DRY up - remove boilerplate from gtFn and eqFn
	private def eqFn(es: List[SExpr], m: M): Either[LispError, Boolean] = {
		val evald: List[EvalResult] 				  = es.map(evaluate(_)(m))
		val resultsOnly: List[Either[LispError, Any]] = evald.map(extractComplete)
		val cs: List[Either[LispError, Int]]    	  = resultsOnly.map{x => x.right.flatMap{y: Any => validateInt(y.toString)} }
		val ints: Either[LispError, List[Int]]  	  = f(cs)
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

	private def getVar(v: String)(map: M): EvalResult = 
		map.get(v) match {
			case None    => Left((NoVarExists,map))
			case Some(x) => Right((x, map))
		}

	private def stringLiteral(x: String) = x.startsWith("\"") && x.endsWith("\"")

	private def handleProc(proc: String, es: List[SExpr], map: M): EvalResult = {
		val evald: Either[LispError, Any] = proc match {
			case "+" => es.foldLeft(plusZero)(addFn(map))
			case ">" => gtFn(es, map)
			case "=" => eqFn(es, map)
			case _   => Left(ProcError)
		}

		evald match {
			case Right(x)        => Right((Val(x), map))
			case Left(ProcError) => checkForLambda(proc, es, map)
			case Left(err) 		 => Left((err, map))
		}
	}

	private def checkForLambda(proc: String, es: List[SExpr], map: M): EvalResult = {
		map.get(proc) match {
			case Some(Fn(f)) => applyLambdaInputs(f, es, map) // TODO: asInstanceOf!
			case _ 			 => Left((ProcError, map))
		}
	}

	private def applyLambdaInputs(fn: Function2[List[Any], M, EvalResult], es: List[SExpr], map: M): EvalResult = {
		val evald: List[EvalResult]                 = es.map(evaluate(_)(map))
		val completes: List[Either[LispError, Any]] = evald.map(extractComplete)
		f(completes) match {
			case Right(xs) => fn(xs, map)
			case Left(err) => Left((err, map))
		}
	}

	private def handleDefine(v: String, exp: SExpr)(m: M): EvalResult = 
		evaluate(exp)(m) match {
			case Complete(Right((result, m)))     => Right(Val(Op), m + (v -> Val(result)))
			case Complete(Left((err, _)))         => Left((err, m))
			case Complete(Right((f @ Fn(_), _)))  => Right((Val(Lambda), m + (v -> f)))
		}

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr)(map: M): EvalResult = 
		evaluate(test)(map) match {
			case Complete(Right((Val(true), m)))  => evaluate(conseq)(m)
			case Complete(Right((Val(false), m))) => evaluate(alt)(m)
			case _			       			      => Left( (BadIfError, map) )
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