package net.interpretter

import net.parser.AST._

object LispInterpretter {
	
	import net.common.Error._

	def evaluateSExprs(es: Seq[SExpr], map: M): List[Either[(LispError, M), (MValue, M)]] = {
		val zero: (List[Either[(LispError, M), (MValue, M)]], M) = (Nil, map)

		val (results, _) = 
			es.foldLeft(zero){
				(acc, elem) => { 
					val (previous, m) = acc
					val result        = LispInterpretter.evaluate(elem)(m)
					(previous ++ List(result), sndE(result))
			}
		}

		results
	}

	def evaluate(e: SExpr)(map: M): Either[(LispError, M), (MValue, M)] =
		e match {
			case Number(n) 					    => Right((Val(n), map))
			case Ident(s) if (stringLiteral(s)) => Right((Val(s),map))
			case Ident(s)  					    => getVar(s)(map)
			case Comb(es)  					    => es match {
				case Ident("if") :: test :: conseq :: alt :: Nil => handleIf(test, conseq, alt)(map)
				case Ident("quote") :: xs 						 => Right((Val(xs.foldLeft("")(_ + _.toString)), map))
				case Ident("define") :: Ident(v) :: exp :: Nil   => handleDefine(v, exp)(map)
				case Ident("set!") :: Ident(v) :: xs 			 => handleSet(v, map, xs)
				case Ident("lambda") :: xs 			             => Right((Fn( (inputs, map) => handleLambda(xs)(map)(inputs) ), map)) // ignored `map` on right?
				case Ident(proc) :: xs							 => handleProc(proc, xs, map) 
				case Nil										 => Left((EmptyExpression, map))
				case Comb(Ident("lambda") :: xs) :: inputs       => handleLambda(xs)(map)(inputs)
				case _									         => Left((ProcError(""), map)) // TODO: better error info
			}
		}

	// Using default inputs in the event that no `define` accompanies the `inputs`.
	private def handleLambda(es: List[SExpr])(map: M)(inputs: List[SExpr]): Either[(LispError, M), (MValue, M)] = es match {
		case Comb(es) :: fn :: Nil => handleFunction(es, map, fn)(inputs)
		case _ 				       => Left((BadLambda, map))
	}

	private def handleFunction(es: List[SExpr], map: M, fn: SExpr)(inputs: List[SExpr]): Either[(LispError, M), (MValue, M)] = {
		val vars: Either[InvalidLambda, List[String]] = getVars(es)
		vars match {
			case Right(xs) => {
				val locals: Either[LispError, M] = getAppliedValues(xs)(inputs)(map) 
				locals match {
					case Right(lcls) => applyFn(map, fn, lcls)
					case Left(x)     => Left((x, map))
				}
			}
			case Left(x)   => Left((x, map))
		}
	}

	private def getAppliedValues(vars: List[String])(inputs: List[SExpr])(map: M): Either[LispError, M] = {
		if(vars.length == inputs.length) {
			val evald: List[Either[(LispError, M), (MValue, M)]]   = inputs.map(evaluate(_)(map))
			val results: Either[(LispError, M), List[(MValue,M)]]  = f(evald)
			val resultsWithoutMap: Either[LispError, List[MValue]] = results match {
				case Right(xs)     => Right(xs.map{_._1})
				case Left((err,_)) => Left(err)
			}
			resultsWithoutMap.right.map{xs => vars.zip(xs).toMap}
		}
		else {
			Left(WrongNumArgs(vars, inputs))
		}
	}

	private def applyFn(map: M, fn: SExpr, locals: M): Either[(LispError, M), (MValue, M)] = {
		evaluate(fn)(map ++ locals) match { // favor local variables over REPL globals
			case Right((Val(Op | Lambda), _)) => Left((DefineNotAllowed, map))  // return original map since 'lambda' may not update the REPL/global map
			case Right((x, _))   	 		  => Right((x, map))  		       // return original map since 'lambda' may not update the REPL/global map
			case Left((err, _))    	          => Left((err, map))
		}
	}

	private def excludeDefine(result: (Any, M)): Either[(LispError, M), (MValue, M)] = 
		result match {
			case ((Op | Lambda, m)) => Left((DefineNotAllowed, m))
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

	private def handleSet(v: String, map: M, es: List[SExpr]): Either[(LispError, M), (MValue, M)] = es match {
		case Ident(x) :: Nil    => Right(Val(SetOp(v, x)),map + (v -> Val(x)))
		case Number(x) :: Nil   => Right((Val(SetOp(v, x)), map + (v -> Val(x))))
		case (c @ Comb(_)) :: Nil => {
			evaluate(c)(map) match {
				case Right((Val(x), m))    => Right((Val(SetOp(v, x)), m + (v -> Val(x))))
				case left @ Left(_)        => left
				case Right((f @ Fn(_), _)) => Right((Val(Lambda), map + (v -> f)))
			}
		}
		case _ 				    => Left((SetError, map))
	}

	private def add(es: List[SExpr], m: M): Either[LispError, Int] = {
		val ints: Either[LispError, List[Int]] = getInts(es, m)
		ints.right.map(_.sum)
	}

	private def getInts(es: List[SExpr], m: M): Either[LispError, List[Int]] = {
		val evald: List[Either[(LispError, M), (MValue, M)]]  = es.map(evaluate(_)(m))
		val resultsOnly: List[Either[LispError, SingleValue]] = evald.map(extractComplete)
		val cs: List[Either[LispError, Int]]    			  = resultsOnly.map{x => x.right.flatMap{y: SingleValue => validateInt(y)} }
		f(cs)
	}

	// Continuously decreasing (http://stackoverflow.com/q/29349946/409976)
	private def gtFn(es: List[SExpr], m: M): Either[LispError, Boolean] = {
		val ints: Either[LispError, List[Int]] = getInts(es, m)
		ints.right.map(decreasing(_)) 
	}

	private def extractComplete(result: Either[(LispError, M), (MValue, M)]): Either[LispError, SingleValue] =	
		result match {
			case Right((v @ Val(_), _)) => Right(v)
			case Left((err, _))         => Left(err)
			case Right((Fn(_), _))      => Left(LambdaNotAllowed)
		}

	private def fst[A, B, C, D](xs: Either[(A, B), (C, D)]): Either[A, C] = xs match {
		case Right((x, y)) => Right(x)
		case Left((x, y))  => Left(x)
	}

	private def eqFn(es: List[SExpr], m: M): Either[LispError, Boolean] = {
		val ints: Either[LispError, List[Int]] = getInts(es, m)
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

	private def getVar(v: String)(map: M): Either[(LispError, M), (MValue, M)] = 
		map.get(v) match {
			case None     => Left((NoVarExists(v),map))
			case Some(x)  => Right((x, map))
		}

	// TODO: trim?
	private def stringLiteral(x: String) = x.startsWith("\"") && x.endsWith("\"")

	private def handleProc(proc: String, es: List[SExpr], map: M): Either[(LispError, M), (MValue, M)] = {
		val evald: Option[Either[LispError, Any]] = proc match {
			case "+" => Some(add(es, map))
			case ">" => Some(gtFn(es, map))
			case "=" => Some(eqFn(es, map))
			case _   => None
		}

		evald match {
			case Some(Right(x))  => Right((Val(x), map))
			case None 		     => checkForLambda(proc, es, map)
			case Some(Left(err)) => Left((err, map))
		}
	}

	private def checkForLambda(proc: String, es: List[SExpr], map: M): Either[(LispError, M), (MValue, M)] = 
		map.get(proc) match {
			case Some(Fn(f)) => applyLambdaInputs(f, es, map) 
			case _ 			 => Left((ProcError(proc), map))
		}

	private def applyLambdaInputs(fn: Function2[List[SExpr], M, Either[(LispError, M), (MValue, M)]], 
								  es: List[SExpr], 
							  	  map: M): Either[(LispError, M), (MValue, M)] = 
		fn(es, map)
	
	private def handleDefine(v: String, exp: SExpr)(m: M): Either[(LispError, M), (MValue, M)] = 
		evaluate(exp)(m) match {
			case Right((value @ Val(_), m)) => Right(Val(Op), m + (v -> value))
			case Right((f @ Fn(_), m))      => Right(Val(Lambda), m + (v -> f))
			case Left((err, _))             => Left((err, m))
		}

	private def handleIf(test: SExpr, conseq: SExpr, alt: SExpr)(map: M): Either[(LispError, M), (MValue, M)] = 
		evaluate(test)(map).right.flatMap(x => {
			val (res, m) = x
			res match {
				case Val(true)  => evaluate(conseq)(m)
				case Val(false) => evaluate(alt)(m)
				case Val(bad)   => Left((BadIfResultError(bad), map))
				case _          => Left((BadIfResultError(res), map))
			}	
		})

	private def validateInt(x: SingleValue): Either[InterpretterError, Int] = x match {
		case Val(x) => {
			try {
				Right(x.toString.toInt)
			}
			catch {
				case _: NumberFormatException => Left(NotAnInt(x.toString))
			}
		}
	}

    def sndE[A, B, C, D](e: Either[(A, Map[C,D]), (B, Map[C,D])]): Map[C, D] = e match {
		case Right((_, m)) => m
		case Left((_, m))  => m
	}

	// TODO: not sure if this is necessary. Returning a Left should still have the right map
	// def getMap(res: Either[(LispError, M), (MValue, M)], previousMap: M): M = res match {
	// 	case Right((_, m)) => m
	// 	case Left(_)       => previousMap
	// }	
}

