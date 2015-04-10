package net.repl

import net.parser.LispParser
import net.parser.AST._
import net.interpretter.LispInterpretter
import net.interpretter.LispInterpretter._
import net.common.Error._
import scala.util.{Try, Success, Failure}
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.NonEmptyList

object LispRepl {

	// def runForever(map: M): IO[Unit] = for {
	// 	input  <- readLn
	// 	_      <- putStrLn(s">$input")
	// 	result <- IO(runSingle(input, map))
	// 	_ 	   <- putStrLn(result.toString)
	// 	_ 	   <- runForever(getMap(result, map))
	// } yield ()

	// def runSingle(input: String, map: M): Either[(LispError, M), (MValue, M)] = 
	// 	parse(input) match {
	// 		case Success(e)  => interpret(e, map)
	// 		case Failure(ex) => Left((ParseError(ex), map))
	// 	}

	// def interpret(s: SExpr, map: M): Either[(LispError, M), (MValue, M)] = 
	// 	LispInterpretter.evaluate(s)(map)

	// def parse(x: String): Try[SExpr] = 
	// 	new LispParser(x).SExprComplete.run() 

	// def evalString(exprs: String, delimiter: String): Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = {
	// 	val inputs: List[String]    				     = exprs.split(delimiter)
	// 	val parsed: List[Try[SExpr]] 				  	 = inputs.map(new LispParser(_).SExprComplete.run())
	// 	val collected: (List[LispError], List[SExpr])    = getParsedResult(parsed)
	// 	val result: Either[List[LispError], List[SExpr]] = collected match {
	// 		case (Nil, xs) => Right(xs)
	// 		case (xs, _ )  => Left(xs)
	// 	}
	// 	g(result)
	// }

	// def g(parsed: Either[List[LispError], List[SExpr]]): Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = {
	// 	lazy val accumulator: (M, List[MValue], MValue) = (Map(), )

	// 	parsed match {
	// 		case Right(es)      => es.foldLeft()
	// 		case Left(err :: _) => Left(err)
	// 		//case Left(Nil)      => // use scalaz NonEmptyList?
	// 	}
	// }

	// private def getParsedResult(parsed: List[Try[SExpr]]): (List[LispError], List[SExpr]) = 
	// 	parsed.foldRight(List[SExpr]()) {
	// 		(parsed, acc) => {
	// 			val (errs, es) = acc
	// 			parsed match {
	// 				case Failure(ex) => (ParseError(ex) :: errs, es)
	// 				case Success(e)  => (errs, e :: es)
	// 			}
	// 		}
	// 	}
}