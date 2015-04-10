package net.repl

import net.parser.LispParser
import net.parser.AST._
import net.interpretter.LispInterpretter
import net.interpretter.LispInterpretter._
import net.common.Error._
import scala.util.{Try, Success, Failure}
import scalaz.effect.IO
import scalaz.effect.IO._

object LispRepl {

	// Evaluates a single SExpr at a time, recursively using the Map
	def runForever(map: M): IO[Unit] = for {
		input  <- readLn
		_      <- putStrLn(s">$input")
		result <- IO(runSingle(input, map))
		_ 	   <- putStrLn(result.toString)
		_ 	   <- runForever(sndE(result))
	} yield ()

	private def runSingle(input: String, map: M): Either[(LispError, M), (MValue, M)] = 
		parse(input) match {
			case Success(e)  => interpret(e, map)
			case Failure(ex) => Left((ParseError(ex), map))
		}

	private def interpret(s: SExpr, map: M): Either[(LispError, M), (MValue, M)] = 
		LispInterpretter.evaluate(s)(map)

	private def parse(x: String): Try[SExpr] = 
		new LispParser(x).OneSExpr.run() 

	// TODO: evaluate like Racket?
	def evalString(exprs: String): Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = {
		val parsed: Try[Seq[SExpr]] = new LispParser(exprs).SExprs.run()
		parsed match {
			case Success(sexprs) => Right(evaluateSExprs(sexprs, Map()))
			case Failure(f)      => Left(ParseError(f)) // TODO: more info on parse failure
		}
	}
}