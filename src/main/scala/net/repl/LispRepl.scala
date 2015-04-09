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

	// def runForever(map: M): IO[Unit] = for {
	// 	input  <- readLn
	// 	_      <- putStrLn(s">$input")
	// 	result <- IO(runSingle(input, map))
	// 	_ 	   <- putStrLn(result.toString)
	// 	_ 	   <- runForever(getMap(result, map))
	// } yield ()

	// def runSingle(input: String, map: M): EvalResult = 
	// 	parse(input) match {
	// 		case Success(e)  => interpret(e, map)
	// 		case Failure(ex) => Left((ParseError(ex), map))
	// 	}

	// def interpret(s: SExpr, map: M): EvalResult = 
	// 	LispInterpretter.evaluate(s)(map)

	// def parse(x: String): Try[SExpr] = 
	// 	new LispParser(x).SExprComplete.run() 
	
	def getMap(res: Either[(LispError, M), (MValue, M)], previousMap: M): M = res match {
		case Right((_, m)) => m
		case Left(_)       => previousMap
	}
}