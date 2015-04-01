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

	import LispInterpretter.M

	def runForever(map: M): IO[Unit] = for {
		input  <- readLn
		_      <- putStrLn(s">$input")
		result <- IO(runSingle(input, map))
		_ 	   <- putStrLn(result.toString)
		_ 	   <- runForever(getMap(result))
	} yield ()

	def runSingle(input: String, map: M): Either[(LispError, M), (Any, M)] = for {
		parsed <- parse(input, map).right
		result <- interpret(parsed._1, map).right
	} yield result

	def interpret(s: SExpr, map: M): Either[(InterpretterError, M), (Any, M)] = 
		LispInterpretter.evaluate(s)(map)

	def parse(x: String, map: M): Either[(LispError, M), (SExpr, M)] = 
		new LispParser(x).SExprComplete.run() match {
			case Success(e)  => Right((e, map))
			case Failure(ex) => Left((ParseError(ex), map))
		}

	 def getMap[A, B](res: Either[(A, M), (B, M)]): M = res match {
		case Right(x) => x._2
		case Left(x) => x._2
	}
}