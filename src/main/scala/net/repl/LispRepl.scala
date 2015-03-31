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

	// def runSingleResult(map: M, ): IO[Either[(LispError, M), (Any, M)]] = 
	// 	input.map{i => (i, eval(i, map))}

	def runForever(map: M): IO[Unit] = for {
		input   <- readLn
		_       <- putStrLn(s">$input")
		parsed  <- IO(parse(input, map))
		evald   <- IO(interpret(parsed), map)
		_ 		<- putStrLn(evald)
		_ 		<- runForever(getMap(evald))
	} yield ()
	
	def eval(x: String, map: M): Either[(LispError, M), (Any, M)] = for {
		p     <- parse(x, map).right
		res   <- interpret(p._1, map).right
	} yield res 

	def parse(x: String, map: M): Either[(LispError, M), (SExpr, M)] = 
		new LispParser(x).SExprComplete.run() match {
			case Success(e)  => Right((e, map))
			case Failure(ex) => Left((ParseError(ex), map))
		}

	private def printInterpretterResult[A, B, C](res: Either[(A, C), (B, C)]): IO[String] = 
		res match {
			case Right(x) => IO(x._1.toString)
			case Left(x)  => IO(x._1.toString)
		}

	private def getMap[A, B](res: Either[(A, M), (B, M)]): M = res match {
		case Right(x) => x._2
		case Left(x) => x._2
	}

	def interpret(s: SExpr, map: M): Either[(InterpretterError, M), (Any, M)] = 
		LispInterpretter.evaluate(s)(map)
}