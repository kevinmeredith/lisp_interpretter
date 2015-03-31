package net.repl

import scalaz.effect.IO
import scalaz.effect.IO._

object Testing {
	def simplePrint: IO[(String, String)]  = {
		val input: IO[String] = readLn
		input.map{i => (i, i + "foobar")}
	}

	def runExample: Unit = {
		val (original, fnApplied) = simplePrint.unsafePerformIO
		println(">" + original)
		println(fnApplied)
	}

	def runForever(m: Map[String, String]): IO[Unit] = for {
		_ <- putStrLn(s"map:$m")
		_ <- putStrLn("type key")
		k <- readLn
		_ <- putStrLn(s"you typed: $k")
		_ <- putStrLn("type value")
		v <- readLn
		_ <- putStrLn(s"you typed: $v")
		_ <- runForever(m + (k -> v))
	} yield ()

}