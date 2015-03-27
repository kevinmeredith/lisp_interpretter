package net.interpretter

import org.scalatest._
import net.parser.LispParser

class InterpretterTest extends FlatSpec {
	
	"The Lisp Interpretter" should "return the number itself for a Number" in {
		val parsed = new LispParser("5").SExpr.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x) == Right(5) )
		})
	}

	"The Lisp Interpretter" should "return the string itself for an Ident" in {
		val parsed = new LispParser("foo3").SExpr.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x) == Right("foo3") )
		})
	}	

}