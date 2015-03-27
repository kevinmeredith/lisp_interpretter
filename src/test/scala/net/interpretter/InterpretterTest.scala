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

	"The Lisp Interpretter" should "handle 'if' statements when test == false" in {
		val parsed = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExpr.run()
		val evalResult = Interpretter.evaluate(parsed.get)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x) == Right(6) )
		})
	}		

	"The Lisp Interpretter" should "handle 'if' statements when test == true" in {
		val parsed = new LispParser("(if (> 50 20) (+ 1 1) (+ 3 3))").SExpr.run()
		val evalResult = Interpretter.evaluate(parsed.get)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x) == Right(2) )
		})
	}			

}