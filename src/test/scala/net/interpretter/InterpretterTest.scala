package net.interpretter

import org.scalatest._
import net.parser.LispParser
import net.interpretter.Interpretter._

class InterpretterTest extends FlatSpec {
	
	val empty: Map[String,Any] = Map()

	"The Lisp Interpretter" should "return the number itself for a Number" in {
		val parsed = new LispParser("5").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(5) )
		})
	}

	"The Lisp Interpretter" should "return the string itself for an Ident" in {
		val parsed = new LispParser("foo3").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right("foo3") )
		})
	}	

	"The Lisp Interpretter" should "return true for a '>' test" in {
		val parsed = new LispParser("(> 1000 1)").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(true) )
		})
	}		

	"The Lisp Interpretter" should "return false for a '>' test" in {
		val parsed = new LispParser("(> 0 999)").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(false) )
		})
	}		

	"The Lisp Interpretter" should "return true for a '='' test" in {
		val parsed = new LispParser("(= 65 65)").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(true) )
		})
	}			

	"The Lisp Interpretter" should "return false for a '='' test" in {
		val parsed = new LispParser("(= 65 999)").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(false) )
		})
	}				

	"The Lisp Interpretter" should "return a ProcError for a parenthesized Number" in {
		val parsed = new LispParser("((((1234))))").SExprComplete.run()
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Left(ProcError) )
		})
	}			

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to false" in {
		val parsed = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(6) )
		})
	}		

	"The Lisp Interpretter" should "return a ProcError for an SExpr beginning with two open parens" in {
		val parsed = new LispParser("((if (> 10 20) (+ 1 1) (+ 3 3)))").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Left(ProcError) )
		})
	}		

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to true" in {
		val parsed = new LispParser("(if (> 50 20) (+ 1 1) (+ 3 3))").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right(2) )
		})
	}

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword" in {
		val parsed = new LispParser("(quote (+ 10 20))").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right("(+ 10 20)") )
		})
	}	

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #2" in {
		val parsed = new LispParser("(quote 555foobar)").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right("555foobar") )
		})
	}		

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #3" in {
		val parsed = new LispParser("(quote (+ 10 (+ 3 4)))").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right("(+ 10 (+ 3 4))") )
		})
	}			

	"The Lisp Interpretter" should "fail an 'if-statement' if the condition is a 'quote'" in {
		val parsed = new LispParser("(if (quote 100) 555 666)").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Left(BadIfError) )
		})
	}	

	"The Lisp Interpretter" should "success for an 'if-statement' if the condition evaluates to true," + 
		"and the consequential action is a quote " in {
		val parsed = new LispParser("(if (= 0 0) (quote 555) 666)").SExprComplete.run()
		val evalResult = Interpretter.evaluate(parsed.get)(empty)
		parsed.foreach({ x =>
			assert( Interpretter.evaluate(x)(empty) == Right("555"))
		})
	}		
}