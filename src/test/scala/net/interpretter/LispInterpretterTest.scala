package net.interpretter

import org.scalatest._
import net.parser.LispParser
import net.parser.AST._
import net.interpretter.LispInterpretter._
import net.common.Error._
import net.repl.LispRepl.getMap
import scala.util.{Try, Success, Failure}

class LispInterpretterTest extends FlatSpec {
	
	val empty: Map[String,Any] = Map()

	"The Lisp Interpretter" should "return the number itself for a Number" in {
		val parsed = new LispParser("5").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((5, empty))), empty)
	}

	"The Lisp Interpretter" should "return the string literal itself for an Ident" in {
		val parsed = new LispParser("\"foo3\"").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right(("\"foo3\"", empty))), empty)
	}	

	"The Lisp Interpretter" should "return the variable's value for an existing variable" in {
		val parsed = new LispParser("x").SExprComplete.run()
		val map = Map("x" -> 456)
		testSuccessfulEval(parsed, Complete(Right((456, map))), map)
	}	

	"The Lisp Interpretter" should "fail for a non-existing variable" in {
		val parsed = new LispParser("x").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Left((NoVarExists, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return true for a '>' test" in {
		val parsed = new LispParser("(> 1000 1)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((true, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return false for a '>' test" in {
		val parsed = new LispParser("(> 0 999)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((false, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return true for a '='' test" in {
		val parsed = new LispParser("(= 65 65)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((true, Map()))), empty)
	}			

	"The Lisp Interpretter" should "return false for a '='' test" in {
		val parsed = new LispParser("(= 65 999)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((false, Map()))), empty)
	}				

	"The Lisp Interpretter" should "return false for '>' when strictly increasing" in {
		val parsed = new LispParser("(> 10 20 30 1000)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((false, Map()))), empty)
	}	


	"The Lisp Interpretter" should "return true for '>' when strictly decreasing" in {
		val parsed = new LispParser("(> 10 9 8 7 0)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((true, Map()))), empty)
	}

	"The Lisp Interpretter" should "return a ProcError for a parenthesized Number" in {
		val parsed = new LispParser("((((1234))))").SExprComplete.run()
		parsed.foreach({ x =>
			assert( LispInterpretter.evaluate(x)(empty) == Complete(Left((ProcError, Map()))) )
		})
	}			

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to false" in {
		val parsed = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((6, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return a ProcError for an SExpr beginning with two open parens" in {
		val parsed = new LispParser("((if (> 10 20) (+ 1 1) (+ 3 3)))").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Left((ProcError, Map()))), empty)
	}		

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to true" in {
		val parsed = new LispParser("(if (> 50 20) (+ 1 1) (+ 3 3))").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Right((2, Map()))), empty)
	}

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword" in {
		val parsed = new LispParser("(quote (+ 10 20))").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Right(("(+ 10 20)", Map()))), empty)
	}	

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #2" in {
		val parsed = new LispParser("(quote \"555foobar\")").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Right(("\"555foobar\"", Map()))), empty)
	}		

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #3" in {
		val parsed = new LispParser("(quote (+ 10 (+ 3 4)))").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Right(("(+ 10 (+ 3 4))", Map()))), empty)
	}			

	"The Lisp Interpretter" should "fail an 'if-statement' if the condition is a 'quote'" in {
		val parsed = new LispParser("(if (quote 100) 555 666)").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Left((BadIfError, Map()))), empty)
	}	

	"The Lisp Interpretter" should "succeeed for an 'if-statement' if the condition evaluates to true," + 
		"and the consequential action is a quote " in {
		val parsed = new LispParser("(if (= 0 0) (quote 555) 666)").SExprComplete.run()
		val evalResult = LispInterpretter.evaluate(parsed.get)(empty)
		testSuccessfulEval(parsed, Complete(Right(("555", Map()))), empty)
	}	

	"The Lisp Interpretter" should "succeed for a valid 'set!' statement expression + expression using it" in {
		val parsed = new LispParser("(set! x 100)").SExprComplete.run()
		val parsed2 = new LispParser("x").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((), Map("x" -> 100))), empty)
		testSuccessfulEval(parsed2, Complete(Right((100, Map("x" -> 100)))), Map("x" -> 100))
	}	

	"The Lisp Interpretter" should "succeed for a valid 'set!' statement expression + expression using it #2" in {
		val parsed = new LispParser("(set! x 100)").SExprComplete.run()
		val parsed2 = new LispParser("(+ x 2)").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right((), Map("x" -> 100))), empty)
		testSuccessfulEval(parsed2, Complete(Right((102, Map("x" -> 100)))), Map("x" -> 100))
	}		

	"The Lisp Interpretter" should "succeed for a valid 'set!' SExpression that has multiple parentheses" in {
		val parsed = new LispParser("(set! x (+ (+ 0 100) 1 2))").SExprComplete.run()
		testSuccessfulEval(parsed, Complete(Right(((),Map("x" -> 103)))), empty)
	}			

	"The Lisp Interpretter" should "fail for an invalid 'set!' SExpression that has multiple parentheses" in {
		val parsed = new LispParser("(set! x (+ (+ 0 100) 1 2)) BLEEP BLAH").SExprComplete.run()
		parsed match {
			case Failure(_) => assert (true)
			case _ 			=> assert (false)
		}
	}				

	"The Lisp Interpretter" should "succeed for define-ing, and then using, a Lambda." in {
		val parsed            = new LispParser("(define f (lambda (x) (+ x x)))").SExprComplete.run()
		val evald: EvalResult = LispInterpretter.evaluate(parsed.get)(empty)
		val newMap: M         = evald match { case Complete(Right((_, m))) => m }
		val parsed2           = new LispParser("(f 10)").SExprComplete.run()
		testSuccessfulEval(parsed2, Complete(Right((20,Map()))), newMap)
	}		

	"The Lisp Interpretter" should "succeed for define-ing 2 lambas, and then adding them." in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprComplete.run()
		val evald: EvalResult  = LispInterpretter.evaluate(parsed.get)(empty)
		val mapWithF: M        = evald match { case Complete(Right((_, m))) => m }

		val parsed2             = new LispParser("(define g (lambda (x) (+ x 3 4)))").SExprComplete.run()
		val evald2: EvalResult = LispInterpretter.evaluate(parsed2.get)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Complete(Right((_, m))) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(+ (f 10) (g 2))").SExprComplete.run()
		testSuccessfulEval(addLambdas, Complete(Right((29,mapWithBoth))), mapWithBoth)
	}	

	"The Lisp Interpretter" should "succeed for define-ing 2 lambas, and then comparing them for equality" in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprComplete.run()
		val evald: EvalResult  = LispInterpretter.evaluate(parsed.get)(empty)
		val mapWithF: M        = evald match { case Complete(Right((_, m))) => m }

		val parsed2             = new LispParser("(define g (lambda (x) (+ x 3 4)))").SExprComplete.run()
		val evald2: EvalResult = LispInterpretter.evaluate(parsed2.get)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Complete(Right((_, m))) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(= (f 10) (g 2))").SExprComplete.run()
		testSuccessfulEval(addLambdas, Complete(Right((false,mapWithBoth))), mapWithBoth)
	}	

	"The Lisp Interpretter" should "succeed for define-ing 2 lambas, and then comparing them for equality #2" in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprComplete.run()
		val evald: EvalResult  = LispInterpretter.evaluate(parsed.get)(empty)
		val mapWithF: M        = evald match { case Complete(Right((_, m))) => m }

		val parsed2             = new LispParser("(define g (lambda (x) 20))").SExprComplete.run()
		val evald2: EvalResult = LispInterpretter.evaluate(parsed2.get)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Complete(Right((_, m))) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(= (f 10) (g 2))").SExprComplete.run()
		testSuccessfulEval(addLambdas, Complete(Right((true,mapWithBoth))), mapWithBoth)
	}		


	def testSuccessfulEval(parsed: Try[SExpr], 
						   expected: EvalResult, 
						   map: M): Unit = parsed match {
		case Success(e) => assert (LispInterpretter.evaluate(e)(map) == expected)
		case Failure(_) => assert (false)
	}
}