package net.interpretter

import org.scalatest._
import net.parser.LispParser
import net.parser.AST._
import net.interpretter.LispInterpretter._
import net.common.Error._
import scala.util.{Try, Success, Failure}

class LispInterpretterTest extends FlatSpec {
	
	val empty: M = Map()

	// helper function to avoid copy-ing `Val(...)` throughout all tests
	implicit def anyToSingleValue(x: Any): SingleValue = Val(x)

	"The Lisp Interpretter" should "return the number itself for a Number" in {
		val parsed = new LispParser("5").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((5, empty)), empty)
	}

	"The Lisp Interpretter" should "return the string literal itself for an Ident" in {
		val parsed = new LispParser("\"foo3\"").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(("\"foo3\"", empty)), empty)
	}	

	"The Lisp Interpretter" should "return the variable's value for an existing variable" in {
		val parsed = new LispParser("x").SExprs.run().get.head
		val map = Map("x" -> Val(456))
		testSuccessfulEvalSingle(parsed, Right(((Val(456), map))), map)
	}	

	"The Lisp Interpretter" should "fail for a non-existing variable" in {
		val parsed = new LispParser("x").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Left((NoVarExists("x"), Map())), empty)
	}		

	"The Lisp Interpretter" should "return true for a '>' test" in {
		val parsed = new LispParser("(> 1000 1)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((true, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return false for a '>' test" in {
		val parsed = new LispParser("(> 0 999)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((false, Map())), empty)
	}		

	"The Lisp Interpretter" should "return the sum of numbers when using '+'" in {
		val parsed = new LispParser("(+ 0 1 2 3 0)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((6, Map())), empty)
	}	

	"The Lisp Interpretter" should "return true for a '='' test" in {
		val parsed = new LispParser("(= 65 65)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((true, Map()))), empty)
	}			

	"The Lisp Interpretter" should "return false for a '='' test" in {
		val parsed = new LispParser("(= 65 999)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((false, Map()))), empty)
	}				

	"The Lisp Interpretter" should "return false for '>' when strictly increasing" in {
		val parsed = new LispParser("(> 10 20 30 1000)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((false, Map()))), empty)
	}	


	"The Lisp Interpretter" should "return true for '>' when strictly decreasing" in {
		val parsed = new LispParser("(> 10 9 8 7 0)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((true, Map()))), empty)
	}

	"The Lisp Interpretter" should "return a ProcError for a parenthesized Number" in {
		val parsed = new LispParser("((((1234))))").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Left((ProcError(""), Map())), empty)
	}			

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to false" in {
		val parsed = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(((6, Map()))), empty)
	}		

	"The Lisp Interpretter" should "return a ProcError for an SExpr beginning with two open parens" in {
		val parsed = new LispParser("((if (> 10 20) (+ 1 1) (+ 3 3)))").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Left((ProcError(""), Map())), empty)
	}		

	"The Lisp Interpretter" should "handle 'if' statements when condition evaluates to true" in {
		val parsed = new LispParser("(if (> 50 20) (+ 1 1) (+ 3 3))").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Right(((Val(2), Map()))), empty)
	}

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword" in {
		val parsed = new LispParser("(quote (+ 10 20))").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Right(("(+ 10 20)", Map())), empty)
	}	

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #2" in {
		val parsed = new LispParser("(quote \"555foobar\")").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Right(("\"555foobar\"", Map())), empty)
	}		

	"The Lisp Interpretter" should "print out the un-evaluated expression for the 'quote' keyword #3" in {
		val parsed = new LispParser("(quote (+ 10 (+ 3 4)))").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Right(("(+ 10 (+ 3 4))", Map())), empty)
	}			

	"The Lisp Interpretter" should "fail an 'if-statement' if the condition is a 'quote'" in {
		val parsed = new LispParser("(if (quote 100) 555 666)").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Left((BadIfResultError("100"), Map())), empty)
	}	

	"The Lisp Interpretter" should "succeeed for an 'if-statement' if the condition evaluates to true," + 
		"and the consequential action is a quote " in {
		val parsed = new LispParser("(if (= 0 0) (quote 555) 666)").SExprs.run().get.head
		val evalResult = LispInterpretter.evaluate(parsed)(empty)
		testSuccessfulEvalSingle(parsed, Right((Val("555"), Map())), empty)
	}	

	"The Lisp Interpretter" should "succeed for a valid 'set!' statement expression + expression using it" in {
		val parsed = new LispParser("(set! x 100)").SExprs.run().get.head
		val parsed2 = new LispParser("x").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((Val(SetOp("x",100)),Map("x" -> Val(100)))), empty) 
		testSuccessfulEvalSingle(parsed2, Right((Val(100), Map("x" -> Val(100)))), Map("x" -> Val(100)))
	}	

	"The Lisp Interpretter" should "succeed for a valid 'set!' statement expression + expression using it #2" in {
		val parsed = new LispParser("(set! x 100)").SExprs.run().get.head
		val parsed2 = new LispParser("(+ x 2)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right(SetOp("x",100), Map("x" -> Val(100))), empty)
		testSuccessfulEvalSingle(parsed2, Right((Val(102), Map("x" -> Val(100)))), Map("x" -> Val(100)))
	}		

	"The Lisp Interpretter" should "succeed for a valid 'set!' SExpression that has multiple parentheses" in {
		val parsed = new LispParser("(set! x (+ (+ 0 100) 1 2))").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((SetOp("x",103),Map("x" -> Val(103)))), empty)
	}			

	"The Lisp Interpretter" should "parse, but fail to evaluate the last proc call to a non-existent variable/function." in {
		val parsed = new LispParser("(set! x (+ (+ 0 100) 1 2)) BLEEP").SExprs.run()
		val evald = evaluateSExprs(parsed.get.toList, empty)
		val expected = Right((Val(SetOp("x",103)),Map("x" -> Val(103)))) :: Left((NoVarExists("BLEEP"), Map("x" -> Val(103)))) :: Nil
		assert(evald == expected)
	}				

	"The Lisp Interpretter" should "succeed for define-ing, and then using, a Lambda." in {
		val parsed            = new LispParser("(define f (lambda (x) (+ x x)))").SExprs.run().get.head
		val evald: Either[(LispError, M), (MValue, M)] = LispInterpretter.evaluate(parsed)(empty)
		val newMap: M         = evald match { case Right((_, m)) => m }
		val parsed2           = new LispParser("(f 10)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed2, Right((Val(20),newMap)), newMap)
	}		

	"The Lisp Interpretter" should "succeed for define-ing 2 lambas, and then adding them." in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprs.run().get.head
		val evald: Either[(LispError, M), (MValue, M)]  = LispInterpretter.evaluate(parsed)(empty)
		val mapWithF: M        = evald match { case Right((_, m)) => m }

		val parsed2             = new LispParser("(define g (lambda (x) (+ x 3 4)))").SExprs.run().get.head
		val evald2: Either[(LispError, M), (MValue, M)] = LispInterpretter.evaluate(parsed2)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Right((_, m)) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(+ (f 10) (g 2))").SExprs.run().get.head
		testSuccessfulEvalSingle(addLambdas, Right((Val(29),mapWithBoth)), mapWithBoth)
	}	

	"The Lisp Interpretter" should "succeed for define-ing 2 lambas, and then comparing them for equality" in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprs.run().get.head
		val evald: Either[(LispError, M), (MValue, M)]  = LispInterpretter.evaluate(parsed)(empty)
		val mapWithF: M        = evald match { case Right((_, m)) => m }

		val parsed2             = new LispParser("(define g (lambda (x) (+ x 3 4)))").SExprs.run().get.head
		val evald2: Either[(LispError, M), (MValue, M)] = LispInterpretter.evaluate(parsed2)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Right((_, m)) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(= (f 10) (g 2))").SExprs.run().get.head
		testSuccessfulEvalSingle(addLambdas, Right((Val(false),mapWithBoth)), mapWithBoth)
	}	

	"The Lisp Interpretter" should "succeed for define-ing 2 lambdas, and then comparing them for equality #2" in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprs.run().get.head
		val evald: Either[(LispError, M), (MValue, M)]  = LispInterpretter.evaluate(parsed)(empty)
		val mapWithF: M        = evald match { case Right((_, m)) => m }

		val parsed2             = new LispParser("(define g (lambda (x) 20))").SExprs.run().get.head
		val evald2: Either[(LispError, M), (MValue, M)] = LispInterpretter.evaluate(parsed2)(mapWithF)
		val mapWithBoth: M     = evald2 match { case Right((_, m)) => m }

		println("mapWithBoth: " + mapWithBoth)

		val addLambdas            = new LispParser("(= (f 10) (g 2))").SExprs.run().get.head
		testSuccessfulEvalSingle(addLambdas, Right((true,mapWithBoth)), mapWithBoth)
	}		

	"The Lisp Interpretter" should "succeed for defining a lambda equal to another lambda, and then invoking it" in {
		val parsed             = new LispParser("(define f (lambda (x) (+ x x)))").SExprs.run().get.head
		val evald: Either[(LispError, M), (MValue, M)]  = LispInterpretter.evaluate(parsed)(empty)	
		val mapWithF: M        = evald match { case Right((_, m)) => m }

		val parsed2             = new LispParser("(define g f)").SExprs.run().get.head
		val evald2: Either[(LispError, M), (MValue, M)]  = LispInterpretter.evaluate(parsed2)(mapWithF)	
		val mapWithBoth: M      = evald2 match { case Right((_, m)) => m }

		val parsed3             = new LispParser("(g 33)").SExprs.run().get.head
		val invoked: Either[(LispError, M), (MValue, M)] = LispInterpretter.evaluate(parsed3)(mapWithBoth)	
		val finalMap: M         = invoked match { case Right((_, m)) => m }

		testSuccessfulEvalSingle(parsed3, Right((Val(66),finalMap)), mapWithBoth)
	}

	"The Lisp Interpretter" should "succeed for when applying a value to a lambda" in {
		val parsed = new LispParser("((lambda (x) (+ 33)) 10)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((Val(33), empty)), empty)
	}

	"The Lisp Interpretter" should "succeed for when applying two values to a lambda" in {
		val parsed = new LispParser("((lambda (x y) (+ 33 x y)) 10 20)").SExprs.run().get.head
		testSuccessfulEvalSingle(parsed, Right((Val(63), empty)), empty)
	}	

	"The Lisp Interpretter" should "evaluate apply a lambda's result to a defined function" in {
		val defined   = new LispParser("(define f (lambda (x) (+ x 10)))").SExprs.run().get.head
		val evald     = LispInterpretter.evaluate(defined)(empty)
		val newMap: M = evald match { case Right((_, m)) => m }
		val res       = new LispParser("(f ((lambda (x) (+ x x)) 10))").SExprs.run().get.head
		testSuccessfulEvalSingle(res, Right((Val(30), newMap)), newMap)
	}

	def testSuccessfulEvalSingle(parsed: SExpr, 
						  	     expected: Either[(LispError, M), (MValue, M)], 
						   		 map: M): Unit = 
		assert (LispInterpretter.evaluate(parsed)(map) == expected)
}