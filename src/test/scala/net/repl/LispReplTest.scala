package net.repl

import net.repl.LispRepl._
import org.scalatest._
import net.parser.AST._
import net.common.Error._

class LispReplTest extends FlatSpec {
	
	"Evaluating 2 SExpressions" should "succeed when defining a lambda and appying it" in {
		val str = "(define double (lambda (x) (+ x x))) (double 10)"
		val result: Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = evalString(str)
		result match {
			case Right(x :: y :: Nil) => assert(getRightValue(x) == Some(Val(Lambda)) && getRightValue(y) == Some(Val(20)))
			case _                    => assert(false)
		}
	}

	"Evaluating 3 SExpressions" should "succeed where each SExpr is simply a Number" in {
		val str = "10 20 30"
		val result: Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = evalString(str)
		result match {
			case Right(x :: y :: z :: Nil) => assert(getRightValue(x) == Some(Val(10)) && getRightValue(y) == Some(Val(20)) && getRightValue(z) == Some(Val(30)))
			case _                         => assert(false)
		}
	}	

	"Evaluating 3 SExpressions" should "succeed where the first 2 are define's, but the third adds them" in {
		val str = "(define x 10) (define y 20) (+ x y)"
		val result: Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = evalString(str)
		result match {
			case Right(x :: y :: z :: Nil) => assert(getRightValue(x) == Some(Val(Op)) && getRightValue(y) == Some(Val(Op)) && getRightValue(z) == Some(Val(30)))
			case _                         => assert(false)
		}
	}		

	"Evaluating an SExpression" should "should return a Parse Error where the first fails to parse" in {
		val str = "(define x 10"
		val result: Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = evalString(str)
		result match {
			case Left(ParseError(_)) => assert(true)
			case _                   => assert(false)
		}
	}		

	"Evaluating 2 SExpression's" should "parse successfully, evaluate the first, but fail to evaluate the second" in {
		val str = "1000 (+ x 1)"
		val result: Either[LispError, List[Either[(LispError, M), (MValue, M)]]] = evalString(str)
		result match {
			case Right(x :: Left((NoVarExists("x"), _)) :: Nil) => assert(getRightValue(x) == Some(Val(1000))) 
			case _                                         => assert(false)
		}
	}		

	private def getRightValue[A,B,C,D](x: Either[(A, C), (B, D)]): Option[B] = x match {
		case Right((x,_)) => Some(x)
		case Left(_)      => None
	} 

}