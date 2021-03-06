package net.parser

import org.scalatest._
import net.parser.AST._
import scala.util.{Success, Failure, Try}

class ParserTest extends FlatSpec {

	"The Lisp Parser" should "parse an Atom" in {
		val result = new LispParser("1234").SExprs.run() 
		assert( result == Success(Vector(Number(1234)) ))
	}

	"The Lisp Parser" should "parse an Ident" in {
		val result = new LispParser("foobar").SExprs.run() 
		assert( result == Success(Vector(Ident("foobar")) ))
	}	

	"The Lisp Parser" should "parse a nested Atom" in {
		val result = new LispParser("(5555)").SExprs.run() 
		assert( result == Success(Vector(Comb(List(Number(5555)))) ))
	}			

	"The Lisp Parser" should "parse a nested Ident" in {
		val result = new LispParser("(bippy)").SExprs.run() 
		assert( result == Success(Vector(Comb(List(Ident("bippy")))) ))
	}			

	"The Lisp Parser" should "fail on a doubley parenthesis Ident" in {
		val result = new LispParser("((  BOOYAH ))").SExprs.run() 
		assert( result == Success(Vector(Comb(List(Comb(List(Ident("BOOYAH")))))) ))
	}				

	"The Lisp Parser" should "simple S Expression" in {
		val result = new LispParser("(bar (foo) 3 5 874)").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Ident("bar"), Comb(List(Ident("foo"))), Number(3), Number(5), Number(874))))))
	}				

	"The Lisp Parser" should "somewhat complex S Expression" in {
		val result = new LispParser("(((lambda x (lambda y (plus x y))) 3) 5)").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Comb(List(Comb(List(Ident("lambda"), Ident("x"), Comb(List(Ident("lambda"), Ident("y"), Comb(List(Ident("plus"), Ident("x"), Ident("y"))))))), Number(3))), Number(5))))))
	}				

	"The Lisp Parser" should "parse an SExpression with a lot of spaces" in {
		val result = new LispParser("( lots of ( spaces in ) this ( one ) )").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Ident("lots"), Ident("of"), Comb(List(Ident("spaces"), Ident("in"))), Ident("this"), Comb(List(Ident("one"))))))))
	}				

	"The Lisp Parser" should "parse a simple SExpression #2" in {
		val result = new LispParser("(+ x (+ y 1))").SExprs.run()
		assert(result == Success(Vector(Comb(List(Ident("+"), Ident("x"), Comb(List(Ident("+"), Ident("y"), Number(1))))))))
	}	

	"The Lisp Parser" should "parse a simple if expression" in {
		val result = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExprs.run()
		assert(result == Success(Vector(Comb(List(Ident("if"), Comb(List(Ident(">"), Number(10), Number(20))), Comb(List(Ident("+"), Number(1), Number(1))), Comb(List(Ident("+"), Number(3), Number(3))))))))
	}

	"The Lisp Parser" should "fail if the SExpression has unmatched input" in {
		val result = new LispParser("(bippy) ......").SExprs.run() 
		assertBadParse(result)
	}	

	"The Lisp Parser" should "succeed for a valid SExpresison with extra trailing whitespace." in {
		val result = new LispParser("(bippy) ").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Ident(("bippy")))))))
	}		

	"The Lisp Parser" should "succeed for a valid 'set!' SExpression" in {
		val result = new LispParser("(set! x 100)").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Ident("set!"), Ident("x"), Number(100))))))
	}			

	"The Lisp Parser" should "fail for an invalid 'set!' SExpression using a string literal \"set!\"" in {
		val result = new LispParser("(\"set!\" x 100)").SExprs.run() 
		assertBadParse(result)
	}				

	"The Lisp Parser" should "succeed for a non-simple set! SExpression" in {
		val result = new LispParser("(set! x (+ (+ 0 100) 1 2))").SExprs.run() 
		assert(result == 
			Success(Vector(Comb(List(Ident("set!"), Ident("x"), Comb(List(Ident("+"), Comb(List(Ident("+"), Number(0), Number(100))), Number(1), Number(2)))))))
			)
	}					

	"The Lisp Parser" should "succeed for SExpr's separated by white-space." in {
		val result = new LispParser("(bippy)      zzzzz ").SExprs.run() 
		assert(result == Success(Vector(Comb(List(Ident("bippy"))), Ident("zzzzz"))))
	}		

	def assertBadParse[A](result: Try[A]) = result match {
		case Failure(_)         => true
		case Success(Vector(_))	=> false
	}		
}