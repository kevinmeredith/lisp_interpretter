package net.parser

import org.scalatest._
import net.parser.AST._
import scala.util.Success

class ParserTest extends FlatSpec {

	"The Lisp Parser" should "parse an Atom" in {
		val result = new LispParser("1234").SExpr.run() 
		assert( result == Success(Number(1234)) )
	}

	"The Lisp Parser" should "parse an Ident" in {
		val result = new LispParser("foobar").SExpr.run() 
		assert( result == Success(Ident("foobar")) )
	}	

	"The Lisp Parser" should "parse a nested Atom" in {
		val result = new LispParser("(5555)").SExpr.run() 
		assert( result == Success(Comb(List(Number(5555)))) )
	}		

	"The Lisp Parser" should "parse a nested Ident" in {
		val result = new LispParser("(bippy)").SExpr.run() 
		assert( result == Success(Comb(List(Ident("bippy")))) )
	}			

	"The Lisp Parser" should "parse a 2-parenthesized Ident" in {
		val result = new LispParser("((  BOOYAH ))").SExpr.run() 
		assert( result == Success(Comb(List(Comb(List(Ident("BOOYAH")))))) )
	}				

	"The Lisp Parser" should "simple S Expression" in {
		val result = new LispParser("(bar (foo) 3 5 874)").SExpr.run() 
		assert(result == Success(Comb(List(Ident("bar"), Comb(List(Ident("foo"))), Number(3), Number(5), Number(874)))))
	}				

	"The Lisp Parser" should "somewhat complex S Expression" in {
		val result = new LispParser("(((lambda x (lambda y (plus x y))) 3) 5)").SExpr.run() 
		assert(result == Success(Comb(List(Comb(List(Comb(List(Ident("lambda"), Ident("x"), Comb(List(Ident("lambda"), Ident("y"), Comb(List(Ident("plus"), Ident("x"), Ident("y"))))))), Number(3))), Number(5)))))
	}				

	"The Lisp Parser" should "parse an SExpression with a lot of spaces" in {
		val result = new LispParser("( lots of ( spaces in ) this ( one ) )").SExpr.run() 
		assert(result == Success(Comb(List(Ident("lots"), Ident("of"), Comb(List(Ident("spaces"), Ident("in"))), Ident("this"), Comb(List(Ident("one")))))))
	}				

	"The Lisp Parser" should "parse a simple SExpression #2" in {
		val result = new LispParser("(+ x (+ y 1))").SExpr.run()
		assert(result == Success(Comb(List(Ident("+"), Ident("x"), Comb(List(Ident("+"), Ident("y"), Number(1)))))))
	}	

	"The Lisp Parser" should "parse a simple if expression" in {
		val result = new LispParser("(if (> 10 20) (+ 1 1) (+ 3 3))").SExpr.run()
		assert(result == Success(Comb(List(Ident("if"), Comb(List(Ident(">"), Number(10), Number(20))), Comb(List(Ident("+"), Number(1), Number(1))), Comb(List(Ident("+"), Number(3), Number(3)))))))
	}
}