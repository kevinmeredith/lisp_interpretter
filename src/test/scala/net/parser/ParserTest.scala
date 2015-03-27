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

}