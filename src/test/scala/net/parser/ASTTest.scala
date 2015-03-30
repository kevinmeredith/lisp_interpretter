package net.parser

import org.scalatest._
import net.parser.AST._

class ASTTest extends FlatSpec {

	"Calling toString on an Ident" should "return the String only" in {
		assert( Ident("1234").toString == "1234")
	}

	"Calling toString on a Number" should "return the Number only" in {
		assert( Number(5555).toString == "5555")
	}	

	"Calling toString on a Comb" should "add nested parentheses" in {
		assert( Comb(List(Ident("+"), Number(1), Number(2))).toString ==
			"(+ 1 2)" )
	}		

	"Calling toString on two nested Comb's" should "add nested parentheses" in {
		assert( Comb(List(Ident("+"), Comb(List(Ident("+"), Number(5), Number(10))), Number(20))).toString ==
			"(+ (+ 5 10) 20)")
	}			
}