package net.parser

import org.parboiled2._

object LispParser {
	val lower: String = ('a' to 'z').toList.foldRight("")(_ + _)
	val upper: String = ('A' to 'Z').toList.foldRight("")(_ + _)
}

class LispParser(val input: ParserInput) extends Parser {

	import LispParser.{upper, lower}

	def SExpr: Rule1[AST.SExpr]       = rule { Atom | (oneOrMore(Comb) ~> (_.toList) ~> (AST.Comb(_))) }
	def Comb:  Rule1[AST.SExpr]       = rule { '(' ~ Spaces ~ SExpr ~ Spaces ~ ')' }
	def Atom:  Rule1[AST.Atom]        = rule { Num | Ident }

	def Num:   Rule1[AST.Number]      = rule { capture(Digits) ~> (_.toInt) ~> (AST.Number(_)) }
	def Ident: Rule1[AST.Ident]       = rule { capture(Alphas) ~> (_.toString) ~> (AST.Ident(_)) }
	
	def Alphas 						  = rule { oneOrMore( anyOf(lower) | anyOf(upper) ) }
	def Digits 						  = rule { oneOrMore(CharPredicate.Digit) }
	def Spaces 						  = rule { zeroOrMore(' ') }
}

// sample for learning
class MyParser(val input: ParserInput) extends Parser {
  def f = rule { capture("foo" ~ push(42)) } 
}