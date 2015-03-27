package net.parser

import org.parboiled2._

object LispParser {
	val lower: String = ('a' to 'z').toList.foldRight("")(_ + _)
	val upper: String = ('A' to 'Z').toList.foldRight("")(_ + _)
	val numbers: String = (0 to 9).toList.map(_.toString.head.toString).mkString
}

class LispParser(val input: ParserInput) extends Parser {

	import LispParser._

	// TODO: followed by end of file (un-parsed input should cause a failure!)
	def SExpr: Rule1[AST.SExpr]       = rule { Atom | '(' ~ (oneOrMore(Comb).separatedBy(Spaces) ~ ')' ~> (_.toList) ~> (AST.Comb(_))) }
	def Comb:  Rule1[AST.SExpr]       = rule {  Spaces ~ SExpr ~ Spaces }
	def Atom:  Rule1[AST.Atom]        = rule { Num | Ident }

	def Num:   Rule1[AST.Number]      = rule { capture(Digits) ~> (_.toInt) ~> (AST.Number(_)) }
	def Ident: Rule1[AST.Ident]       = rule { capture(AnyString) ~> (_.toString) ~> (AST.Ident(_)) }
	
	def AnyString 					  = rule { oneOrMore( anyOf(lower) | anyOf(upper) | anyOf(numbers)) | "+" | "-" }
	def Digits 						  = rule { oneOrMore(CharPredicate.Digit) }
	def Spaces 						  = rule { zeroOrMore(' ') }
}