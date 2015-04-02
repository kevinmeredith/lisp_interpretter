package net.parser

import scala.annotation.tailrec
import net.common.Error._

object AST {

	sealed trait SExpr
	sealed trait Atom extends SExpr
	case class Comb(sExprs: List[SExpr]) extends SExpr {
		override def toString = "(" + printSExprs(sExprs) + ")"
	}

	case class Number(x: Int) extends Atom {
		override def toString = x.toString
	}
	case class Ident(x: String) extends Atom {
		override def toString = x
	}

	def printSExprs(xs: List[SExpr]): String = 
		xs.foldLeft("") {
			(acc: String, elem: SExpr) => {
				acc match {
					case "" => elem.toString
					case _  => acc + " " + elem.toString
				}
			}
		}

	type M = Map[String, Any]

	sealed trait EvalResult
	case class Complete(res: Either[(LispError, M), (Any, M)]) extends EvalResult
	case class Partial(res: List[Any] => M => EvalResult) extends EvalResult

	implicit def eitherResultToComplete(x: Either[(LispError, M), (Any, M)]): EvalResult = 
		Complete(x)

	sealed trait DefineOp
	case object Op extends DefineOp	
	case object Lambda extends DefineOp
}