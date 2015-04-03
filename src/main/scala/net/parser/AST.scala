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
		override def toString = x.toString // TODO: use `show`
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

	sealed trait MValue
	sealed trait SingleValue extends MValue
	case class Val(x: Any) extends SingleValue
	case class Fn(f: Function2[List[Any], M, EvalResult]) extends MValue

	type M = Map[String, MValue]

	sealed trait EvalResult
	case class Complete(res: Either[(LispError, M), (SingleValue, M)]) extends EvalResult
	case class Partial(f: Function2[List[Any], M, EvalResult]) extends EvalResult

	implicit def eitherResultToComplete(x: Either[(LispError, M), (SingleValue, M)]): EvalResult = 
		Complete(x)

	sealed trait MapUpdateOp 
	case class SetOp(v: String, value: Any) extends MapUpdateOp

	sealed trait DefineOp extends MapUpdateOp
	case object Op extends DefineOp	
	case object Lambda extends DefineOp
}