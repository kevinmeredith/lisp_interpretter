object AST {
	
	sealed trait SExpr
	sealed trait Atom extends SExpr
	case class Comb(sExprs: List[SExpr]) extends SExpr

	case class Number(x: Int) extends Atom
	case class Ident(x: String) extends Atom
}