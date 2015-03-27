# ENBF (Grammar used to construct the S-Expression DSL)

> Credit: Prof. Yorgey at UPenn
> http://www.cis.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

## Examples
For example, the following are all valid S-expressions:
5
foo3
(bar (foo) 3 5 874)
(((lambda x (lambda y (plus x y))) 3) 5)
( lots of ( spaces in ) this ( one ) )