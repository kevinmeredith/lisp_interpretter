# Lisp Interpretter

## Overview

This code implements a parser, interpretter, and REPL for a 
simple [norvig.com-inspired](http://norvig.com/lispy.html) Lisp.

It uses [parboiled2](parboiled2.org) for parsing. Upon a successful parse, an
`net.parser.AST.SExpr` instance gets returned.

Lastly, `net.interpretter.LispParser#evaluate`'s return type is `Either[(InterpretterError, M), (Any, M)]`.

In short, the 

## Run the REPL

```
sbt console
```

And then run the following to kick off the REPL:


```
scala> net.repl.LispRepl.runForever(Map())
res4: scalaz.effect.IO[Unit] = scalaz.effect.IOFunctions$$anon$6@2ae85ad9
 
scala> res4.unsafePerformIO
>(+1 2)
Right((3,Map()))
>
Right((3,Map()))
>4
Right((4,Map()))
>5
Right((5,Map()))
>100
Right((100,Map()))
>"foobar"
Right(("foobar",Map()))
>ff
Left((NoVarExists,Map()))
>((1))
Left((ProcError,Map()))
>(define x 100)
Right(((),Map(x -> 100)))
>x
Right((100,Map(x -> 100)))
>(define y 100)
Right(((),Map(x -> 100, y -> 100)))
>(> 100 9 8 7)
Right((true,Map(x -> 100, y -> 100)))
>(> x y)
Right((false,Map(x -> 100, y -> 100)))
>(+ x y )
Right((200,Map(x -> 100, y -> 100)))
>(+ 100 200) (define y 10)
Left((ParseError(ParseError(Position(12,1,13), Position(12,1,13), <2 traces>)),Map(x -> 100, y -> 100)))
```

## Testing

There are two [Scalatest](http://www.scalatest.org/) tests:

* net.parser.ParserTest
* net.interpretter.LispInterpretterTest

## TODO

* add `lambda` (**In progress**)
* show actual text when user types it in the REPL

### Initial Lambda Work

**Lambda working**

```
scala> new net.parser.LispParser("(lambda (x) (+ x 10))").SExprComplete.run()
res4: scala.util.Try[net.parser.AST.SExpr] = Success((lambda (x) (+ x 10)))

scala> net.interpretter.LispInterpretter.evaluate(res4.get)(Map())
res5: net.parser.AST.EvalResult = Partial(<function1>)

scala> val lambdaFn = res5 match { case Partial(f) => f }
<console>:12: warning: match may not be exhaustive.
It would fail on the following input: Complete(_)
       val lambdaFn = res5 match { case Partial(f) => f }
                      ^
lambdaFn: List[Any] => net.parser.AST.EvalResult = <function1>

scala> lambdaFn(List(10))
res6: net.parser.AST.EvalResult = Complete(Right((20,Map())))

scala> lambdaFn(List(10, 20))
res7: net.parser.AST.EvalResult = Complete(Left((WrongNumArgs(List(x),List(10, 20)),Map())))
```

**Issue with `define`**

It appears that the `define f <lambda>` succeeds at first. But,
attempting to execute the lambda via `f 10` fails with a *ProcError*.

```
scala> new net.parser.LispParser("(define f (lambda (x) (+ x 2)))").SExprComplete.run()
res9: scala.util.Try[net.parser.AST.SExpr] = Success((define f (lambda (x) (+ x 2))))

scala> net.interpretter.LispInterpretter.evaluate(res9.get)(Map())
res10: net.parser.AST.EvalResult = Complete(Right((Lambda,Map(f -> <function1>))))

scala> val newMap = res10 match { case Complete(Right((_, m))) => m }
<console>:12: warning: match may not be exhaustive.
It would fail on the following inputs: Complete(Left(_)), Partial(_)
       val newMap = res10 match { case Complete(Right((_, m))) => m }
                    ^
newMap: net.parser.AST.M = Map(f -> <function1>)

scala> new net.parser.LispParser("(f 10)").SExprComplete.run()
res11: scala.util.Try[net.parser.AST.SExpr] = Success((f 10))

scala> net.interpretter.LispInterpretter(res11.get)(newMap)
<console>:15: error: net.interpretter.LispInterpretter.type does not take parameters
              net.interpretter.LispInterpretter(res11.get)(newMap)
                                               ^

scala> net.interpretter.LispInterpretter.evaluate(res11.get)(newMap)
res13: net.parser.AST.EvalResult = Complete(Left((ProcError,Map(f -> <function1>))))
```