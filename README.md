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

* add `lambda` 
* show actual text when user types it in the REPL