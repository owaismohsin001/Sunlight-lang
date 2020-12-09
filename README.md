# What is Sunlight-Lang?

Sunlight-Lang is a purely functional, declarative, dynamically typed programming language. It is designed with functional programming beginners in mind, especially the ones who come from imperative/object-oriented and dynamically typed programming languages such as Python and JavaScript. it's extremely easy to transition from one of the aforementioned languages to Sunlight-Lang while learning actual functional programming concepts. Many people use a language like Scala, FSharp, and OCaml to transition from OO to functional, because they offer mutability, free side-effects, and sometimes even loops but some of those people, just continue programming imperatively, except in a functional language. This language is designed to avoid such pitfalls by not providing conveniences like the aforementioned ones while still being very beginner-friendly.

It does that by being extremely expressive like Haskell while providing intuitive constructs like dynamic dispatch based on arbitrary predicates, similar to that of Clojure. This language allows you to be concise and on the point while solving an arbitrary problem, by the use of a great many abstractions this language provides, for example even `map` in this language is an abstract and extendable function.

But enough talk, let's see some code.
```
out <- "Hello World"
```

Ok, but what about something less trivial? Like an actual function. Here come Fibonacci.

```
fib: n <- if n < 2 then 1 else (fib: n-1) + (fib: n-2)
```
Fine, what about an actual program? Maybe output the lyrics of 99 bottles of beer

```
lib "*std"

class unlessZero: n
n = 0 -> []
true -> [n] .. unlessZero: n-1

bottles: i <- s .. " bottles of beer on the wall,\\n" .. s .. " bottles of beer.\\nTake one pass it around,\\n" .. (stringify: i-1) .. " bottles of beer on the wall.\\n" where
   s <- stringify: i
end

bottle <- "1 bottle of beer on the wall,\\n1 bottle of beer.\\nTake one down, pass it around,\\nNo bottles of beer on the wall."

out <- map: (\if x = 1 then bottle else bottles: x), unlessZero: 99

```
That was oddly specific but ok. So, to learn more definetly visit [this page](https://github.com/ameerwasi001/Sunlight-lang/blob/master/Docs-Tutorial.md). Do definitely note that this project is still in it's pre-alpha stagesso don't use it in a serious and big projecct.

# Editor Support
The only editor support for now is this [VSCode syntax highlighter](https://github.com/ameerwasi001/Sunlight-Lang-VSCode) plugin, but there's definitely more to come.
