This functional programming language compiles to Lua, it has the philosophy of being structured. Structured doesn't necessarily mean typed, but every piece of data must have some structure to it. This is where we put the utmost focus of our language on structs as it allows data to be organized in a very flexible way.

(Note: For installation, remember to run setup.nim or add [this file](https://github.com/ameerwasi001/Hashing-Lua/blob/main/hashLib.lua) to download repository)

# Data structures
This language has a few data structures and has tools for you to create whatever else you desire using them as the base. Following are the built-in data structures, this language offers

### String
This, as you expect holds a string of data, like `"data"`

### Numbers
Numbers are either floating-point or integers like such `1`/`2.4`

### Lists
Linked lists are the backbone of most functional programming languages. So, here we have infinite lists that allow you to string together as much data as you would like, they work like what follows
```
[1, 2, 3, 4, 5]
```

### Functions
Since this is a functional language, you have at your disposal, first-class functions. Meaning these can be passed around like values, to structs, functions, and anything else. Here's a square function
```
f: x <- x*x
```
and if this is not named then you can type it as a lambda function
```
\x -> x*x
```
and for lambda function that takes one parameter then there's a shorthand for it, where an argument named `x` is implicitly taken
```
\x*x
```

# Declarations
A program is composed entirely of declarations like
```
x <- 2
```
and declarations can also be functions
```
id: x <- x
```
# Comments
So it's easy to comment on anything in your code by using Lua like `--`. This is an example,
```
-- Hey this is a comment
```
Although, unlike some other languages, comments cannot be a full parseable program. 

# Types
Types are either structs or multiple structs that are grouped under the same structure. A struct can be declared as such
```
type Cirle <- {r}
```
and a sum of these can be declared as such
```
type Shape <- Circle{r} | Triangle{a, b, c} | Square{e}
```
Now a shape can be either a circle, a triangle, or a square. 

## Instantiation of a type
The way to instantiate these types is to use record/type syntax
```
out <- Square{e :: 5}
```
or
```
out <- Triangle{a :: 5, b :: 3, c :: 8}
```
That definition of types also defines functions named `square`, `circle`, and `triangle`(these are entirely lowercased), all of which are just functions that take the number of arguments their respective type takes and return an instance of their respective types. For example,
```
out <- square: 5
```
or
```
out <- triangle: 5, 3, 8
```

The benefit of using functions instead of the actual type syntax is that these functions are curried, unlike that aforementioned record syntax.
## Checking a type
To check a type you can simply type a variable's name by saying `a@Circle` where RHS of `@` is a type.

# MultiMethods
There are two types of multi-methods in Sunlight-lang, opened and closed.

## Closed methods
A closed method cannot be extended after they are declared, an example of a closed method would be
```
class name: p
p@Pet -> p.name .. p.species
p@Human -> p.name
```
Now you can't say
```
id: x <- x
p@Wild -> p.name .. p.forest
```

## Open methods
Now open methods can be extended to have new cases anywhere from your program. Here's an example
```
open fst: stct
fst ? stct@SltTuple -> f where (f, s) = stct
```
Now we can have
```
id: x -> x
fst ? stct@SltList -> head: stct
```
# Application
Function application is really simple, all you gotta do is,
```
add: 1, 2
```
where `add` is a function, `1` and `2` are arguments and this entire thing is an application. Now, there are other ways to apply functions like the forward pipe
```
out <- [1, 2, 3, 4] |> map: \x -> x*2 |> filter: \x -> x /= 2
```
or the backward pipe, which comes in handy quite often
```
out <-  filter: \x -> x /= 2 <| map: \x -> x*2 <| [1, 2, 3, 4]
```
both of which output 
```
[4, 6, 8]
```
## Infix calls
The aforementioned `add` function can also be used like
```
1 add 2
```
which would yield exactly what you expect, `3`. By default these are right-associative, for example, this would result in
```
sub: a, b <- a-b
5 sub 2 sub 1
```
and result in `4` but you can make it left-associative by instead making the call as
```
5 sub' 2 sub' 1
```
which would as expected return `2`.
# Optional strictness
By default all values in Sunlight are lazy but you can have strict(they don't get full immediately evaluated, but they do get evaluated as soon as) values by creating strict types, these will necessarily be memoized by all functions, they may or may not be wrappers
```
type SMaybe <- !Just{a} | !Nothing
```
This can both positively and negatively impact the performance of values of certain types since memoization can be both bad and good.
# Multiple files
There are ways to use multiple files in a project when using Sunlight-lang. This is the way to include the standard library and use defined libraries, and it is
```
include "something.slt"
... actual code ...
```
It will include that file in your code now, but if you are looking for more of a module, then consider adding
```
mod someModule
... actual code ...
end
```
In this way all of your variables will be locked up in `someModule` and to access, you must say `someModule::varName`. This is true even when you are coding inside a module but for brevity's sake, you can just say `$varName` where `$` sort of means `this` but for modules.

## Libraries
As you might have seen, you cannot nest `mods` into each other with the `mod` keyword and that is because you are not supposed to, if you have a problem that requires an additional set of files then you should use the `lib` keyword. In order to create a library, you must create a folder(ie. std) put all your library files in it, and then create a `main` file in this aforementioned folder. This `main` file should include all other files of the folder(that you want this library to include), then specify this folder's name in your root's `main` file, like such `lib "*std"`.

That's about it, for the actual language because most of the properties of this language come from its standard library

# Standard Library
Avalible std modules currently are
`errors.slt`,
`traversable.slt`,
`access.slt`,
`monads.slt`

# Error Handling
(From `errors.slt`)
Most programming languages have some sort of exception handling mechanism built-in but other programming languages like Sunlight-lang are expressive enough to define these in the standard library. So, enter `Maybe`, Here's a simple demonstration of it.
```
class div: a b
b = 0 -> None
true -> some: a/b
```
This works fine when there's only one way something could possibly have failed but when that's not true, you need `Either`, like this
```
class makeCar: c, m
m <= 0 -> left: "Model of a ar must be positive"
c = right: "Land Cruiser" -> car: c, m
c = right: "Mistubishi" -> car: c, m
c = right: "Honda" -> car: c, m
true -> left: "No company named " .. c .. "exists"
```
Here you'll get the error in form of either one thing or another.

# Lenses
(From `access.slt`)
## Access
If you want to access something from a data structure then you should use access, which has its syntactic sugar
```
a <- [0, 8, 6, 4, 5][2]
```
which yields eight because by default indexing starts from one in Sunlight-lang.
Sometimes you want more than access to a single element, and for those times we have `glance` where you can just glance at several of the elements that don't satisfy a given predicate. For example,
```
out <- glance: \x < 3, 1, [0, 9, 4, 6, 8, 1]
```
and with both of those combined, you can use `view` which is just `access` but with better pipe support and no syntactic sugar.
```
out <- [[1, 2], [2, 5], [9, 3]] |> view: 3 |> view: 2
```
## Update
If you want to update something in accordance with its index then your best bet is to change to use `change` like this
```
out <- change: [3, 5, 55, 8], 1, \x -> x*2
```
which returns `[6, 5, 55, 8]` and if you want to chain these you should say
```
out <- \f -> change: [[1, 2], [2, 5], [9, 3]], 2, f <| \f, x -> change: x, 1, f <| \x*2
```
which in turn returns `[[1, 2], [4, 5], [9, 3]]`. If you instead want to change a bunch of elements that dissatisfy a predicate then you should use `unedit` and write
```
out <- out <- unedit: [3, 5, 55, 8], 1, \x < 3, \x*2
```
and these can be chained with continuations the same way that the other one can, as `change` is merely a specification of `unedit`.

## Making your own lenses
If you define your own data structures that can be "indexed", whatever indexing in the context of your data structures might mean. In order for you to do this, you must extend the methods `unedit`, `glance` and optionally, although very much preferably `access`. In this way, all the aforementioned syntax, functions, and methods can be used for your data structures.

# Data structure manipulation
(From `traversable.slt`)
There are many ways to manipulate data structures, you can map/filter over them, take from them, and even fold them to some value. For mapping and filtering, you can use `every` keyword like this
```
every [1, 2, 3, 4, 5] is x+1
```
and filtering over them using the same syntax will also be very much possible when you say
```
every [1, 2, 3, 4, 5] is x+1 if x /= 2
```
returning `[2, 4, 5, 6]`. There's a way to map and filter without actually using this syntax for the times it seems verbose, and to use this you should use `map` and `filter`. Reducing is done by the `fold` function like this
```
fold: \a, b -> a*b, 1, [1, 2, 3, 4, 5]
```
You can also take some data from some structure using the `take` function like this
```
inf: n <- [n] .. inf: n+1
out <- take: 10, inf: 1
```
which returns `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`.

In statically typed functional languages like Haskell, there's a `fmap` for modifying values inside data structures and we do the same here by using `Maybe`, `Either`, and potentially for your own data type.

## Instantiation of these functions
All of these functions are `open` and can be extended except for `map` and `filter` which are mere specifications of `map_and_filter`. So, in order for you to use these functions for your own data structures, just define these by the use of the aforementioned open method syntax.

# Sequencing
(From `monads.slt`)

## Bind
There's sequencing in the standard library, it lets you chain certain evaluations/actions one after another with intermediate actions while creating. This is done by using bind, like this
```
out <- Some{a :: [11, 5, 0, 4]} bind \a -> a[2] bind \b -> some: a .. [b*2]
```
which returns `Some{a :: [11, 5, 0, 4, 10]}`. You can quite obviously define your own types that work like, for instance `Maybe` is defined like this
```
bind ? s@Some -> f: s.a
bind ? s@None -> None
```
## Unit
The simplest/default value/constructor of a type can be defined using the `unit` function. For types like Maybe, the default is simply
```
unit ? r@Maybe -> some: s
```
Here `r` is just the type you are given by the caller and `s` is the value given by the user and you put `s` in the minimal context of `r`. Here's an example of the usage of this type
```
ls <- [9, 8, 7, 6, 5, 4, 3, 2, 1]
out <- (unit: 1, &Maybe) bind \n -> ls[3] bind \e -> unit: n+e, &Maybe
```

# Strict type
(From library `strict`)

Strict type is in the "standard library"(as in, it's there for you when you install this language) but it needs to be imported like such
```
lib "*strict"
```
which defines a module namely `Strict` there exists a type `Strict` and it is a functor, applicative, and a monad. You can use it like such
```
lib "*strict"

fib: n <- if n.a < 2 then Strict::strict: 1 else (\a, b -> a+b) fmap' (fib: fmap: \x-1, n) seq' (fib: fmap: \x-2, n)
out <- fib: Strict::Strict{a :: 100}
```

# Thank You
Thanks for reading through, hope you enjoy playing around Sunlight-Lang. Since this language is still in its pre-alpha stage, make sure that you report bugs if you find them.
