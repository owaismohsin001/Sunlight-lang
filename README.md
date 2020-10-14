This functional progamming language compiles to lua, it has thge philosophy of being structured. Structured doesn't nessacarily mean typed, but every piece of data must have some structure to it. This is where we put utmost focus of our language on structs as it allow data to be organized in a very flexible way.

# Data structues
This language has a few data structures and has tools for you to create whatever else you desire using them as the base. Following are the built-in data structures, this language offers

### String
This, as you expect holds a string of data, like `"data"`

### Numbers
Numbers are either floating point or integers like such `1`/`2.4`

### Lists
Linked lists are the backbone of most functional programming languages. So, here we have infinite lists that allow you to string together as much data as you would like, they work like what followd
```
[1, 2, 3, 4, 5]
```

### Functions
Since this is a functional language, you have at your disposal, first class functions. Meaning these can be passed around like values, to structs, functions, and anything else. Here's a square function
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

# Types
Types are either structs or multiple structs that are grouped under the same same structure. A struct can be declared as such
```
type Cirle <- {r}
```
and a sum of these an be declared as such
```
type Shape <- Circle{r} | Triangle{a, b, c} | Square{e}
```
Now a shape can be either a cirle, a triangle, or a square. 

## Instantiation of a type
The way to instantiate these types is to use record/type syntax
```
out <- Square{e :: 5}
```
or
```
out <- Triangle{a :: 5, b :: 3, c :: 8}
```
Those definitions of types also defines functions named `square`, `circle`, and `triangle`(these are entirely lowercased), all of which are just functions that take the number of arguments their respective type takes and return an instance of their respective types. For examle,
```
out <- square: 5
```
or
```
out <- triangle: 5, 3, 8
```

The benefit of using functions instead of the actual type syntax is that these functions are curried unlike that aforementioned record syntax.
## Checking a type
To chack a type you can simply type variable's name by saying `a@Circle` where rhs of `@` is a type.

# MultiMethods
There are two types of multimethods in Sunlight-lang, opened an closed.

## Closed methods
Closed method cannot be extended after they are declared, an example of a closed method would be
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
Now open methods can be extended to have new ases anywhere from your program. Here's an example
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
where `add` is a function, `1` and `2` are aruments and this entire thing is an application. Now, there are other ways to aply functions like the forward pipe
```
out <- [1, 2, 3, 4] |> map: \x -> x*2 |> filter: \x -> x /= 2
```
or the backwards pipe, which comes in handy quite often
```
out <-  filter: \x -> x /= 2 <| map: \x -> x*2 <| [1, 2, 3, 4]
```
both of which ouput 
```
[4, 6, 8]
```
## Infix calls
The aforemention `add` function can also be used like
```
1 `add` 2
```
which would yeild exactly what you expet, `3`.

# Multiple files
There are ways to use multiple files in a project, when using Sunlight-lang. This is the way to include standard library and use defined libraries, and it is
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
In this way all of your variables will be locked up in `someModule` and to access, you must say `someModule::varName`. This is true even when you are coding inside a module. Although more conviniences will show up for modules in the near future.

That's about it, for the actual language because most of the properties of this language come from it's standard library

# Standard Library
Avalible std modules currently are
`errors.slt`,
`traversable.slt`,
`access.slt`

# Error Handling
(From `errors.slt`)
Most programming languages have some sort of exception handling mechanism built-in but other programming languages like Sunlight-lang are expressive enough to define these in the standard library. So, enter `Maybe`, Here's a simple demonstation of it.
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
If you want to access something from a datastructure then you should use access, which has it's syntactic sugar
```
a <- [0, 8, 6, 4, 5][2]
```
which yeilds eight because by default indexing starts from one in Sunlight-lang.
Sometimes you want more than access to a single element, and for those times we have `glance` where you can just glance at several of the elements that doesn't satisfy a given predicate. For example,
```
out <- glance: \x < 3, 1, [0, 9, 4, 6, 8, 1]
```
and with both of those combined you can use `view` which is just `access` but with better pipe support and no syntactic suagr.
```
out <- [[1, 2], [2, 5], [9, 3]] |> view: 3 |> view: 2
```
## Update
If you want to update something in accordance with it's index then your best bet is to change to use `change` like this
```
out <- change: [3, 5, 55, 8], 1, \x -> x*2
```
which returns `[6, 5, 55, 8]` and if you want to chain these you should say
```
out <- \f -> change: [[1, 2], [2, 5], [9, 3]], 2, f <| \f, x -> change: x, 1, f <| \x*2
```
which in turn returns `[[1, 2], [4, 5], [9, 3]]`. If you instead want to change a bunch of elemnts that disatisfy a predicate then you should use `unedit` and write
```
out <- out <- unedit: [3, 5, 55, 8], 1, \x < 3, \x*2
```
and these can be chained with continuations the same way that the other one can, as `change` is merely a specification of `unedit`.

## Making your own lenses
If you define your own data structures that can be "indexed", whatever inexing in the context of your data structures might mean. In order for you to do this, you must extend the methods `unedit`, `glance` and optionally, although very much preferably `access`. In this way, all the aforementioned syntax, functions and methods can be used for your data structurs.

# Data structure manipulation
(From `traversable.slt`)
There are many ways to manipulation data structures, you can map/filter over them, take from them, and even fold them to some value. For mapping and filtering, you can use `every` keyword like this
```
every [1, 2, 3, 4, 5] is x+1
```
and filtering over them using the same syntax will also be very much possible when you say
```
every [1, 2, 3, 4, 5] is x+1 if x /= 2
```
returning `[2, 4, 5, 6]`. There's a way to map anf filter without actually using this syntax for the times it seems verbose, and to use this you should use `map` and `filter`. Reducing is done by the `fold` function like this
```
fold: \a, b -> a*b, 1, [1, 2, 3, 4, 5]
```
You can also take some data from some structure using the `take` function like this
```
inf: n <- [n] .. inf: n+1
out <- take: 10, inf: 1
```
which returns `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`.

In statically typed functional languages like Haskell, there's a `fmap` for modifying valuse inside data structures, we just use map here to do the same, with `Maybe` and `Either` and potentially for your own data type.

## Instansiation of these functions
All of these functions are `open` and can be extened except for `map` and `filter` which are mere specifications of `map_and_filter`. So, in order for you to use these functions for your own data structures, just define these by the use fo aforementioned open method syntax.

# Thank You
Thanks for reading through, hope you enjoy playing around Sunlight-Lang. Since this language is still in it's pre-alpha stage, make sure that you report bugs if you find them.
