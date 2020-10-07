# Sunlight-lang
This functional progamming language compiles to lua, it has thge philosophy of being structured. Structured doesn't nessacarily mean typed, but every piece of data must have some structure to it. This is we put utmost focus of our language on structs which allow data to be organized in a very flexible way.

# Data structues
This language has a few data and has tools for you to create whatever else you desire using them as the base. Following are the built-in data structures this language has

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
