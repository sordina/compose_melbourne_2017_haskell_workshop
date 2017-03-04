
----

# Introduction

<div class="important">

The following exercises are intended to be used to warm up your fingers, rather than
your brain. These should be run through quickly to get you used to using your
development environment.

The majority of your workflow should be performed by writing code in an editor,
and testing it in GHCi. You will write the definitions in the editor and
test them with simple expressions in GHCi.

</div>

## Lexicon

-----------       -------------     ------------
-----------       -------------     ------------
Primitives        Prelude           Variables
Literals          `let`             Definitions
String            Tuples            Functions
Invocation        Lists             Infix
Cons              (:)               []
Destructuring     Pattern-Matching  head
Partial           length            map
Expressiveness

## Primitives

Haskell comes pre-packaged with many primitives available in the `Prelude` module
that is included by default. You don't need to know them all right now,
but you should at least make yourself familiar with
the following types, and literal syntax:

-------------  ------------ --------------
Type           What?        Literal Syntax
-------------  ------------ --------------
Int            Machine Ints 42

String, [Char] Strings      "Hello World"

Bool           Booleans     True, False
-------------  ------------ --------------

```real
You can type any literal directly into GHCi in order to have it echoed
right back at you. This may be useful for sanity checking that you have
the syntax right!

ghci> 42
42
```

## Variables

In Haskell you can define a variable with the `=` sign.

Variables can be defined at the top-level (no-indentation):

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myVariable = 2
~~~

Variable names should start with a lowercase letter and contain no spaces, or
special characters, besides underscores, numbers, and `'`.

<!--
```real
If you wish to define a variable inside GHCi, you have to prefix
the definition with "let"... For example:

[Prelude] > let myName = "Simon"
```

(Not anymore!!)
-->

Some examples of variable names are:

* `a`
* `my_name`
* `data43'`

```instruction
Define your own variable.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
x = "hello"
~~~

```instruction
What is an example of an invalid variable name?
```

~~~{ data-language=haskell .answer .nocheck }
InvalidVariable = 123
~~~

String literals look familiar:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myString = "hello world"
~~~

```instruction
Define a variable containing a string.
```

## Tuples

Tuples look like this:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myTuplePair = (1,"hello")

myTupleTrio = (1,"hello",3)
~~~

They can be used to group multiple, differently-typed (heterogeneous) values.

```instruction
Define a variable containing a tuple.
```

## Branching

In order to decide to make a decision in Haskell you can branch in many different
ways. Two simple ways are with `if` statements, and `case` statements.

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
sunny = True

umbrellasToBring =
  if sunny
    then 1
    else 0

kindOfBagRequired =
  case umbrellasToBring
    of 0 -> "satchel"
       1 -> "backpack"
       x -> "duffle"
~~~

Another way to perform branching is with `pattern-matching` which we will discuss
shortly.

It's worth noting that Haskell is white-space sensitive. In general, you can
add a new-line and indent in-order to continue an expression, however there are
several gotchas. See the [indentation page on the wiki](https://en.wikibooks.org/wiki/Haskell/Indentation)
for more information.

## Functions

Functions are a core part of Haskell. Function definition and invocation look like this:

~~~{data-language=haskell .nocheck} 
-- Definition:
myFunction x y ... = ...

-- Invocation:
... myFunction 1 2 ...
~~~

This is different to what you might be familiar from a c-familiy language such
as Javascript:

~~~{data-language=javascript .nocheck} 
// Definition:
function javascriptFunction(a,b ...) { ... }

// Invocation:
javascriptFunction(1,2)
~~~

For example:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myAdd x y = x + y
~~~

`myAdd` takes two numbers and returns the result of the addition of those two numbers.

```note
If you wish to use another function application as an
argument, then you will usually have to enclose it in
a pair of parentheses.

For example:

Prelude> myAdd (myAdd 5 4) 3
12
```


```instruction
Define a function `myMultiply` that multiplies 3 numbers.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myMultiply x y z = x * y * z
~~~

```instruction
Use your `myMultiply` function to multiply `4`, `5` and `6734`.
```

~~~{data-language=haskell .answer }
Prelude> myMultiply 4 5 6734
~~~

```note
There is also an inline-function lambda-syntax:

	\{args...} -> {value}

Here is an example of an "addOne" function:

	Prelude> (\x -> x + 1) 2
	3
```

## Pattern Matching

One fairly unique feature of ML family languages like Haskell is a dedicated
syntax for "pattern-matching". Pattern matching is the practice of deconstructing
and dispatching on a value at the time of assignment. Deconstruction and
dispatching are orthogonal features, but they are often used together.
Deconstruction can take the place of a great deal of "accessor-function" style
programming, while dispatching can replace many conditional-branching constructs.

### Deconstruction

Drawing on our `tuple` example, here is a definition that gets the second element
of a tuple:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
getSecondElement (e1,e2) = e2
~~~

This would be equivalent to:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
-- `snd` gets the second element of a tuple.
getSecondElementEquiv t = snd t
~~~

### Dispatch

Dispatching on a boolean argument instead of using an if statement:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
multiplyByFiveIfTrue x True  = x * 5
multiplyByFiveIfTrue x False = x
~~~

This would be equivalent to:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
multiplyByFiveIfTrue x b  =
  if b then x * 5
       else x
~~~

Another interesting use-case for dispatch is numbers!

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
-- http://mathworld.wolfram.com/AckermannFunction.html
ackermann 0 y = y + 1
ackermann x 0 = ackermann (x-1) 1
ackermann x y = ackermann (x-1) (ackermann x (y-1))
~~~


### Combining Deconstruction and Dispatch

Here is an example combining both deconstruction and dispatch, but
taking a tuple argument with a number in the first position
and a boolean in the second. The number is used in the definition,
and the boolean is used to dispatch:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
multiplyByFiveIfTrueTuple (x,True)  = x * 5
multiplyByFiveIfTrueTuple (x,False) = x
~~~

This would be equivalent to:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
multiplyByFiveIfTrueTupleEquiv t =
  if (snd t) then (fst t) * 5
             else (fst t)
~~~

While you can use pattern-matching in function-definitions, you can
also use it wherever you bind a value, such as case-statements,
where and let blocks, and do-notation. You don't have to use
pattern-matching in all cases, and sometimes it's easier to just
use an `if` statement. That's fine! Pattern-matching simply looks
a little cleaner in some cases.

## Local Definitions with `let` and `where`

If you wish to create local variable definitions in order
to simplify your functions or reduce computation, then
you can use `let` bindings:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
amountOfCatFoodLet =
  let
    days = 21
    servingsPerDay = 2
    numberOfCats = 3
    numberOfServings = numberOfCats * servingsPerDay * days
    servingSize = 100 -- grams
  in numberOfServings * servingSize
~~~

Or with `where` bindings:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
amountOfCatFoodWhere = numberOfServings * servingSize
  where
    days = 21
    servingsPerDay = 2
    numberOfCats = 3
    numberOfServings = numberOfCats * servingsPerDay * days
    servingSize = 100 -- grams
~~~


## Importing Modules

Some functions that you may wish to use aren't part of the Prelude, so you will have
to import the modules which they reside in.

This can be done in both your source-code, and GHCi with the `import` keyword, although
the imports in your source have to be close to the top of the file.

~~~{data-language=haskell .nocheck}
import Data.Char                  -- Access to all Data.Char functions
import Data.Char as DCH           -- Name the import for later use
import qualified Data.Char as D   -- Only allow qualified references
import Data.Char (toUpper)        -- Only import a set of functions
import Data.Char hiding (isDigit) -- Hide certain functions
~~~

Try using [Hoogle](https://www.haskell.org/hoogle/) to search for functions and
the modules where they live!

```note
An example of using module imports:

Prelude> import Data.Char (toUpper)
Prelude> reverse (map toUpper "hello")
"OLLEH"
```

## Lists

Lists are a commonly used data-structure in Haskell. Everything in a list has
the same type (they are homogeneous).

Lists are built using the infix data-constructor `(:)` (pronounced "cons"). They also have a compact notation using `[...]`.

List literals look like:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
list1 = [1,2,3]
list2 = 1 : 2 : []
list3 = "hello" : "world" : []
~~~

More information about why lists can be used the way that they are is
contained in the [ADTs](#adts-algebraic-data-types) chapter.

```instruction
Define a variable containing a list.
```

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myList = [1,2,3]
~~~

You can deconstruct a list by pattern matching the head and tail as in the case
of this function definition:

~~~{data-language=haskell .nocheck}
f (x:xs) = something something ...
~~~

```instruction
Define a function to get the first element of a list.
```

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myHead (x:xs) = x -- This is a partial function, Beware!
~~~

In `Prelude` this function is called `head`.

```note
"head" is a partial function - It will raise an exception if
called with an empty list.

In Haskell we generally wish to avoid defining partial functions.
```

```instruction


Define a variable containing the first element of your list.

Make use of your myHead function in the definition!
```

~~~{.answer data-language=haskell .nocheck} 
myFirstElement = myHead myList
~~~

### Define Length

```instruction
Define a function that takes a list and returns the length.
```

Your solution should have the form of:

~~~{data-language=haskell .nocheck}
myLength []     = ...
myLength (x:xs) = ...
~~~

Things to consider:

* What is the length of an empty list? (the base case)
* What is the length of something with an additional element?
* What is the length of xs? Can you use a function for this?

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myLength []     = 0
myLength (x:xs) = 1 + myLength xs
~~~


```note
Repeated definitions with different argument structures,
such as myLength, is called "pattern-matching". This is
because each line "matches" the "pattern" of its arguments.
```

### Define `myMap`

```instruction


Define a function "myMap" that takes a function-argument and a list,
and returns a new list containing the result of the function-argument
applied to each element of the list-argument.
```

Some concrete examples of such a function may do the following:

* Take a function that divides integers by two, list of ints, and returns a list of doubles.
* Take a function that turns lowercase into uppercase characters, and a String, and returns a string in CAPS.
  Such an uppercasing function can be found in the `Data.Char` module named `toUpper`.

Things to consider:

* What is the base-case of myMap?
* What is the inductive-case of myMap?

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
~~~

### Combining Lists

Lists can be combined by using the `++` function. This is an infix function
similar to `+`.

```instruction


Define a new "betterList" combining your "myList" list with itself
to make it twice as good!
```

~~~{.answer data-language=haskell }
betterList = myList ++ myList
~~~

## Fun List Functions

For your reading pleasure, here are some definintions of other common functions:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myFilter f []     = []
myFilter f (x:xs) = if f x then x : myFilter f xs
                           else     myFilter f xs

myFold f z []     = z
myFold f z (x:xs) = f x (myFold f z xs)

myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myElem e []     = False
myElem e (x:xs) = if e == x then True
                            else myElem e xs
~~~

```open
An open-ended question:

What is a good balance between safety and expressiveness in a
programming-language?
```
