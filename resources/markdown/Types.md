
---

# Types

```note.notitle
Question: How do you create a great program?
Answer:   You type it!
```

In this chapter, we will go over the exercises from the introduction
and add types to the examples.

## Lexicon

-----------       -------------     ------------
-----------       -------------     ------------
Type              Signature         Inline
Int               ::                Floating
Variable          Synonym           String
Tuple             Function          (,)
C                 Argument          Curried
Parentheses       Multiply          Lists
Around-Fix        Prefix-Form       Deconstruction
head              length            map

## Types

In Haskell, values have associated types. What this means is that any
particular value can have its type reasoned about statically. But
what is a type? There are primitive types, type-builders, and user-defined
types. These make available a parallel language for expressing, reasoning
about, and constraining different types. The value-level language
and the type-level language are linked in several different ways.
One is that through "inference" the types of your expressions have to
"make sense". Another way is the explicit type-signatures that you
can give you values.

## Signatures

Type signatures can be provided inline, or above definitions.
Primitive types generally start with a capital letter. The general format
goes something like this:

~~~{ data-language=haskell }
value :: { signature }
value { definition }
~~~

In English, you can read this as...

> value is of type { signature }...

For example:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
x :: Int
x = 3
~~~

or

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
x = (3 :: Int)
~~~

It is far more common to place the type-signature above the definition,
with inline types only used in situations where ambiguities need
to be resolved.

<div class="important">

~~~ {.instruction .nobefore}
You are defining a floating point variable:
~~~

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myFloat = 1.1
~~~

```instruction
Give your variable a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myFloat :: Float
myFloat = 1.1
~~~

</div>

## Type Synonyms

In Haskell, we can give type-expressions an alias (or synonym) by
using the `type` keyword. This allows you to cut down the verbosity
and chance of errors
in your code when you have type expressions that would otherwise
be repeated frequently.

An example of this is the `String` type-synonym, which is defined as
follows:

~~~{data-language=haskell .nocheck}
type String = [Char]
~~~

```instruction
 

Give your "myString" variable from the previous chapter a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myString :: String
myString = "Hello Haskell"
~~~

## Tuples

Tuple type signatures look the same as the tuples themselves, with
types in place of the data.

For example, if you had a tuple of a String and an Int, the type
would look as follows:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myTuple :: (String, Int)
myTuple = ("The meaning of life", 42)
~~~

```instruction
Give your previous "myTuplePair" definition a type signature.
```

## Type-Variables

Most primitive-types in a type-signature are expressed with upper-case
names. However, if you want to indicate that a type is repeated
in a signature, but don't want to lock down what the type is, then
you can use a lower-case type-variable.

For example, if we wanted a tuple to have the same type of element
in each position, then we could write the following:

~~~{data-language=haskell .nocheck}
... (a,a) ...
~~~

This becomes especially important in the context of functions!

## Functions

The type signatures of functions in Haskell are a little different
from how they look in the more familiar C family languages,
but the syntax is very elegant and will allow a higher-level of
reasoning than less consistent forms.

The syntax for a function type-signature is of the form:

~~~{data-language=haskell .nocheck}
{functionName} :: {argument} -> {result}
~~~

The main idea is that functions in Haskell only ever take one
argument. If you wish to define a function that takes more
than one argument, then you should, in fact, define a function
that takes one argument, then returns another function.

Luckily the syntax for doing this in Haskell looks identical
to defining a multi-argument function:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply x y z = x * y * z
~~~

However, the distinction becomes clear with the type-signature:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply :: Int -> (Int -> (Int -> Int))
myMultiply x y z = x * y * z
~~~

```note
If you ask for info of your multiply function before
giving it a type-signature, you will see references to
" Num => "... Ignore that for now, we will cover it in
the type-classes chapter!
```

Now we can see that the function only takes one argument, then returns a function
(that only takes one argument, and returns a function
(that only takes one argument, that returns an Int.))

This is known as currying.

Fortunately, Haskell's function syntax is right-associative, allowing us to
drop the parentheses:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myMultiply :: Int -> Int -> Int -> Int
myMultiply x y z = x * y * z
~~~

... and the syntax for function-application works well with this idea too!

```instruction


Define a "myMultiplyB" function that multiplies 4 numbers.

Make sure to give your function a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myMultiplyB :: Int -> Int -> Int -> Int -> Int
myMultiplyB w x y z = w * x * y * z
~~~

## Lists

List type-signatures look like:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
list1 :: [Int]
list2 :: [Int]
list3 :: [String]

list1 = [1,2,3]
list2 = 1 : 2 : []
list3 = "hello" : "world" : []

list1A :: ([]) Int -- Very rarely used syntax!
list1A = [1]
~~~

```note
Comments in Haskell start with "--" for single-line,
and "{- ... -}" for multi-line.
```

List type signatures are special in that the type-constructor is "Around"-fix.
This is not generally possible, and lists are a special case in that regard.

If you find you need to, you can use the list type in prefix-form, as per variable
`list1A`.

```instruction
Define a list variable and give it a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myList :: [Int]
myList = [1,2,3]
~~~

```instruction
Give your `head` deconstructor function a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh}
myHead :: [a] -> a
myHead (x:xs) = x
~~~

### Length Signature

```instruction
Give your length function a type-signature.
```

~~~{data-language=haskell .answer data-filter=./resources/scripts/check.sh} 
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs
~~~

### Map Signature

```instruction
Give your `map` function a type-signature.
```

Things to consider:

* What is the type of the first argument of myMap?
* What is the second argument, etc?
* What is the type of the result of myMap?

~~~{.answer data-language=haskell data-filter=./resources/scripts/check.sh}
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
~~~

## Fun List Functions Types

Here are the types for the definintions of the list functions from the previous chapter:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f []     = []
myFilter f (x:xs) = if f x then x : myFilter f xs
                           else     myFilter f xs

myFold :: (a -> b -> b) -> b -> [a] -> b
myFold f z []     = z
myFold f z (x:xs) = f x (myFold f z xs)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

myElem :: Eq a => a -> [a] -> Bool
myElem e []     = False
myElem e (x:xs) = if e == x then True
                            else myElem e xs
~~~

```instruction
Try to understand the type-signatures for these functions.

Hint: Try finding a way to say them in English.
```

```open
An open-ended question:

How many types could a type-checker check...
... if a type checker could check types?
```
