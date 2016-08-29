
----

# Ecosystem

The Haskell ecosystem is large and interesting, it is held together more
by convention than by dictation, with the current convention being that
open source packages are made available through `cabal` on Hackage.
On top of this distribution, there is a convenient tool provided by
Commercial-Haskell called [Stack](https://docs.haskellstack.org/en/stable/README/).
Stack builds off the existing ecosystem, but provides stable
snapshot releases of compatible packages that makes it easy
to install packages that play well together.

## Lexicon

-----------       -------------     ------------
-----------       -------------     ------------
Stack             install           pointfree
ghci              new               Hackage

## Stack

The easiest way for newcomers to get started with Haskell these days
is by installing Stack via the steps outlined in the [Setup](#setup)
chapter.

Stack provides a plethora of functionality and you can get an
inkling of this by invoking `stack --help`. However, for the purposes
of this workshop you will only really require the use of `stack exec --ghci`.

The next steps to take would be the installation of libraries and programs
via `stack install` and the creation of new stack projects via
`stack new`.

```instruction


Install the pointfree package from stack.

Use the `pointfree` command-line program to to check what the
pointfree version of `\x -> \y -> x + y + 1` is.

Did this seem pointless to you?
```

```{ .answer }
$ pointfree '\x -> \y -> x + y + 1'

flip flip 1 . ((+) .) . (+)
```

```open
An open-ended question:

How would you go about publishing your own package to Hackage?
```
