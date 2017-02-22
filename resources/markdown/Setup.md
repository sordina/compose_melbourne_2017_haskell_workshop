
----

Setup
=====

This section will help you get up and running so that you
can participate in the workshop and complete the exercises.

## Lexicon

-----------       -------------     ------------
-----------       -------------     ------------
Stack             setup             GHCi
Calculations      $PATH             Loading
:reload           Redefine          GHC
Compile           Optimisation      Install
Pointfree         Ecosystem

<div class="important">
Ensure that you have the following programs installed and functioning correctly:

## [Stack](https://docs.haskellstack.org/en/stable/README/)

Check that you have stack installed:

```shell
stack --version
```

This should output something similar to:

    Version 1.1.2 x86_64 hpack-0.14.0

Otherwise, install it!

### Linux and OS X:

```shell
curl -sSL https://get.haskellstack.org/ | sh
stack setup
stack ghci
> 1 + 1
```

### Windows:

Use the 64-bit installer from <https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows>.

Provided you use the 64-bit version, you shouldn't need to worry about the PATH issues.

Then run the following in `Cmd`:

```shell
stack setup
stack ghci
> 1 + 1
```

This should output:

    2

</div>

```real
You can use GHCi to perform calculations other than just "1 + 1".

Here is an example session:

[Prelude] > 1 + 2 + 3
6
[Prelude] > 100 / 2
50.0
[Prelude] > 6 ^ 7
279936
[Prelude] > ^D
Leaving GHCi.
```

```instruction
Using GHCi...

Calculate the price of 42-bakers-dozens of eggs at $3 per-egg.
```

```answer
-- Note that a baker's dozen is 13!
[Prelude] 42 * 13 * 3
1638
```

```note
If ghci is on your PATH, then you can invoke it directly,
however, if you have just installed stack, then you will
need to invoke ghci indirectly by calling

> stack exec -- ghci [ARGS]

```


## Loading files in GHCi {.important}

There are many ways to load and execute Haskell code. For the purposes of this workshop,
if you do not already have a workflow you are comfortable with, then we suggest the
following steps:

* Write and edit your programs in files ending in the extension ".hs"
* When you are ready to test your program, load it into GHCi
* After making modifications to your program, reload the program in GHCi

Say you had written the following program `test.hs`:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
main = print "hello world"
~~~

Load the file in GHCi to see it in action:

```shell
> stack exec -- ghci test.hs
GHCi, version 7.6.2: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Main             ( test.hs, interpreted )
Ok, modules loaded: Main.
[*Main] > main
"hello world"
```

... Unfortunately there is a bug in the program, so in your editor you
make the change required to print "hello, world" with the mandated comma:

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
main = print "hello, world"
~~~

Now, back in GHCi, you can reload the program without exiting the
REPL (Read Eval Print Loop):

```shell
[*Main] > :reload
[1 of 1] Compiling Main             ( test.hs, interpreted )
Ok, modules loaded: Main.
[*Main] > main
"hello, world"
```

Much better!

```real
You can inspect a value (or function) in ghci with the `:info` command
in order to find out a little about its type and definition:

ghci> :info main
main :: IO ()   -- Defined at test.hs:1:1

If you just wish to see the type of an expresison, you can use
the `:type` command:

ghci> :type main
main :: IO ()
```

```instruction


* In the previous example, you defined a function 'main'
  that printed "hello, world"...
* .. Now, define a new numeric function that prints something else
* Load it in GHCi
* Test your function in GHCi
* Make a modification
* Reload your chages without exiting GHCi
* Test your changes
```

## GHC {.important}

Create the following source file (program.hs):

~~~{data-language="haskell"}
main = print "hello world"
~~~

Compile the program as follows:

```shell
stack exec -- ghc --make program.hs
```

Run the program with the following command:

```shell
./program
```

The output should look as follows:

```text
"hello world"
```

```real
Compiled programs are almost always significantly faster than instructions
run inside GHCi. Even greater speed-ups are possible by using the "-O"
optimisation settings for GHC.
```

```instruction
Using GHC...

Compile and run hello-world.
```

```answer
> echo 'main = print "hello friends"' > main.hs
> stack exec -- ghc --make main.hs
[1 of 1] Compiling Main             ( main.hs, main.o )
Linking main ...
> ./main
"hello friends"
```

```open
An open-ended question:

Given that GHC is largely written in Haskell, how was GHC first compiled?
```

```open
An open-ended question:

What are some of the current issues with the Haskell ecosystem?
```
