
# Making a Web-Site with Scotty { #website }

The Haskell library [Scotty](http://hackage.haskell.org/package/scotty) is
similar to the ruby web-library [Sinatra](http://www.sinatrarb.com/).

<div class="center">

![Beam me Up](resources/images/scotty.png)

</div>

Scotty can be installed by using the following Stack command:

```shell
> stack install scotty
```

Scotty's behaviour is based around [REST](http://en.wikipedia.org/wiki/Representational_state_transfer)
verbs and routes.

For example - A simple Hello-World website:

~~~{ data-language=haskell .nocheck }
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
~~~

```note
The "LANGUAGE" line is a pragma, instructing the compiler to
allow string literals to be of types other than "String".
```

If we inspect the type of `get` in GHCi we see this:

```ghci
> import Web.Scotty
> :info get
get :: RoutePattern -> ActionM () -> ScottyM ()
```

The ActionM Monad allows us to perform any IO action we desire, such as
printing to the console, reading files, etc - through the use of the liftIO
function.

~~~{data-language=haskell data-filter=./resources/scripts/check.sh}
{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class
import Data.Monoid

myRoute = get "/hello" $ do
  liftIO $ putStrLn "about to return hello!"
  html "Hi!"
~~~

```instruction
Modify this simple website to show the current time.
```

```hint
Install the "time" package and "import Data.Time.Clock".
Use "show" to convert the time to a String.
Use the pack function from "import Data.Text.Lazy (pack)".
"pack" can convert Strings to Text.
```

```answer
First install the "time" package.

Then define your route as:

import Data.Time.Clock
import Data.Text.Lazy (pack)

myTimeRoute = get "/time" $ do
  t <- liftIO getCurrentTime
  html $ pack $ show t

-- You can find a full file in the scaffold/website folder.
```

```instruction


Modify this simple website to output the answers from the
other chapters in this workshop!
```

```open
An open question:

What features do the more advanced Haskell web-frameworks include
that Scotty lacks?
```
