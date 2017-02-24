{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Monoid
import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Text.Lazy (pack)

main = scotty 3000 $ do
  myRoute
  myTimeRoute
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

myRoute = get "/hello" $ do
  liftIO $ putStrLn "about to return hello!"
  html "Hi!"

myTimeRoute = get "/time" $ do
  t <- liftIO getCurrentTime
  html $ pack $ show t
