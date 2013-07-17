{-# LANGUAGE OverloadedStrings #-}
-- http://orbt.io/QHBm.png
module Main where

import Text.Hastache
import Web.Scotty as S
import Web.Scotty.Hastache

main :: IO ()
main = scottyH 3000 $ do
  setTemplatesDir "templates"
  
  get "/:word" $ do
    beam <- param "word"
    setH "action" $ MuVariable (beam :: String)
    hastache "greet.html"
