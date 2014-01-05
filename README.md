scotty-hastache
===============

Integrating Hastache into Scotty

- [Scotty](http://github.com/xich/scotty) - a light-weighted Web framework/router
- [Hastache](https://github.com/lymar/hastache) - Haskell implementation of [Mustache](http://mustache.github.io/) templates

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Hastache
import Web.Scotty.Trans as S
import Web.Scotty.Hastache

main :: IO ()
main = scottyH' 3000 $ do
  templates "templates"
  -- ^ Setting up the director with templates
  get "/:word" $ do
    beam <- param "word"
    setH "action" $ MuVariable (beam :: String)
    -- ^ "action" will be binded to the contents of 'beam'
    hastache "greet.html"
```

templates/greet.html:
```html
<h1>Scotty, {{action}} me up!</h1>
```

Installation
=========

1. Install GHC, Haskell platform, etc.
2. Use cabal-install to automatically pull all the dependencies and
install the library:

```
cabal update
cabal install scotty-hastache
```

Examples
========

- `examples` folder
- <https://gist.github.com/co-dan/6015894>
