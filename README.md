scotty-hastache
===============

Integrating Hastache to Scotty

- [Scotty](http://github.com/xich/scotty) - a light-weighted Web framework/router
- [Hastache](https://github.com/lymar/hastache) - Haskell implementation of [Mustache](http://mustache.github.io/) templates

This is still work-in-progress

Installation
=========

1. Install GHC, Haskell platform, etc
2. Install my fork of Scotty:
  you need to checkout the `scotty-transformer` branch.

  ```
  $ git clone https://github.com/co-dan/scotty.git
  $ cd scotty 
  $ git checkout scotty-transformer
  $ cabal install
  ```
3. Install hastache:

  ```
  $ cabal install hastache
  ```
4. Clone & install scotty-hastache

Examples
========

- `examples` folder
- <https://gist.github.com/co-dan/6015894>
