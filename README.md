typereplike
===========

Serializable form of [TypeRep](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Typeable.html#t:TypeRep),
along with some utilities for manipulating them.

# Compiling and building

When compiling, be sure to update the submodules and add them to a cabal sandbox:

```
$ git submodule update --init
$ cabal sandbox init
Writing a default package environment file to
/home/eric/workspace/typereplike/cabal.sandbox.config
Creating a new sandbox at /home/eric/workspace/typereplike/.cabal-sandbox
$ for v in submodules/*; do cabal sandbox add-source $v; done
$ cabal install
```
