calculusbot
===========

Calulusbot does math! But much better than I do. Where I make lots of mistakes when taking derivatives,
calculusbot doesn't. That is, assuming she's written correctly ;)

Building
--------

You should be able to build CB with the latest version of GHC. Just pull the code and do a
`cabal build`. Calculusbot depends on a couple things (see: `calculusbot.cabal`), but nothing
your standard Haskell-platform doesn't include.

Testing
-------
Haha, what's that?

Running
-------
To play with CB, build and then type `./dist/build/interact/interact`. It'll run the program found
in `interact/Main.hs`. As you can see, it's pretty basic--read a function from stdin (it will keep
reading until it hits EOF, ctrl+d on most UNIX-y systems), differentiate it, then print it out
nicely.
