Network-address
===============

A Haskell module for querying network interface addresses on Linux. It uses [libnl](http://www.infradead.org/~tgr/libnl/) under the hood.

Note: only works on Linux.

Install
-------

First make sure libnl and libnl-dev are installed. For example on Debian/Ubuntu:

    $ sudo apt-get install libnl libnl-dev

Then you can use cabal to install the module:

    $ cabal install

Or test it via ghci:

    $ cabal build
    $ cabal repl
