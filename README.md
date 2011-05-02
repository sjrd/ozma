The Ozma programming language
=============================

Ozma is a programming language based on Scala, with extensions regarding
concurrency. It adds the general paradigm of declarative concurrency to the
Scala language.

Ozma does not compile towards the JVM nor the MSIL. Instead, it compiles towards
the Mozart programming system. This runtime provides built-in support for
declarative concurrency.

Software requirements
---------------------

In order to build the Ozma compiler and library, you need the following software
installed on your computer:

*   Scala >= 2.9.0 (currently in RC)
*   Ant >= 1.6 (as required by Scala)

Build instructions
------------------

The entire Ozma compiler and library can be built with Ant:

    $ ant

Run the compiler
----------------

After you have built Ozma, you can run the compiler using:

    $ <ozma>/bin/ozmac FILE.scala...

All the options applicable to `scalac` (see `man scalac`) are also applicable to
`ozmac`.

Author
------

Sébastien Doeraene

About Scala and Mozart
----------------------

*   [The Scala programming language](http://www.scala-lang.org/)
*   [The Mozart programming system](http://www.mozart-oz.org/)
