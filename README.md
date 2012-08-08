The Ozma programming language
=============================

Ozma is a programming language based on [Scala](http://www.scala-lang.org/),
with extensions regarding concurrency. It adds three [paradigms](http://www.info.ucl.ac.be/~pvr/VanRoyChapter.pdf) of the Oz
programming language to Scala: declarative concurrency, lazy execution
(demand-driven execution) and message-passing concurrency.

Ozma does not compile towards the JVM nor the MSIL. Instead, it compiles towards
the [Mozart programming system](http://www.mozart-oz.org/). This runtime
provides built-in support for these three concurrency models.

Ozma was initially developed as a master's thesis at the [Université Catholique de
Louvain](http://www.uclouvain.be/), under the supervision of Pr Peter Van Roy.
The [text of the master's
thesis](http://www.info.ucl.ac.be/~pvr/MemoireSebastienDoeraene.pdf) details the
design and implementation, as well as rationale for the Ozma programming
language. It also discusses most of the example programs.


Get Ozma
--------

### Software requirements

In order to run the Ozma compiler and Ozma programs, you need the following
software installed on your computer:

*   Scala >= 2.9.0 < 2.9.1
*   Mozart >= 1.4.0

Binaries for Scala and Mozart are required to be available in the PATH. The
following should run properly:

    $ scala -version
    Scala code runner version 2.9.0.final -- Copyright 2002-2011, LAMP/EPFL
    $ oztool version
    1.4.0

Ozma is tested only on Linux by its original author. It is also supposed to work
fine on any POSIX environment supporting both Scala and Mozart (e.g. Mac OS).

Windows users are likely to run into trouble, though it has been reported that
it can be made working. Contributions to make Ozma run smoothly on Windows are
welcome!

You can either download pre-compiled binaries for Ozma, or compile it from
source. We recommand compiling from source because binaries are not updated very
often.

### Pre-compiled binaries

*   [Download as .tgz](http://35541hpv124015.ikoula.com/~ozma/download/ozma-complete.tgz)
*   [Download as .zip](http://35541hpv124015.ikoula.com/~ozma/download/ozma-complete.zip)

Unzip them wherever you want. Executables are available in the `bin/` directory.
It is recommanded that you add this directory to your PATH, for convenience.

### Build Ozma from source

#### Further requirements

In order to build Ozma from source, you need:

*   Ant >= 1.6

You also need to define the environment variable `SCALA_HOME` so that it points
to your local Scala installation. Note that installers for Scala (such as the
Typesafe Stack) automatically configure this environment variable.

#### Actual build

The entire Ozma compiler and library can be built with Ant:

    $ ant

The compilation is likely to take quite a lot of time (tens of minutes).

Executables are placed in the `bin/` subdirectory. It is recommanded that you
add this directory to your PATH, for convenience.


Usage
-----

### Hello world

The running unit of an Ozma program is the `main(args: String[Array])` method of
an _object_.

Here is the traditional HelloWorld program in Ozma. You can find it in
`docs/examples/helloworld/`, in the file `helloworld/HelloWorld.scala`.

    package helloworld

    object HelloWorld {
      def main(args: Array[String]) {
        Console.println("Hello world!")
      }
    }

Note: using the `App` (or `Application`) trait of Scala does not work for now.

### Run the compiler

After you have built Ozma, you can run the compiler using:

    $ ozmac FILE.scala...

All the options applicable to `scalac` (see `man scalac`) are also applicable to
`ozmac`.

To compile the Hello World program, do the following:

    $ cd <ozma>/docs/examples/helloworld/
    $ ozmac helloworld/HelloWorld.scala

This will produce a compiled functor `HelloWorld.ozf` in the `helloworld`
subdirectory.

### Run a compiled object

To run a compiled object, use the program `ozma`.

    $ ozma pack.subpack.ObjectName [ARGS...]

Thus, you can run the Hello world program like this:

    $ ozma helloword.HelloWorld


Examples
--------

A lot of examples can be found in the directory `docs/examples/`. In increasing
order of complexity, they are:

*   Basic Scala-valid examples
    *   Hello world!
    *   Echo: echo command-line arguments on the standard output
    *   Exceptions
*   Basic Ozma examples
    *   Trivial thread
*   Scala-valid examples with Ozma-specific tail recursion
    *   Merge sort
    *   Binary trees
*   Streams
    *   Prime numbers
    *   Token ring
    *   Bounded buffer
*   Message-passing concurrency
    *   Tossing the ball
    *   Server ports
*   Advanced examples
    *   Digital logic simulation

Another advanced example is available in a dedicated repositiory:
[Capture the Flag](https://github.com/sjrd/capture-the-flag)


Documentation
-------------

The best source of documentation is the [text of the master's
thesis](http://www.info.ucl.ac.be/~pvr/MemoireSebastienDoeraene.pdf).

*   Chapter 3 contains a tutorial for Ozma
*   Chapter 4 gives step-by-step construction of some of the example programs
*   Chapter 5 gives the semantics of the language


Development
-----------

Ozma is developed using Eclipse, with the Scala IDE plugin.


Author
------

Sébastien Doeraene


About Scala and Mozart
----------------------

*   [The Scala programming language](http://www.scala-lang.org/)
*   [The Mozart programming system](http://www.mozart-oz.org/)
