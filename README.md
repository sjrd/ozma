The Ozma programming language
=============================

Ozma is a programming language based on [Scala](http://www.scala-lang.org/),
with extensions regarding concurrency. It adds three paradigms of the Oz
programming language to Scala: declarative concurrency, lazy execution
(demand-driven execution) and message-passing concurrency.

Ozma does not compile towards the JVM nor the MSIL. Instead, it compiles towards
the [Mozart programming system](http://www.mozart-oz.org/). This runtime
provides built-in support for these three concurrency models.

Ozma was initially developed as a master thesis at the [Université Catholique de
Louvain](http://www.uclouvain.be/), under the supervision of Pr Peter Van Roy.
The [text of the master
thesis](http://ks365195.kimsufi.com/~sjrd/master-thesis.pdf) details the design
and implementation, as well as rationale for the Ozma programming language. It
also discusses most of the example programs.


Usage
-----

### Software requirements

In order to build the Ozma compiler and library, you need the following software
installed on your computer:

*   Scala >= 2.9.0
*   Mozart >= 1.4.0
*   Ant >= 1.6

Ozma is tested only on Linux by its original author. It is also supposed to work
fine on any POSIX environment supporting both Scala and Mozart (e.g. Mac OS).

Windows users are likely to run into trouble, though it has been reported that
it can be made working. Contributions to make Ozma run smoothly on Windows are
welcome!

### Build instructions

Due to a limitation of the Gump parser generator of Mozart, users of Windows or
of any 64-bit system cannot build one module of Ozma, called `ozastc`. For those
users, we provide binaries of `ozastc`. Simply download the appropriate archive
and unzip its contents into the directory `build/ozastc/`.

*   [`ozastc` for Linux](http://ks365195.kimsufi.com/~ozma/download/ozma-build-ozastc-linux.tgz)
*   [`ozastc` for Mac](http://ks365195.kimsufi.com/~ozma/download/ozma-build-ozastc-darwin.tgz)
*   [`ozastc` for Windows](http://ks365195.kimsufi.com/~ozma/download/ozma-build-ozastc-windows.zip)

We understand that this is quite annoying. We plan to move to a Gump-free
solution relatively soon. The `ozastc` module is fortunately very stable, so
that it is unlikely that this operation will be needed twice.

Once this is done (or immediately if you run a 32-bit POSIX environment), the
entire Ozma compiler and library can be built with Ant:

    $ ant

Scala and Mozart binaries must be available in the PATH. Additionaly, you need
to define the environment variable `SCALA_HOME` so that it points to your local
Scala installation.

Executables are placed in the `bin/` subdirectory. It is recommanded that you
add this directory to your PATH, for convenience.

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

The best source of documentation is the [text of the master
thesis](http://ks365195.kimsufi.com/~sjrd/master-thesis.pdf).

*   Chapter 3 contains a tutorial for Ozma
*   Chapter 4 gives step-by-step construction of some of the example programs
*   Chapter 5 gives the semantics of the language


Development
-----------

Ozma is developed using Eclipse, with the Scala plugin for Eclipse. You will
need to define the variable `SCALA_HOME` to point to your actual Scala
installation.

This can be set up in the preferences, under Java > Build path > Classpath
Variables.


Author
------

Sébastien Doeraene


About Scala and Mozart
----------------------

*   [The Scala programming language](http://www.scala-lang.org/)
*   [The Mozart programming system](http://www.mozart-oz.org/)
