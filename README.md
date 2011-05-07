The Ozma programming language
=============================

Ozma is a programming language based on Scala, with extensions regarding
concurrency. It adds the general paradigm of declarative concurrency to the
Scala language.

Ozma does not compile towards the JVM nor the MSIL. Instead, it compiles towards
the [Mozart programming system](http://www.mozart-oz.org/). This runtime
provides built-in support for declarative concurrency.


Usage
-----

### Software requirements

In order to build the Ozma compiler and library, you need the following software
installed on your computer:

*   Scala >= 2.9.0 (currently in RC)
*   Mozart >= 1.4.0
*   Ant >= 1.6

### Build instructions

The entire Ozma compiler and library can be built with Ant:

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

    $ ozma package.subpack.ObjectName [ARGS...]

In the current state of development, `ozma` cannot work with multiple paths in
its classpath. Hence, you need to add symlinks to the `java/` and `scala/`
directory of the runtime:

    $ ln -s <ozma>/build/runtime/java java
    $ ln -s <ozma>/build/runtime/scala scala

Then, you can run the Hello world program:

    $ ozma helloword.HelloWorld


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
