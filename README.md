# README for Sake: a Build Tool Written in Scala

Dean Wampler
dean@polyglotprogramming.com

I wrote _Sake_ (pronounced like the Japanese rice beverage) as a learning exercise to experiment with Scala features, like DSL creation. It is inspired by Ruby _Rake_ and Unix _Make_, hence the name. (I know that there is already a distributed build system for Ruby called _Sake_...) _Sake_ was used to build the [code examples](http://examples.oreilly.com/9780596155964/) for [Programming Scala, 1st Edition](http://programming-scala.labs.oreilly.com/).

_Sake_ is very incomplete and it has plenty of warts. If you want a "production quality" build tool for Scala and Java programs, consider [SBT](http://www.scala-sbt.org/), [CBT](https://github.com/cvogt/cbt), or [Bazel](https://bazel.build/).

## Getting Started

The distribution is already built, but you can run `bin/sake` to re-build it at any time. (See also the note below.)

It is easiest to copy the whole distribution somewhere useful and then define a `SAKE_HOME` environment variable that points to that root directory. Follow these steps.

* Copy the distribution somewhere convenient, e.g., `/usr/local/sake`.
* Define `SAKE_HOME`, e.g., `SAKE_HOME=/usr/local/sake` (using appropriate syntax for your shell).
* Add `$SAKE_HOME/bin` to your path.

To use Sake in a project:

* Create a `sake.scala` file in the root of your project.
* Copy the `sake.scala` contents from the Sake distribution to your `sake.scala`.
* Edit to taste.
* Copy the versions of Sake's jars that you want to use, either `lib/2.7.7/sake-2.7.7-1.1.jar` or `lib/2.8.0.RC7/sake-2.8.0.RC7-1.1.jar`.
* Build with `bin/sake [targets]`, where one or more targets can be specified. (Defaults to `all`.)

> **Note:** To bootstrap sake when there is not a previous version built, use the included Makefile. For example, this will be necessary when upgrading to a newer version of Scala that is not binary compatible with the previous version.

## Layout of the Distribution

* `README.md` - This file.
* `sake.scala` - Sake build file that builds sake itself. Used by `bin/sake`.
* `bin` - Directory with the `sake` Linux/OSX shell script and `sake.bat` Windows script.
* `build` - Directory where build projects are staged.
* `lib` - Directory of third party libraries necessary to build and use Sake and where the sake jars themselves are also written.
* `src` - Directory of Sake source code.

## History

* December 6, 2008: Created Sake project.
* December 20, 2008: First "bootstrap" build of Sake with itself!
* April 18, 2009: Added sxr to build for generating browsable HTML of the source.
* September 21, 2009: Moved Sake to GitHub.
* July 10, 2010: Ported to Scala v2.8.0.RC7.
* November 5, 2017: Numerous updates.

## TODO

* Add a built-in jar command.
* Add copy and mv/rename commands.
* Add ScalaTest and ScalaCheck commands.
* Support setting environment options through command-line options.
* Provide a way for the user to specify the default target.
* Interactive Mode:
  * Run targets, if specified, after loading file.
  * If a target fails, don't exit!
* Use implicits to convert from strings to symbols where "useful". Same for A* vs. List[A] ??
* Support using tuples when defining dependencies, where lists are now required.
