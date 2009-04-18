README.txt for Sake a build tool written in Scala

Dean Wampler dean@polyglotprogramming.com

GETTING STARTED:

The distribution is already built, but you can run "bin/sake" to re-build it at any time.

It is easiest to copy the whole distribution somewhere useful and then define a SAKE_HOME
environment variable that points to that root directory. Follow these steps.

* Copy the distribution somewhere convenient, e.g., /usr/local/sake.
* Define SAKE_HOME, e.g., SAKE_HOME = /usr/local/sake (using appropriate syntax for your shell).
* Add $SAKE_HOME/bin to your path.

To use Sake in a project:

* Create a "sake.scala" file in the root of your project.
* Copy the "sake.scala" contents from the Sake distribution to your sake.scala.
* Edit to taste.
* Build with "bin/sake [targets]", where one or more targets can be specified. (Defaults to "all".)


LAYOUT OF THE DISTRIBUTION:

README.txt - This file.
sake.scala - Sake build file that builds sake itself. Used by "bin/sake"
bin - Directory with the "sake" UNIX shell script and "sake.bat" Windows script.
browse - Directory of browsable HTML version of the Sake source code, generated using "sxr" 
(http://github.com/harrah/browse/tree/master). When you compile, you'll get the same
browsable HTML output in the browse directory.
build - Directory where build projects are staged.
lib - Directory of 3rd party libraries necessary to build and use Sake.
spec - Directory of "specs" files for testing.
src - Directory of Sake source code.


HISTORY:

December 6, 2008: Created Sake project.
December 20, 2008: First "bootstrap" build of Sake with itself!
April 18, 2009: Added sxr to build for generating browsable HTML of the source.

TODO:

* Add jar target.
* Add a copy and mv/rename targets.
* Add test, scalacheck commands.
* Set environment options through command-line options.
* Provide a way for the user to define the default target.
* Interactive Mode:
** Run targets, if specified, after loading file.
** If a target fails, don't exit!
* Use implicits to convert from strings to symbols where "useful". Same for A* vs. List[A] ??
* Support using tuples when defining dependencies, where lists are now required.


IMPLEMENTATION NOTES:

1) You'll notice that a lot of types have small, protected methods that are often one liners for creating Files, etc. They could be easily in-lined. Usually, they are there to facilitate testing. A spec can subclass the type under test (TUT) and override the method to return a test double, etc.

