README.txt for Sake a build tool written in Scala

Dean Wampler dean@polyglotprogramming.com



History:

December 6, 2008: Created Sake project.
December 20, 2008: First "bootstrap" build of Sake with itself!

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


BUGS:

* Specs target broken because it uses the current path, before specs compiled?!


IMPLEMENTATION NOTES:

1) You'll notice that a lot of types have small, protected methods that are often one liners for creating Files, etc. They could be easily inlined. Usually, they are there to facilitate testing. A spec can subclass the type under test (TUT) and override the method to return a test double, etc.

