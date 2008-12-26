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

BUGS:

None known at this time.

IMPLEMENTATION NOTES:

1) You'll notice that a lot of types have small, protected methods that are often one liners for creating Files, etc. They could be easily inlined. Usually, they are there to facilitate testing. A spec can subclass the type under test (TUT) and override the method to return a test double, etc.

