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

Running full spec suite, FileSpec passes, yet leaves the temporary foobar and tossdir stuff around. Yet, if you
run the spec separately, it doesn't happen!

