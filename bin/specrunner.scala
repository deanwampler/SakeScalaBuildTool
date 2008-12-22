import org.specs.runner._

/** 
 * Simple script to run the specs in the "spec" directory from a command line.
 * Run using:
 *   scala -cp lib/specs-1.4.1.jar:lib/junit-4.4.jar:build bin/specrunner.scala
 */
val runner = new SpecsFileRunner("spec", ".*")
runner.report(runner.specs)
