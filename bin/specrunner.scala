import org.specs.runner._

/** 
 * Simple script to run the specs in the "spec" directory from a command line.
 * Run using:
 *   scala -classpath lib/specs-1.4.3.jar:lib/junit-4.4.jar:build bin/specrunner.scala
 */
val runner = new SpecsFileRunner("spec", ".*")
runner.report(runner.specs)
if (runner.specs.exists(_.isFailing)) System.exit(1) else System.exit(0)  
