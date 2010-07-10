import org.specs.runner._

/** 
 * Simple script to run the specs in the "spec" directory from a command line.
 * Run using:
 *   scala -classpath lib/specs-X.Y.Z.jar:lib/junit-4.5.jar:build bin/specrunner.scala
 */
val runner = new SpecsFileRunner("spec", ".*Spec")
runner.report(runner.specs)
if (runner.specs.exists(_.isFailing)) System.exit(1) else System.exit(0)  
