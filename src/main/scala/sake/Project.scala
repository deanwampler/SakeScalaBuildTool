package sake

// import scala.collection.immutable._
import sake.command.builtin.Commands
import sake.command.{Result, Passed, Failed}
import sake.context.{Properties, Settings}
import sake.target.Target
import sake.util.{Exit, Log}

/**
* The Project class defines actual build scripts.
*/
class Project extends Commands {

  var settings: Settings = Settings.default
  var log: Log = Log.log
  var logLevel: Log.Level.Value = Log.Level.Info

  /**
   * If true, don't actually build anything, just report the the commands that would be executed.
   */
  var dryRun = false

  var showStackTracesOnFailures = true

  def build(targetName: String): Result = build(Seq(targetName))

  def build(targetNames: Seq[String]): Result = {
    val context = Map.empty[String, Any]  // Target.Context
    def b(lastResult: Result, targs: Seq[Target]): Result = targs match {
      case Nil => lastResult
      case head +: tail =>
        val result = doBuild(context, head)
        if (result.passed) b(result, tail) else result
    }
    val result = b(Passed.default, determineTargets(targetNames))
    result match {
      case p: Passed => Log.log.info(s"Success! $p")
      case f: Failed => handleBuildError(f)
    }
    result
  }

  protected def determineTargets(targetNames: Seq[String]): Seq[Target] =
    determineBuildOrder(targetNames).distinct

  protected def determineBuildOrder(targetNames: Seq[String]): Seq[Target] = {
    targetNames.foldLeft(Vector.empty[Target]) { (allTargs, t) =>
      Target.targets.get(t) match {
        case None => Exit.error("No target "+t+" found!")
        case Some(targ) =>
          // Put dependencies before targ!
          allTargs ++ determineBuildOrder(targ.dependencies) :+ targ
      }
    }
  }

  protected def doBuild(context: Target.Context, target: Target): Result = {
    log(logLevel, "building "+target.name)
    try {
      target.build(context)
    } catch {
      case _root_.scala.util.control.NonFatal(ex) => Failed.exception(s"target ${target.name}", ex)
    }
  }

  /**
   * Create a target with a sequence of dependencies and actions.
   * Example: target(name, dependencies = Seq(dep1, dep2, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target(name: String, dependencies: Seq[String] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Target =
    Target(name, dependencies, actions)

  /**
   * Create a target with a sequence of dependencies and actions.
   * Example: target('name -> Seq('a, 'b), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target(name_dependencies: (String, Seq[String]), actions: Seq[Target.Action] = Vector.empty): Target =
    Target(name_dependencies, actions)

  /**
   * Create one or more targets, passed in as a sequence of Strings, with the same
   * sequence of dependencies and actions.
   * Example: target(names = Seq(name1, ...), dependencies = Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target(names: Seq[String], dependencies: Seq[String] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Vector[Target] =
    Target(names, dependencies, actions)

  /**
   * Create one or more targets, passed in as a sequence of Symbols, Strings,
   * or tuples, (name -> sequence of dependency names).
   * Example: target(names_dependencies = (Seq(name1, ...) -> Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target(names_dependencies: (Seq[String], Seq[String]), actions: Seq[Target.Action] = Vector.empty): Vector[Target] =
    Target(names_dependencies, actions)

  private def handleBuildError(failed: Failed) {
    Console.err.println(s"Build failure: $failed")
    failed.cause match {
      case Some(cause) if showStackTracesOnFailures => cause.printStackTrace(log.out)
      case _ => // do nothing
    }
    Exit.error("Build failed")
  }
}

object Project {
  def apply() = new Project()
}
