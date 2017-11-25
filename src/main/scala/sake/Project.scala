package sake

// import scala.collection.immutable._
import sake.command.builtin.Commands
import sake.command.{Result, Passed, Failed}
import sake.context.{Properties, Settings}
import sake.target.Target
import sake.util.Log
import sake.files.File

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

  /** The user asks to build targets. Verify they exist and build them in order! */
  def build(targetNames: String*): Result = build(targetNames :_*)

  /** The user asks to build targets. Verify they exist and build them in order! */
  def build(targetNames: Seq[String]): Result = {
    def b(lastResult: Result, targs: Seq[Target[_]]): Result = targs match {
      case Nil => lastResult
      case head +: tail =>
        val context = Target.Context(head, Map.empty[String, Any])  // TODO add environment?
        val result = doBuild(context, head)
        if (result.passed) b(result, tail) else result
    }
    determineTargets(targetNames) match {
      case Right(targets) => b(Passed.default, targets) match {
        case p: Passed => reportSuccess(p); p
        case f: Failed => reportBuildError(f); f
      }
      case Left(error) =>
        val f = Failed(1, error)
        reportBuildError(f)
        f
    }
  }

  protected def determineTargets(targetNames: Seq[String]): Either[String, Seq[Target[_]]] = {
    def buildOrder(targetNames: Seq[Target[_]]): Seq[Target[_]] = {
      targetNames.foldLeft(Vector.empty[Target[_]]) { (allTargs, t) =>
        val target = Target.targets(t.name) // we already know it's there, so no need to verify.
        // It might seem unsafe to call buildOrder and not determineTargets; how
        // do we know the dependencies exist? They do, because they are defined
        // already as targets. determineTargets is called when the user invokes
        // the build, so it catches user errors at build time.
        // Put dependencies before the target!
        allTargs ++ buildOrder(target.dependencies) :+ target
      }
    }

    val (targets, unknowns) = targetNames.foldLeft(Vector.empty[Target[_]], Vector.empty[String]) {
      case ((targets, unknowns), name) => Target.targets.get(name) match {
        case None => (targets, unknowns :+ name)
        case Some(target) => (targets :+ target, unknowns)
      }
    }
    unknowns.size match {
      case 0 => Right(buildOrder(targets).distinct)
      case 1 => Left(s"Unknown target: ${unknowns.head}")
      case _ => Left(s"Unknown targets: ${unknowns.mkString(", ")}")
    }
  }

  protected def doBuild(context: Target.Context, target: Target[_]): Result = {
    log(logLevel, "building "+target.value)
    try {
      target.build(context)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${target.name}", ex)
    }
  }

  /**
   * Create a target with a sequence of dependencies and actions.
   * Example: target(name, dependencies = Seq(dep1, dep2, ...), actions = Seq(act1, ...))
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](value: T, dependencies: Seq[Target[_]] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Target[T] =
    Target[T](value, dependencies, actions)

  /**
   * Create a target with a sequence of dependencies and actions.
   * Example: target("name" -> Seq("a", "b"), actions = Seq(act1, ...))
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](value_dependencies: (T, Seq[Target[_]]), actions: Seq[Target.Action] = Vector.empty): Target[T] =
    Target[T](value_dependencies, actions)

  /**
   * Create one or more targets, passed in as a sequence, with the same sequence
   * of dependencies and actions.
   * Example: target(names = Seq(name1, ...), dependencies = Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](values: Seq[T], dependencies: Seq[Target[_]] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Vector[Target[T]] =
    Target[T](values, dependencies, actions)

  /**
   * Create one or more targets, passed in as a sequence, with the same sequence
   * of dependencies and actions.
   * Example: target(names_dependencies = (Seq(name1, ...) -> Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def target[T](values_dependencies: (Seq[T], Seq[Target[_]]), actions: Seq[Target.Action] = Vector.empty): Vector[Target[T]] =
    Target[T](values_dependencies, actions)

  /**
   * Ensure that a directory already exists, but fail if the parent doesn't exist and the
   * createParents flag is false (default is true).
   * Example: mkdir(File(dirName), dependencies = Seq(dep1, dep2, ...), createParents = false)
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def dir(directory: File, dependencies: Seq[Target[_]] = Vector.empty, createParents: Boolean = true): Target[File] =
    Target[File](directory, dependencies, mkdirsAction(Seq(directory), createParents))

  /**
   * Ensure that a directory already exists, but fail if the parent doesn't exist and the
   * createParents flag is false.
   * Example: mkdir(File(dirName) -> Seq(dep1, dep2, ...), createParents = false)
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def dir(directory_dependencies: (File, Seq[Target[_]]), createParents: Boolean = true): Target[File] =
    Target[File](directory_dependencies, mkdirsAction(Seq(directory_dependencies._1), createParents))

  /**
   * Ensure that one or more directories already exist, but fail immediately if any
   * of the parents don't exist and the createParents flag is false, or if a creation fails.
   * Example: mkdir(Seq(File(dirName1), File(dirName2)), Seq(dep1, dep2, ...), createParents = false)
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def dir(directories: Seq[File], dependencies: Seq[Target[_]] = Vector.empty, createParents: Boolean = true): Target[Seq[File]] =
    Target[Seq[File]](directories, dependencies, mkdirsAction(directories, createParents))

  /**
   * Ensure that one or more directories already exist, but fail immediately if any
   * of the parents don't exist and the createParents flag is false, or if a creation fails.
   * Example: mkdir(Seq(File(dirName1), File(dirName2)) -> Seq(dep1, dep2, ...), createParents = false)
   * @return the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def dir(directories_dependencies: (Seq[File], Seq[Target[_]]), createParents: Boolean = true): Target[File] =
    Target[File](directories_dependencies, mkdirsAction(directories_dependencies._1, createParents))


  /**
   * Create a file, based on a sequence of dependencies and actions.
   * Example: file(afile, dependencies = Seq(dep1, dep2, ...), actions = Seq(act1, ...))
   * @return  the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(file: File, dependencies: Seq[Target[_]] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Target[File] =
    Target[File](file, dependencies, actions)

  /**
   * Create a file, based on a sequence of dependencies and actions.
   * Example: file(afile -> Seq("a", "b"), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(file_dependencies: (File, Seq[Target[_]]), actions: Seq[Target.Action] = Vector.empty): Target[File] =
    Target[File](file_dependencies, actions)

  /**
   * Create one or more files, passed in as a sequence, with the same sequence of
   * dependencies and actions.
   * Example: file(Seq(file1, ...), dependencies = Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files: Seq[File], dependencies: Seq[Target[_]] = Vector.empty, actions: Seq[Target.Action] = Vector.empty): Vector[Target[File]] =
    Target[File](files, dependencies, actions)

  /**
   * Create one or more files, passed in as a sequence, with the same sequence of
   * dependencies and actions.
   * Example: file((Seq(file1, ...) -> Seq(dep1, ...), actions = Seq(act1, ...))
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files_dependencies: (Seq[File], Seq[Target[_]]), actions: Seq[Target.Action] = Vector.empty): Vector[Target[File]] =
    Target[File](files_dependencies, actions)

  /**
   * Delete the target, after satisfying the dependencies. For a directory, it deletes
   * recursively.
   * Example: clean(File(dirName), dependencies = Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directory: File, dependencies: Seq[Target[_]] = Vector.empty): Target[File] =
    Target[File](directory, dependencies, cleanAction(Seq(directory)))

  /**
   * Delete the target, after satisfying the dependencies. For a directory, it deletes
   * recursively.
   * Example: clean(File(dirName) -> Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directory_dependencies: (File, Seq[Target[_]])): Target[File] =
    Target[File](directory_dependencies, cleanAction(Seq(directory_dependencies._1)))

  /**
   * Delete one or more targets, after satisfying the dependencies. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(Seq(File(dirName1), File(dirName2)), Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directories: Seq[File], dependencies: Seq[Target[_]] = Vector.empty): Target[File] =
    Target[File](directories, dependencies, cleanAction(directories))

  /**
   * Delete one or more targets, after satisfying the dependencies. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(Seq(File(dirName1), File(dirName2)) -> Seq(dep1, dep2, ...))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def clean(directories_dependencies: (Seq[File], Seq[Target[_]])): Target[File] =
    Target[File](directories_dependencies, cleanAction(directories_dependencies._1))

  import Target.Context

  protected def mkdirsAction(files: Seq[File], createParents: Boolean): Vector[Context => Result] = {
    val md = if (createParents) (f: File) => f.mkdirs else (f: File) => f.mkdir
    def mds(fs: Seq[File], r: Result): Result = fs match {
      case Nil => r
      case head +: tail =>
        if (md(head)) mds(tail, r)
        else Failed(1, s"Failed to create directory $head (create parents? $createParents)")
    }

    Vector((c:Context) => mds(files, Passed.default))
  }

  protected def cleanAction(files: Seq[File]): Vector[Context => Result] = {
    def cs(fs: Seq[File], r: Result): Result = fs match {
      case Nil => r
      case head +: tail =>
        if (head.deleteRecursively) cs(tail, r)
        else Failed(1, s"Failed to delete $head")
    }

    Vector((c:Context) => cs(files, Passed.default))
  }

  private def reportSuccess(passed: Passed): Unit =
    Log.log.info(s"Success! $passed")

  private def reportBuildError(failed: Failed): Unit = {
    Log.log.error(s"Build failure: $failed")
    failed.cause match {
      case Some(cause) if showStackTracesOnFailures => cause.printStackTrace(log.out)
      case _ => // do nothing
    }
  }
}

object Project {
  def apply() = new Project()
}
