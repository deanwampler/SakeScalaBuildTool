package sake

// import scala.collection.immutable._
import sake.command.builtin.Commands
import sake.command.{Result, Passed, Failed}
import sake.context.{Context, Settings}
import sake.target.{Target, TargetVector}
import sake.util.{Exit, Log}
import sake.files.{File, FilesFinder, JavaFilesFinder}

/**
* The Project class defines actual build scripts.
*/
class Project(val name: String) extends Commands {

  Project.add(this)

  var settings: Settings = Settings.default
  var log: Log = Log.default
  var logLevel: Log.Level.Value = Log.Level.Info

  var showStackTracesOnFailures = true

  /** Track all definitions for a target */
  def targets: Map[String, Target[_]] = allTargets

  protected var allTargets: Map[String, Target[_]] = Map.empty

  /** The user asks to build targets. Verify they exist and build them in order! */
  def build(targetNames: String*): Result = {
    def b(lastResult: Result, targs: Seq[Target[_]]): Result = targs match {
      case Nil => lastResult
      case head +: tail =>
        val result = doBuild(head, settings)
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

  object implicits {
    import scala.language.implicitConversions

    /**
     * An implicit conversion that allows you to use the Scala process API, where
     * integer exit codes are returned, as actions for targets. This function lifts
     * the exit code to a Result.
     */
    implicit def exitCodeToResult(code: Int): Result =
      if (code == 0) Passed.default
      else Failed(code, s"Command failed, returned exit code $code")

    /**
     * Marker for an allowed type of target.
     * Provides a name method to implement a consistent way for extracting target
     * names from instances of type T.
     */
    trait TargetKind[T] {
      def name(t: T): String
    }

    /** Marker to allow a String as a target "value". Also used to construct a name consistently. */
    implicit object StringTargetKind extends TargetKind[String] {
      def name(s: String): String = s
    }

    /** Marker to allow a Symbol as a target "value". Also used to construct a name consistently. */
    implicit object SymbolTargetKind extends TargetKind[Symbol] {
      def name(s: Symbol): String = s.toString
    }

    /** Marker to allow a File as a target "value". Also used to construct a name consistently. */
    implicit object FileTargetKind extends TargetKind[File] {
      def name(f: File): String = f.getName
    }

    // These conversions are used to auto-convert dependencies specified as
    // strings, symbols, files, etc.

    implicit def toTarget(name: String): Target[String] = makeTarget[String](StringTargetKind, name)
    implicit def toTarget(name: Symbol): Target[Symbol] = makeTarget[Symbol](SymbolTargetKind, name)
    implicit def toTarget(f: File):      Target[File]   = makeTarget[File]  (FileTargetKind,   f)
  }
  import implicits._

  protected def determineTargets(targetNames: Seq[String]): Either[String, Seq[Target[_]]] = {
    def buildOrder(targetNames: Seq[Target[_]]): Seq[Target[_]] = {
      targetNames.foldLeft(Vector.empty[Target[_]]) { (allTargs, t) =>
        val target = allTargets(t.name) // we already know it's there, so no need to verify.
        // It might seem unsafe to call buildOrder and not determineTargets; how
        // do we know the dependencies exist? They do, because they are defined
        // already as targets. determineTargets is called when the user invokes
        // the build, so it catches user errors at build time.
        // Put dependencies before the target!
        allTargs ++ buildOrder(target.dependencies) :+ target
      }
    }

    val (targets, unknowns) = targetNames.foldLeft(Vector.empty[Target[_]], Vector.empty[String]) {
      case ((targets, unknowns), name) => allTargets.get(name) match {
        case None => println(s"$name not found"); (targets, unknowns :+ name)
        case Some(target) => println(s"$name found"); (targets :+ target, unknowns)
      }
    }
    unknowns.size match {
      case 0 => Right(buildOrder(targets).distinct)
      case 1 => Left(s"Project $name: unknown target: ${unknowns.head} (known targets: ${targets.mkString("(\n  ", ",\n  ", ")\n")})")
      case _ => Left(s"Project $name: unknown targets: ${unknowns.mkString(", ")} (known targets: ${targets.mkString("(\n  ", ",\n  ", ")\n")})")
    }
  }

  protected def doBuild[T](target: Target[T], settings: Settings): Result = {
    val context = Context[T](target, settings, Map.empty[String, Any])  // TODO what should go in the map?
    log(logLevel, "building "+target.name)
    try {
      target.build(context)
    } catch {
      case scala.util.control.NonFatal(ex) => Failed.exception(s"target ${target.name}", ex)
    }
  }

  /** Put at the end of an action to force it to succeed. */
  def success: Result = Passed.default

  /** Use as a noop action, although that's the default behavior if none is specified. */
  def done[T]: Context[T] => Result = _ => Passed.default

  /** Return the files in one or more directories. */
  def find(parent: File, parents: File*): Seq[File] =
    JavaFilesFinder.find(parent +: parents)

  /** Return the files in one or more directories. */
  def find(parents: Seq[File]): Seq[File] =
    JavaFilesFinder.find(parents)

  /** Return the files in a directory that match a glob pattern. */
  def find(parent: File, filePattern: String): Seq[File] =
    JavaFilesFinder.find(parent, filePattern)

  /** Return the files in one or more directories that match a glob pattern. */
  def find(parents: Seq[File], filePattern: String): Seq[File] =
    JavaFilesFinder.find(parents, filePattern)

  /** Return the files in a directory, recursively. */
  def findRecursive(parent: File, parents: File*): Seq[File] =
    JavaFilesFinder.findRecursive(parent +: parents)

  /** Return the files in one or more directories, recursively. */
  def findRecursive(parents: Seq[File]): Seq[File] =
    JavaFilesFinder.findRecursive(parents)

  /** Return the files in a directory, recursively, that match a glob pattern. */
  def findRecursive(parent: File, filePattern: String): Seq[File] =
    JavaFilesFinder.findRecursive(parent, filePattern)

  /** Return the files in one or more directories, recursively, that match a glob pattern. */
  def findRecursive(parents: Seq[File], filePattern: String): Seq[File] =
    JavaFilesFinder.findRecursive(parents, filePattern)

  // Define methods the user would typically use in a build file to create targets.
  // The assumption is that the actions wouldn't be passed as arguments, as in the
  // Target constructor, but instead would be defined using the Target#apply method.
  // Also, the dependencies are added using Target#dependencies or related methods.

  /**
   * Create targets with no dependencies (they can be added later).
   * If followed by an action literal, it will be applied to all the targets.
   * Example: target(name1, name2) { action }
   * @return a TargetVector with the new Targets. Also updates the internal map of targets.
   */
  def target[T : TargetKind](value: T, values: T*): TargetVector[T] =
    makeTargets(implicitly[TargetKind[T]], value +: values)

  /**
   * Create one or more targets, passed in as a sequence, with no dependencies.
   * If followed by an action literal, it will be applied to all the targets.
   * Example: target(names = Seq(name1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   */
  def target[T : TargetKind](values: Seq[T]): TargetVector[T] =
    makeTargets(implicitly[TargetKind[T]], values)

  /**
   * Ensure that one or more directories exist, but fail if the parent doesn't exist
   * or the creation fails.  The process stops on the first failure.
   * Dependencies can be added afterwards.
   * Example: mkdir(File(dirName))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdir(directory: File, directories: File*): TargetVector[File] =
    makeTargets(FileTargetKind, directory +: directories, postActions = mkdirsAction(false))

  /**
   * Ensure that one or more directories exist, with no dependencies, but fail
   * if a parent doesn't exist, or if a creation fails. The process stops on the first failure.
   * Dependencies can be added afterwards.
   * Example: mkdir(Seq(File(dirName1), File(dirName2)))
   * @see mkdirs, which creates parents if necessary.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdir(directories: Seq[File]): TargetVector[File] =
    makeTargets(FileTargetKind, directories, postActions = mkdirsAction(false))

  /**
   * Ensure that one or more directories exist. Create the parents if they don't
   * exist. Fail if any creation step fails. The process stops on the first failure.
   * Dependencies can be added afterwards.
   * Example: mkdirs(File(dirName))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdirs(directory: File, directories: File*): TargetVector[File] =
    makeTargets(FileTargetKind, directory +: directories, postActions = mkdirsAction(true))

  /**
   * Ensure that one or more directories exist. Create the parents if they don't
   * exist. Fail if any creation step fails. The process stops on the first failure.
   * Dependencies can be added afterwards.
   * Example: mkdirs(Seq(File(dirName1), File(dirName2)))
   * @see mkdir, which fails if the parents don't exist.
   * @return the new Target. Also updates the internal map of targets.
   */
  def mkdirs(directories: Seq[File]): TargetVector[File] =
    makeTargets(FileTargetKind, directories, postActions = mkdirsAction(true))


  /**
   * Create one or more files, passed in as a sequence, with no dependencies.
   * If an action literal is present, it is applied to all of them.
   * After the actions are completed, the files must exist or the build fails.
   * Example: file(afile) { action }
   * @return  the new Target. Also updates the internal map of targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(f: File, files: File*): TargetVector[File] =
    makeTargets(FileTargetKind, f +: files, postActions = fileExistsAction())

  /**
   * Create one or more files, passed in as a sequence, with no dependencies.
   * If an action literal is present, it is applied to all of them.
   * After the actions are completed, the files must exist or the build fails.
   * Example: file(Seq(file1, ...)) { action }
   * @return Vector[Target] of the new Targets.
   * @note No nesting of dependencies and their dependencies is supported! Use a separate target invocation.
   */
  def file(files: Seq[File]): TargetVector[File] =
    makeTargets(FileTargetKind, files, postActions = fileExistsAction())

  /**
   * Delete one or more files or directories. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(File(dirName))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   */
  def clean(file: File, files: File*): TargetVector[File] =
    makeTargets(FileTargetKind, file +: files, actions = cleanAction())

  /**
   * Delete one or more targets, which have no dependencies. For directories, it deletes
   * recursively. It stops immediately on the first failure, if any.
   * Example: clean(Seq(File(dirName1), File(dirName2)))
   * @return Vector[Target] of the new Targets. Also updates the internal map of targets.
   */
  def clean(files: Seq[File]): TargetVector[File] =
    makeTargets(FileTargetKind, files, actions = cleanAction())

  protected def makeTargets[T](
    kind: TargetKind[T],
    values: Seq[T],
    dependencies: Seq[Target[_]] = Vector.empty,
    actions: Seq[Context[T] => Result] = Vector.empty,
    preActions: Seq[Context[T] => Result] = Vector.empty,
    postActions: Seq[Context[T] => Result] = Vector.empty): TargetVector[T] =
    TargetVector(values.toVector map { value =>
      makeTarget(kind, value, dependencies, actions, preActions, postActions)
    })

  protected def makeTarget[T](
    kind: TargetKind[T],
    value: T,
    dependencies: Seq[Target[_]] = Vector.empty,
    actions: Seq[Context[T] => Result] = Vector.empty,
    preActions: Seq[Context[T] => Result] = Vector.empty,
    postActions: Seq[Context[T] => Result] = Vector.empty): Target[T] = {
    val t = Target[T](kind.name(value), value, dependencies, actions, preActions, postActions)
    registerTarget(t)
  }

  protected def fileExistsAction(): Vector[Context[File] => Result] =
    fileAction("File %s was not created")((f: File) => f.exists)

  protected def mkdirsAction(createParents: Boolean): Vector[Context[File] => Result] = {
    val make = if (createParents) (f: File) => f.mkdirs else (f: File) => f.mkdir
    fileAction(s"Failed to create directory %s (create parents? $createParents)")(make)
  }

  protected def cleanAction(): Vector[Context[File] => Result] =
    fileAction("Failed to delete %s")((f: File) => f.deleteRecursively)

  // Create a vector of actions. The errorMessageFormat must contain one "%s" for the file name.
  protected def fileAction(errorMessageFormat: String)(f: File => Boolean): Vector[Context[File] => Result] =
    Vector((c:Context[File]) => {
      val file = c.target.value
      booleanToResult(f(file), errorMessageFormat.format(file))
    })


  protected def booleanToResult(b: Boolean, errorMessage: String): Result =
    if (b) Passed.default else Failed(1, errorMessage)

  /**
   * Register the target in the project-wide list. Note that if it has to be merged
   * with an existing target and the merge fails, then the build aborts immediately.
   */
  protected def registerTarget[T](target: Target[T]): Target[T] =
    allTargets.get(target.name) match {
      case None =>
        allTargets += (target.name -> target)
        target
      case Some(t2) =>
        target.merge(t2) match {
          case Right(t) =>
            allTargets += (target.name -> t)
            t
          case Left(error) => Exit.fatal(error)
        }
    }

  private def reportSuccess(passed: Passed): Unit =
    Log.info(s"Success! $passed")

  private def reportBuildError(failed: Failed): Unit = {
    Log.error(s"Build failure: $failed")
    failed.cause match {
      case Some(cause) if showStackTracesOnFailures => cause.printStackTrace(log.out)
      case _ => // do nothing
    }
  }
}

object Project {
  var all: Map[String, Project] = Map.empty

  def add(p: Project): Unit =
    if (all.contains(p.name)) {
      Exit.fatal(s"Projects must have unique names. This name: ${p.name}. Projects already defined: ${all.keySet.mkString(", ")}")
    } else {
      all += (p.name -> p)
    }
}
