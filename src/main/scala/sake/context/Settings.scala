package sake.context

import sake.files.{File, FilesFinder, JavaFilesFinder, Path}
import sake.util.Log

/**
 * Defines convenient variables for builds.
 * @param version          Version string for this project.
 * @param scalaVersion     Version of Scala.
 * @param srcDirs          One or more directories where source files are find.
 * @param srcDirs          One or more directories where source files are find.
 * @param testDirs         One or more directories where test source files are find.
 * @param libDirs          One or more directories where libraries are find that aren't resolved with Ivy or Maven.
 * @param targetDir        Directory where compiled class files and archives are written.
 * @param classPathPrefix  A Path for the CLASSPATH that is put before any elements resolved using Ivy or Maven.
 *                         Defaults to any jar in the libDirs (except for source jars: "*src.jar") and any class files in targetDir.
 * @param showStackTracesOnFailures If true (default), show stack traces when a failure happens.
 * @param logThreshold     Logging level: Info (default), Notice, Warn, Error, Failure
 * @param userSettings     Key-value map of settings for your own use.
 */
case class Settings(
  version: String,
  scalaVersion: String,
  srcDirs: Vector[File],
  testDirs: Vector[File],
  libDirs: Vector[File],
  targetDir: File,
  classPathPrefix: Path[File],
  dryRun: Boolean,
  logThreshold: Log.Level.Value,
  showStackTracesOnFailures: Boolean,
  val userSettings: Map[String,Any]
)

object Settings {
  val default = Settings()

  def apply(
    version: String = "2.0",
    scalaVersion: String = "2.12.4",
    srcDirs: Vector[File]  = Vector(File("src/main/scala")),
    testDirs: Vector[File] = Vector(File("src/test/scala")),
    libDirs: Vector[File]  = Vector(File("lib/")),
    targetDir: File        = File("target/"),
    classPathPrefix: Path[File] = pathArgPlaceholder,
    dryRun: Boolean = false,
    logThreshold: Log.Level.Value = Log.Level.Info,
    showStackTracesOnFailures: Boolean = true,
    userSettings: Map[String,Any] = Map.empty): Settings = {

    // Compute the default prefix if the placeholder instance was provided!
    val prefix =
      if (classPathPrefix eq pathArgPlaceholder) finder.find(libDirs, targetDir)
      else classPathPrefix

    new Settings(
      version, scalaVersion,
      srcDirs, testDirs, libDirs, targetDir, prefix,
      dryRun, logThreshold, showStackTracesOnFailures, userSettings)
  }

  // Override for mocking.
  protected[context] var finder: DefaultClassPathPrefixFinder = DefaultJavaClassPathPrefixFinder

  val pathArgPlaceholder = Path[File]()
}

/**
 * Trait to encapsulate building the default CLASSPATH prefix. To facilitate
 * unit testing, the file finder object is pluggable through overriding.
 */
trait DefaultClassPathPrefixFinder {
  def find(libDirs: Seq[File], targetDir: File): Path[File] = {
    val finder = makeFinder()
    val files = (finder(libDirs.map(_ + Properties.fileSeparator + "*.jar")) diff
     finder(libDirs.map(_ + Properties.fileSeparator + "*src.jar"))) :+ targetDir
    Path(files, Properties.fileSeparator)
  }

  protected def makeFinder(): FilesFinder
}

object DefaultJavaClassPathPrefixFinder extends DefaultClassPathPrefixFinder {
  protected def makeFinder(): FilesFinder = JavaFilesFinder
}
