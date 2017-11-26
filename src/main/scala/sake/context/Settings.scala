package sake.context

import sake.files.{ClassPathUtil, Path, File, FilesFinder, JavaFilesFinder}
import sake.util.Log

/**
 * Defines convenient variables for builds.
 * @param version          Version string for this project.
 * @param scalaVersion     Version of Scala.
 * @param srcDirs          One or more directories where source files are found.
 * @param testDirs         One or more directories where test source files are found.
 * @param libDirs          One or more directories where libraries are found that aren't resolved with Ivy or Maven.
 * @param targetDir        Directory where compiled class files and archives are written.
 * @param classPathPrefix  A Path for the CLASSPATH that is put before any elements resolved using Ivy or Maven.
 *                         Defaults to any jar in the libDirs (except for source jars: "*src.jar") and any class files in targetDir.
 * @param dryRun           Don't actually run the commands.
 * @param logThreshold     Logging level: Info (default), Notice, Warn, Error, Failure
 * @param showStackTracesOnFailures If true (default), show stack traces when a failure happens.
 * @param custom           Key-value map of settings for your own use.
 */
case class Settings(
  version: String = "2.0",
  scalaVersion: String = "2.12.4",
  srcDirs: Vector[File]  = Vector(File("src/main/scala")),
  testDirs: Vector[File] = Vector(File("src/test/scala")),
  libDirs: Vector[File]  = Vector(File("lib/")),
  targetDir: File        = File("target/"),
  classPathPrefix: Path[File] = ClassPathUtil.classpath,
  dryRun: Boolean = false,
  logThreshold: Log.Level.Value = Log.Level.Info,
  showStackTracesOnFailures: Boolean = true,
  custom: Map[String,Any] = Map.empty)

object Settings {
  lazy val default = new Settings()
}

/**
 * Trait to encapsulate building the default CLASSPATH prefix. To facilitate
 * unit testing, the file finder object is pluggable through overriding.
 */
trait DefaultClassPathPrefixFinder {
  def find(libDirs: Seq[File], targetDir: File): Path[File] = {
    val finder = makeFinder()
    val files = (finder(libDirs.map(_ + File.separator + "*.jar")) diff
     finder(libDirs.map(_ + File.separator + "*src.jar"))) :+ targetDir
    Path(files, File.separator)
  }

  protected def makeFinder(): FilesFinder
}

object DefaultJavaClassPathPrefixFinder extends DefaultClassPathPrefixFinder {
  protected def makeFinder(): FilesFinder = JavaFilesFinder
}
