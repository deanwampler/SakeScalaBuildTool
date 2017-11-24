package sake.context

import sake.files.{File, Path}

/**
 * Convenient access to Java system properties.
 */
object Properties {

  /**
   * It appears that the "java.class.path" does not include the environments
   * CLASSPATH by default. If this variable is true, we combine the two when
   * the `classpath` method is called.
   */
  var combineCLASSPATHandSystemCLASSPATH = true

  /**
   * Returns a system property or "" if not defined. If you would prefer null for
   * not defined, use System.getProperty() directory.
   */
  def getSystemProperty(key:String): String = System.getProperty(key) match {
      case null => ""
      case s    => s
  }

  def setSystemProperty(key:String, value:String): Unit = System.setProperty(key, value)

  /**
   * Returns the system environment, as an immutable Scala Map.
   */
  def getSystemProperties: Map[String,String] = {
    val map = scala.collection.mutable.HashMap.empty[String,String]
    val iter = System.getenv().entrySet.iterator
    while (iter.hasNext) {
      val entry = iter.next
        map += entry.getKey -> entry.getValue
    }
    map.toMap
  }

  /**
   * The path separator, for convenience.
   */
  def pathSeparator = Properties.getSystemProperty("path.separator")

  /**
   * The file separator.
   */
  def fileSeparator = Properties.getSystemProperty("file.separator")

  /**
   * The line separator.
   */
  def lineSeparator = Properties.getSystemProperty("line.separator")

  /**
   * The current working directory.
   */
  def cwd = Properties.getSystemProperty("user.dir")

  /**
   * For convenience and to keep the system classpath consistent with any user changes,
   * the "classpath" is exposed explicitly as a List that is kept synchronized with the
   * system's value. Use standard list operations to change it.
   * We have also found it necessary to merge the environment variable CLASSPATH
   * with the property, which apparently isn't done by the JVM by default.
   */
  def classpath: Path[File] = {
      val cp = if (Properties.combineCLASSPATHandSystemCLASSPATH) {
        Properties.getSystemProperty("java.class.path") +
                pathSeparator + sys.env.getOrElse("CLASSPATH","")
      } else {
        Properties.getSystemProperty("java.class.path")
      }
      val seq = for {
          s <- cp.split(pathSeparator)
      } yield s
      Path(seq.distinct.map(x => File(x)).toVector, pathSeparator)
  }

  /**
   * Set the system classpath.
   */
  def classpath_=(newPath: Path[File]) = {
      Properties.setSystemProperty("java.class.path", newPath.toString)
  }
}
