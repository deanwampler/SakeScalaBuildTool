package sake.util

import sake.files.{File, Path}

/**
 * Utility to help with CLASSPATHs.
 */
object ClassPathUtil {

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
   * It can be useful to merge the environment variable CLASSPATH with the property,
   * which isn't done by the JVM by default. If `combineCLASSPATHandSystemCLASSPATH`
   * is true, the returned CLASSPATH merges these values.
   */
  def classpath: Path[File] = {
    val separator = sys.props("path.separator")
    val cp1 = sys.props("java.class.path").trim
    val cp2 = if (combineCLASSPATHandSystemCLASSPATH) sys.env.getOrElse("CLASSPATH","").trim else ""
    val cp = (cp1, cp2) match {
        case ("",  "") => ""
        case (c1, "")  => c1
        case ("",  c2) => c2
        case (c1,  c2) => c1 + separator + c2
      }
    } else {
      sys.props("java.class.path")
    }
    val vect = cp.split(pathSeparator).toVector.distinct.filter(_.trim.length > 0).map(p => File(p))
    Path(vector, separator)
  }
}
