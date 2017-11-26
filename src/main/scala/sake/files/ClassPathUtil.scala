package sake.files

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
   * Return the system CLASSPATH separator.
   */
  val separator: String = sys.props("path.separator")

  /**
   * It can be useful to merge the environment variable CLASSPATH with the property,
   * which isn't done by the JVM by default. If `combineCLASSPATHandSystemCLASSPATH`
   * is true, the returned CLASSPATH merges these values.
   */
  def classpath: Path[File] = {
    val cp1 = sys.props("java.class.path").trim
    val cp2 = if (combineCLASSPATHandSystemCLASSPATH) sys.env.getOrElse("CLASSPATH","").trim else ""
    val cp = (cp1, cp2) match {
      case ("",  "") => ""
      case (c1, "")  => c1
      case ("",  c2) => c2
      case (c1,  c2) => c1 + separator + c2
    }
    val vector = cp.split(separator).toVector.distinct.filter(_.trim.length > 0).map(p => File(p))
    Path(vector, separator)
  }
}
