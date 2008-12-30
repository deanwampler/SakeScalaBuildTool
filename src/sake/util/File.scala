package sake.util

import java.io.{File => JFile, FilenameFilter => JFilenameFilter, 
    Reader => JReader, Writer => JWriter}
import scala.util.matching.Regex
import sake.environment._

/** 
 * A wrapper around Java's file, to better support mocking.
 */
trait File {
    val path:String
    if (path.length == 0) throw new BuildError("File path can't be an empty string.")
    def exists: Boolean
    def isDirectory: Boolean
    def isFile: Boolean
    def contents: Option[List[String]]
    def contentsFilteredBy(nameFilter: String): Option[List[String]]
    
    def createNewFile: Boolean
    
    /**
     * Create a directory with this name and any of its parent directories that don't
     * already exist.
     */
    def mkdirs: Boolean
    
    /**
     * Delete the File. If it is a non-empty directory, this operation will fail.
     */
    def delete: Boolean
    
    /**
     * Delete the File or directory, recursively. If it is a non-empty directory, it will delete
     * all the contents first.
     */
    def deleteRecursively:Boolean = {
        if (isDirectory) {
            contents match {
                case None => true
                case Some(files) => files.foreach { f => 
                    if (makeFile(path, f).deleteRecursively == false)
                        return false
                }
            }
        }
        delete
    }
    
    /**
     * Return a java.io.Writer for writing new content to this file.
     * Will throw an exception if it can't open the stream. 
     * @see java.io.PrintStream.
     */
    def writer: JWriter
    
    /**
     * Return a java.io.Reader for reading the content from this file.
     * Will throw an exception if it can't open the stream. 
     * @see java.io.BufferedReader.
     */
    def reader: JReader
    
    // These methods were extracted to facilitate creation of test doubles.
    protected def makeFile(path: String):File = File(path)
    protected def makeFile(parent: String, child: String):File = File(parent, child)
}

object File {
    def apply(path: String) = new JavaFileWrapper(path)
    def apply(parent: String, child: String) = new JavaFileWrapper(parent, child)
    
    implicit def toJavaFile(jfw: JavaFileWrapper): JFile = jfw.javaFile
    
    def makePath(prefix: String, elem: String) = prefix match {
        case "" => elem
        case _  => prefix + Environment.environment.fileSeparator + elem
    }
}

class FileFilter(val nameFilter: String) extends JFilenameFilter {

     val regex = ("^"+nameFilter.replaceAll("\\*", ".*").replaceAll("\\?", ".")+"$").r

     def accept(dir: JFile, name: String): Boolean = acceptName(name, regex)

     protected def acceptName(name: String, filterRegex: Regex) = {
         regex findFirstIn name match {
             case None => false
             case Some(n) => true
         }
     }
}
