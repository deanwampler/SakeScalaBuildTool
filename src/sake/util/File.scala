package sake.util

import java.io.{File => JFile, FilenameFilter => JFilenameFilter}
import scala.util.matching.Regex

/** 
 * A wrapper around Java's file, to better support testing.
 */
trait File {
    def exists: Boolean
    def isDirectory: Boolean
    def isFile: Boolean
    def contents: List[String]
    def contentsFilteredBy(nameFilter: String): List[String]
}

class FileFilter(val nameFilter: String) extends JFilenameFilter {
     val regex = nameFilter.replaceAll("\\*", ".*").replaceAll("\\?", ".").r

     def accept(dir: JFile, name: String) = acceptName(name, regex)

     protected def acceptName(name: String, filterRegex: Regex) = {
         regex findFirstIn name match {
             case None => false
             case Some(n) => true
         }
     }
}

class JavaFileWrapper(val path: String) extends File {
    val file = new JFile(path)
    
    def exists = file.exists()
    
    def isDirectory = file.isDirectory()
    
    def isFile = file.isFile()
    
    def contents = file.list().toList
    
    def contentsFilteredBy(nameFilter: String) = file.list(new FileFilter(nameFilter)).toList
}

