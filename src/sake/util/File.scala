package sake.util

import java.io.{File => JFile}

/** 
 * A wrapper around Java's file, to better support testing.
 */
trait File {
    def exists: Boolean
    def isDirectory: Boolean
    def isFile: Boolean
    def contents: List[String]
}

class JavaFileWrapper(val path: String) extends File {
    val file = new JFile(path)
    
    def exists = file.exists()
    
    def isDirectory = file.isDirectory()
    
    def isFile = file.isFile()
    
    def contents = file.list().toList
}

