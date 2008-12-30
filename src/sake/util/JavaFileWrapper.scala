package sake.util

import java.io.{File => JFile, FilenameFilter => JFilenameFilter, 
    InputStreamReader  => JInputStreamReader,  FileInputStream  => JFileInputStream,
    OutputStreamWriter => JOutputStreamWriter, FileOutputStream => JFileOutputStream }

class JavaFileWrapper(override val path: String) extends File {

    def this(parent: String, child: String) = this(File.makePath(parent, child))
    
    var javaFile = new JFile(path)
    
    def exists = javaFile.exists()
    
    def isDirectory = javaFile.isDirectory()
    
    def isFile = javaFile.isFile()
    
    def contents = {
        val ary = javaFile.list()
        if (ary == null) None else Some(ary.toList)
    }
    
    def contentsFilteredBy(nameFilter: String) = {
        val ary = javaFile.list(new FileFilter(nameFilter))
        if (ary == null) None else Some(ary.toList)
    }
    
    def createNewFile = javaFile.createNewFile()
    
    def writer = new JOutputStreamWriter(new JFileOutputStream(path))
    
    def reader = new JInputStreamReader(new JFileInputStream(path))

    def mkdirs = javaFile.mkdirs()
    def delete = javaFile.delete()
}
