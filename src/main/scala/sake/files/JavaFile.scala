package sake.files

import java.io.{File => JFile, FilenameFilter => JFilenameFilter,
    InputStreamReader  => JInputStreamReader,  FileInputStream  => JFileInputStream,
    OutputStreamWriter => JOutputStreamWriter, FileOutputStream => JFileOutputStream }

case class JavaFile(path: String) extends File {

  @transient val javaFile = new JFile(path)

  def exists = javaFile.exists()

  def isDirectory = javaFile.isDirectory()

  def isFile = javaFile.isFile()

  def getPath = javaFile.getPath()

  def toJavaFile(jfw: JavaFile): JFile = jfw.javaFile

  def / (child: String): File = JavaFile(path, child)

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

object JavaFile {
  def apply(parent: File, name: String): JavaFile = new JavaFile(File.makePath(parent, name))
  def apply(elements: String*): JavaFile = new JavaFile(File.makePath(elements :_*))
}
