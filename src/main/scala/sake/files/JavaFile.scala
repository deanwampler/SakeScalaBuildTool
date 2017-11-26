package sake.files

import java.io.{File => JFile, FilenameFilter => JFilenameFilter,
    InputStreamReader  => JInputStreamReader,  FileInputStream  => JFileInputStream,
    OutputStreamWriter => JOutputStreamWriter, FileOutputStream => JFileOutputStream }

case class JavaFile(path: String) extends File {

  @transient val javaFile = new JFile(path)

  def exists: Boolean = javaFile.exists()

  def isDirectory: Boolean = javaFile.isDirectory()

  def isFile: Boolean = javaFile.isFile()

  def getName: String = javaFile.getName()
  def getPath: String = javaFile.getPath()

  def toJavaFile(jfw: JavaFile): JFile = jfw.javaFile

  def / (child: String): File = JavaFile(path, child)

  def contents: Seq[File] = {
    val ary = javaFile.list()
    if (ary == null) Vector.empty else ary.toVector.map(f => File(f))
  }

  def contentsFilteredBy(nameFilter: String): Seq[File] = {
    val ary = javaFile.list(new FileFilter(nameFilter))
    if (ary == null) Vector.empty else ary.toVector.map(f => File(f))
  }

  def createNewFile = javaFile.createNewFile()

  def writer = new JOutputStreamWriter(new JFileOutputStream(path))

  def reader = new JInputStreamReader(new JFileInputStream(path))

  def mkdir  = javaFile.mkdir()
  def mkdirs = javaFile.mkdirs()
  def delete = javaFile.delete()
}

object JavaFile {
  def apply(parent: File, name: String): JavaFile = new JavaFile(File.makePath(parent, name))
  def apply(elements: String*): JavaFile = new JavaFile(File.makePath(elements :_*))
}
