package sake.util

import java.io.{StringReader => JStringReader, StringWriter => JStringWriter}

class FakeFile(val path: String, exists2: Boolean, 
        val isDirectory2: Boolean, val contents2: List[String]) extends File {

    def this (path: String) = this(path, false, false, Nil)

    def exists = exists2
    def isDirectory = isDirectory2
    def isFile = !isDirectory2
    def contents = Some(contents2)
    def contentsFilteredBy(nameFilter: String) = contents
    
    def createNewFile = false
    def mkdirs = isDirectory
    def delete = true

    val stringWriter = new JStringWriter()
    def writer = stringWriter

    def stringForReading = stringWriter.getBuffer().toString()
    def reader = new JStringReader(stringForReading)

    override def makeFile(path: String) = new FakeFile(path)
    override def makeFile(parent: String, child: String) = new FakeFile(File.makePath(parent, child))
    
    override def toString() = {
        "FakeFile: exists: "+exists+", isDirectory? "+isDirectory+", isFile? "+isFile+", stringForReading: \""+stringForReading+"\"."
    }
}

class FakeFileForSpecs(path: String, exists: Boolean, 
       isDirectory: Boolean, contents: List[String]) extends FakeFile(path, exists, isDirectory, contents) {

    def this (path: String) = this(path, false, false, Nil)

    override def contentsFilteredBy(nameFilter: String) = {
        val filter = new FileFilter(nameFilter)
        val unused = new java.io.File(".")
        Some(contents2.filter(filter.accept(unused, _)))
    }

    override def makeFile(path: String) = new FakeFileForSpecs(path)
    override def makeFile(parent: String, child: String) = new FakeFileForSpecs(File.makePath(parent, child))
}

object FakeFileForSpecs {
    val oneFakeFile = new FilesFinder() {
        override def makeFile(path: String) = path match {
            case "." => new FakeFileForSpecs(path, true, true, List("foo"))
            case "foo" => new FakeFileForSpecs(path, true, true, List("bar1"))
            case "foo/bar1" => new FakeFileForSpecs(path, true, true, List("ASpec.class"))
            case "foo/bar1/ASpec.class" => new FakeFileForSpecs(path, true, false, Nil)
        }
    }
        
    val fakeFilesFinder = new FilesFinder() {
        override def makeFile(path: String) = path match {
            case "." => new FakeFileForSpecs(path, true, true, List("foo"))
            case "foo" => new FakeFileForSpecs(path, true, true, List("bar1", "bar2"))
            case "foo/bar1" => new FakeFileForSpecs(path, true, true, List("A.class", "ASpec.class"))
            case "foo/bar2" => new FakeFileForSpecs(path, true, true, List("BSpec.class", "B.class", "a"))
            case "foo/bar1/A.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar1/ASpec.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/BSpec.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/B.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/a" => new FakeFileForSpecs(path, true, true, List("b", "ABSpec.class", "AB.class"))
            case "foo/bar2/a/b" => new FakeFileForSpecs(path, true, true, List("c", "ABCSpec.class", "ABC.class"))
            case "foo/bar2/a/ABSpec.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/a/AB.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/a/b/c" => new FakeFileForSpecs(path, true, true, Nil)
            case "foo/bar2/a/b/ABCSpec.class" => new FakeFileForSpecs(path, true, false, Nil)
            case "foo/bar2/a/b/ABC.class" => new FakeFileForSpecs(path, true, false, Nil)
            case _ => new FakeFile(path)
        }
    }

    val oneFakeFileExpected = List("foo/bar1/ASpec.class")
    
    val fakeFilesExpected = List(
            "foo/bar1/ASpec.class", 
            "foo/bar2/BSpec.class",
            "foo/bar2/a/ABSpec.class",
            "foo/bar2/a/b/ABCSpec.class")

    val fakeFilesExpectedBar2a = List(
            "foo/bar2/BSpec.class",
            "foo/bar2/a/ABSpec.class",
            "foo/bar2/a/b/ABCSpec.class")
}