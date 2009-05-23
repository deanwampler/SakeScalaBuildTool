package sake.util

import org.specs._ 

import java.io.{File => JFile, FilenameFilter => JFilenameFilter, BufferedReader => JBufferedReader}
import scala.util.matching.Regex

object FileSpec extends Specification { 

    doBeforeSpec {
        val d = new JavaFileWrapper("toss")
        d.mkdirs mustEqual true
        d.exists mustEqual true
    }

    doAfterSpec {
        val d = new JavaFileWrapper("toss")
        d.deleteRecursively mustEqual true
        d.exists mustEqual false
        new JavaFileWrapper("toss").exists mustEqual false
    }

    import sake.environment._
    
    val cwd = Environment.environment.currentWorkingDirectory
    
    "A FileFilter object" should {
        
        "ignore the directory java.io.File argument" in {
            val ff = new FileFilter("f1")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "f1.txt") mustEqual false
        }
        
        "return true only for exact matches if the filter string has no wildcards" in {
            val ff = new FileFilter("f1")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "f1.txt") mustEqual false
            ff.accept(dir, "f1") mustEqual true
        }
        
        "return true for matches if the filter string has wildcards" in {
            val ff = new FileFilter("foo*bar?")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "foobar1")    mustEqual true
            ff.accept(dir, "fooXbar1")   mustEqual true
            ff.accept(dir, "fooXYZbar1") mustEqual true
            ff.accept(dir, "1foobar2")   mustEqual false
            ff.accept(dir, "ffoobar2")   mustEqual false
            ff.accept(dir, "foobar1")    mustEqual true
            ff.accept(dir, "foobar2")    mustEqual true
        }
    }
    
    "A JavaFileWrapper" should {
        "not accept an empty path parameter" in {
            new JavaFileWrapper("") must throwA[BuildError]
        }
    }        
    
    "File.exists" should {
        "return true if the corresponding file exists" in {
            new JavaFileWrapper(cwd).exists mustEqual true
        }
        
        "return false if the corresponding file does not exist" in {
            new JavaFileWrapper("nonexistent").exists mustEqual false
        }
    }
    
    "File.path" should {
        "return the input path" in {
            new JavaFileWrapper(cwd).path mustEqual cwd
        }
    }
    
    "File.javaFile" should {
        "return the underlying java.io.File" in {
            new JavaFileWrapper(cwd).javaFile.isInstanceOf[JFile] mustEqual true
        }
    }

    "File.isDirectory" should {
        "return true if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).isDirectory mustEqual true
        }
        
        "return false if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isDirectory mustEqual false
                    case Some(x) => f.isDirectory mustEqual true
                }
            }
        }
    }
    
    "File.isFile" should {
        "return true if the corresponding object is a file" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isFile mustEqual true
                    case Some(x) => f.isFile mustEqual false
                }
            }
        }
        
        "return false if the corresponding object is not a file" in {
            new JavaFileWrapper(cwd).isFile mustEqual false
        }
    }
    
    "File.contents" should {
        "return a Some(List[String]) if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).contents must beSomething
        }
        
        "return the contents of the directory in the Some(List[String]) if the corresponding object is a directory" in {
            val listOpt = new JavaFileWrapper(cwd).contents
            val list = listOpt.get
            (list.length > 0) mustEqual true
        }
        
        "return None if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isFile mustEqual true
                    case Some(x) => f.isFile mustEqual false
                }
            }
        }
    }
    
    "File.contentsFilteredBy" should {
        "return a Some(List[String]) whose elements match the filter if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).contentsFilteredBy("sake*") must beSomething 
        }
        
        "return None if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contentsFilteredBy("*") match {
                    case None    => f.isFile mustEqual true
                    case Some(x) => f.isFile mustEqual false
                }
            }
        }
    }
    
    "File.mkdirs" should {
        "fail for files" in {
            val f = new JavaFileWrapper("toss/f1")
            f.createNewFile
            f.mkdirs mustEqual false
            f.delete mustEqual true
            f.exists mustEqual false
        }
    
        "fail for a directory that already exists" in {
            val d = new JavaFileWrapper("toss")
            d.mkdirs mustEqual false
            d.exists mustEqual true
        }
    
        "succeed for a non-existent directory" in {
            val d = new JavaFileWrapper("toss/f1")
            d.mkdirs mustEqual true
            d.exists mustEqual true
            d.delete mustEqual true
            d.exists mustEqual false
        }
    
        "fail for a non-existent file or directory" in {
            val d = new JavaFileWrapper("toss/f1")
            d.exists mustEqual false
            d.delete mustEqual false
        }
    }
    
    "File.delete" should {
        "succeed for a file that exists" in {
            val f = new JavaFileWrapper("toss/f1")
            f.createNewFile
            f.exists mustEqual true
            f.delete mustEqual true
            f.exists mustEqual false
        }
    }
    
    "File.delete" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFileWrapper("toss/d1")
            d.mkdirs mustEqual true
            d.exists mustEqual true
            d.delete mustEqual true
            d.exists mustEqual false
            new JavaFileWrapper("toss/d1").exists mustEqual false
        }
    }
    
    "File.delete" should {
        "fail for a directory that exists and is not empty" in {
            val d = new JavaFileWrapper("toss/d1")
            d.mkdirs mustEqual true
            d.exists mustEqual true
            val f = new JavaFileWrapper("toss/d1/f1")
            f.createNewFile
            f.exists mustEqual true
            d.delete mustEqual false
            // cleanup
            f.delete mustEqual true
            f.exists mustEqual false
            d.delete mustEqual true
            d.exists mustEqual false
            new JavaFileWrapper("toss/d1").exists mustEqual false
        }
    }
    
    "File.deleteRecursively" should {
        "fail for a non-existent file or directory" in {
            val d = new JavaFileWrapper("toss/f1")
            d.exists mustEqual false
            d.deleteRecursively mustEqual false
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a file that exists" in {
            val f = new JavaFileWrapper("toss/f1")
            f.createNewFile
            f.exists mustEqual true
            f.deleteRecursively mustEqual true
            f.exists mustEqual false
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFileWrapper("toss/d1")
            d.mkdirs mustEqual true
            d.exists mustEqual true
            d.deleteRecursively mustEqual true
            d.exists mustEqual false
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a directory that exists and is not empty" in {
            val d = new JavaFileWrapper("toss/d1")
            d.mkdirs mustEqual true
            d.exists mustEqual true
            val f = new JavaFileWrapper("toss/d1/f1")
            f.createNewFile mustEqual true                
            f.exists mustEqual true
            d.deleteRecursively mustEqual true
            f.exists mustEqual false
            d.exists mustEqual false
            new JavaFileWrapper("toss/d1").exists mustEqual false
        }
    }

    def readwrite = {
        val f = new JavaFileWrapper("toss.txt")
        f.createNewFile mustEqual true
        val writer = f.writer
        writer.write("hello world!")
        writer.flush()
        writer.close()
        val f2 = new JavaFileWrapper("toss.txt")
        val reader = new JBufferedReader(f2.reader)
        reader.readLine() mustEqual "hello world!"
        f2.delete
    }
    
    "File.writer" should {
        "return a writer for writing output to the fail." in { readwrite }
    }
    
    "File.reader" should {
        "return a reader for reading content from the file." in { readwrite }
    }
    
}