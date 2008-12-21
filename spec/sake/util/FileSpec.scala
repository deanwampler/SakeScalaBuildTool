package sake.util

import org.specs._ 

import java.io.{File => JFile, FilenameFilter => JFilenameFilter}
import scala.util.matching.Regex

object FileSpec extends Specification { 

    import sake.environment._
    
    val cwd = Environment.environment.currentWorkingDirectory
    
    "A FileFilter object" should {
        
        "ignore the directory java.io.File argument" in {
            val ff = new FileFilter("foobar")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "foobar.txt") must be_==(false)
        }
        
        "return true only for exact matches if the filter string has no wildcards" in {
            val ff = new FileFilter("foobar")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "foobar.txt") must be_==(false)
            ff.accept(dir, "foobar") must be_==(true)
        }
        
        "return true for matches if the filter string has wildcards" in {
            val ff = new FileFilter("foo*bar?")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "foobar1")    must be_==(true)
            ff.accept(dir, "fooXbar1")   must be_==(true)
            ff.accept(dir, "fooXYZbar1") must be_==(true)
            ff.accept(dir, "1foobar2")   must be_==(false)
            ff.accept(dir, "foobar12")   must be_==(false)
            ff.accept(dir, "foobar1")    must be_==(true)
            ff.accept(dir, "foobar2")    must be_==(true)
        }
    }
    
    "A JavaFileWrapper" should {
        "not accept an empty path parameter" in {
            new JavaFileWrapper("") must throwA[BuildError]
        }
    }        
    
    "File.exists" should {
        "return true if the corresponding file exists" in {
            new JavaFileWrapper(cwd).exists must be_==(true)
        }
        
        "return false if the corresponding file does not exist" in {
            new JavaFileWrapper("nonexistent").exists must be_==(false)
        }
    }
    
    "File.path" should {
        "return the input path" in {
            new JavaFileWrapper(cwd).path must be_==(cwd)
        }
    }
    
    "File.file" should {
        "return the underlying java.io.File" in {
            new JavaFileWrapper(cwd).file.isInstanceOf[JFile] must be_==(true)
        }
    }

    "File.isDirectory" should {
        "return true if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).isDirectory must be_==(true)
        }
        
        "return false if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isDirectory must be_==(false)
                    case Some(x) => f.isDirectory must be_==(true)
                }
            }
        }
    }
    
    "File.isFile" should {
        "return true if the corresponding object is a file" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isFile must be_==(true)
                    case Some(x) => f.isFile must be_==(false)
                }
            }
        }
        
        "return false if the corresponding object is not a file" in {
            new JavaFileWrapper(cwd).isFile must be_==(false)
        }
    }
    
    "File.contents" should {
        "return a Some(List[String]) if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).contents match {
                case None => fail()
                case Some(x) =>
            } 
        }
        
        "return the contents of the directory in the Some(List[String]) if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).contents match {
                case None => fail()
                case Some(l:List[_]) => (l.length > 0) must be_==(true)
            } 
        }
        
        "return None if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contents match {
                    case None    => f.isFile must be_==(true)
                    case Some(x) => f.isFile must be_==(false)
                }
            }
        }
    }
    
    "File.contentsFilteredBy" should {
        "return a Some(List[String]) whose elements match the filter if the corresponding object is a directory" in {
            new JavaFileWrapper(cwd).contentsFilteredBy("sake*") match {
                case None => fail()
                case Some(x) =>
            } 
        }
        
        "return None if the corresponding object is not a directory" in {
            new JavaFileWrapper("..").contents.get.foreach{ fileName =>
                val f = new JavaFileWrapper("../"+fileName)
                f.contentsFilteredBy("*") match {
                    case None    => f.isFile must be_==(true)
                    case Some(x) => f.isFile must be_==(false)
                }
            }
        }
    }
    
    "File.mkdirs" should {
        "fail for files" in {
            val f = new JavaFileWrapper("./foobar")
            f.createNewFile
            f.mkdirs must be_==(false)
            f.delete
            f.exists must be_==(false)
        }
    }
    
    "File.mkdirs" should {
        "fail for a directory that already exists" in {
            val d = new JavaFileWrapper(".")
            d.mkdirs must be_==(false)
            d.exists must be_==(true)
        }
    }
    
    "File.mkdirs" should {
        "succeed for a non-existent directory" in {
            val d = new JavaFileWrapper("./foobar")
            d.mkdirs must be_==(true)
            d.exists must be_==(true)
            d.delete
            d.exists must be_==(false)
        }
    }
    
    "File.delete" should {
        "fail for a non-existent file or directory" in {
            val d = new JavaFileWrapper("./foobar")
            d.exists must be_==(false)
            d.delete must be_==(false)
        }
    }
    
    "File.delete" should {
        "succeed for a file that exists" in {
            val f = new JavaFileWrapper("./foobar")
            f.createNewFile
            f.exists must be_==(true)
            f.delete must be_==(true)
            f.exists must be_==(false)
        }
    }
    
    "File.delete" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFileWrapper("./tossdir")
            d.mkdirs
            d.exists must be_==(true)
            d.delete must be_==(true)
            d.exists must be_==(false)
        }
    }
    
    "File.delete" should {
        "fail for a directory that exists and is not empty" in {
            val d = new JavaFileWrapper("./tossdir")
            d.mkdirs
            d.exists must be_==(true)
            val f = new JavaFileWrapper("./tossdir/foobar")
            f.createNewFile
            f.exists must be_==(true)
            d.delete must be_==(false)
            // cleanup
            f.delete must be_==(true)
            d.delete must be_==(true)
            d.exists must be_==(false)
        }
    }
    
    "File.deleteRecursively" should {
        "fail for a non-existent file or directory" in {
            val d = new JavaFileWrapper("./foobar")
            d.exists must be_==(false)
            d.deleteRecursively must be_==(false)
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a file that exists" in {
            val f = new JavaFileWrapper("./foobar")
            f.createNewFile
            f.exists must be_==(true)
            f.deleteRecursively must be_==(true)
            f.exists must be_==(false)
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFileWrapper("./tossdir")
            d.mkdirs
            d.exists must be_==(true)
            d.deleteRecursively must be_==(true)
            d.exists must be_==(false)
        }
    }
    
    "File.deleteRecursively" should {
        "succeed for a directory that exists and is not empty" in {
            val d = new JavaFileWrapper("./tossdir")
            d.mkdirs
            d.exists must be_==(true)
            val f = new JavaFileWrapper("./tossdir/foobar")
            f.createNewFile
            f.exists must be_==(true)
            d.deleteRecursively must be_==(true)
        }
    }
    
}