package sake.util

import org.scalatest._
import org.scalatest.Matchers._
import java.io.{File => JFile, FilenameFilter => JFilenameFilter, BufferedReader => JBufferedReader}
import scala.util.matching.Regex

object FileSpec extends FreeSpec {

    doBeforeSpec {
        val d = new JavaFile("toss")
        d.mkdirs shouldEqual true
        d.exists shouldEqual true
    }

    doAfterSpec {
        val d = new JavaFile("toss")
        d.deleteRecursively shouldEqual true
        d.exists shouldEqual false
        new JavaFile("toss").exists shouldEqual false
    }

    import sake.context._

    val cwd = Environment.currentWorkingDirectory

    "A FileFilter object" should {

        "ignore the directory java.io.File argument" in {
            val ff = new FileFilter("f1")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "f1.txt") shouldEqual false
        }

        "return true only for exact matches if the filter string has no wildcards" in {
            val ff = new FileFilter("f1")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "f1.txt") shouldEqual false
            ff.accept(dir, "f1") shouldEqual true
        }

        "return true for matches if the filter string has wildcards" in {
            val ff = new FileFilter("foo*bar?")
            val dir = new JFile("nonexistent")
            ff.accept(dir, "foobar1")    shouldEqual true
            ff.accept(dir, "fooXbar1")   shouldEqual true
            ff.accept(dir, "fooXYZbar1") shouldEqual true
            ff.accept(dir, "1foobar2")   shouldEqual false
            ff.accept(dir, "ffoobar2")   shouldEqual false
            ff.accept(dir, "foobar1")    shouldEqual true
            ff.accept(dir, "foobar2")    shouldEqual true
        }
    }

    "A JavaFile" should {
        "not accept an empty path parameter" in {
            new JavaFile("") must throwA[BuildError]
        }
    }

    "File.exists" should {
        "return true if the corresponding file exists" in {
            new JavaFile(cwd).exists shouldEqual true
        }

        "return false if the corresponding file does not exist" in {
            new JavaFile("nonexistent").exists shouldEqual false
        }
    }

    "File.path" should {
        "return the input path" in {
            new JavaFile(cwd).path shouldEqual cwd
        }
    }

    "File.javaFile" should {
        "return the underlying java.io.File" in {
            new JavaFile(cwd).javaFile.isInstanceOf[JFile] shouldEqual true
        }
    }

    "File.isDirectory" should {
        "return true if the corresponding object is a directory" in {
            new JavaFile(cwd).isDirectory shouldEqual true
        }

        "return false if the corresponding object is not a directory" in {
            new JavaFile("..").contents.get.foreach{ fileName =>
                val f = new JavaFile("../"+fileName)
                f.contents match {
                    case None    => f.isDirectory shouldEqual false
                    case Some(x) => f.isDirectory shouldEqual true
                }
            }
        }
    }

    "File.isFile" should {
        "return true if the corresponding object is a file" in {
            new JavaFile("..").contents.get.foreach{ fileName =>
                val f = new JavaFile("../"+fileName)
                f.contents match {
                    case None    => f.isFile shouldEqual true
                    case Some(x) => f.isFile shouldEqual false
                }
            }
        }

        "return false if the corresponding object is not a file" in {
            new JavaFile(cwd).isFile shouldEqual false
        }
    }

    "File.contents" should {
        "return a Some(List[String]) if the corresponding object is a directory" in {
            new JavaFile(cwd).contents must beSomething
        }

        "return the contents of the directory in the Some(List[String]) if the corresponding object is a directory" in {
            val listOpt = new JavaFile(cwd).contents
            val list = listOpt.get
            (list.length > 0) shouldEqual true
        }

        "return None if the corresponding object is not a directory" in {
            new JavaFile("..").contents.get.foreach{ fileName =>
                val f = new JavaFile("../"+fileName)
                f.contents match {
                    case None    => f.isFile shouldEqual true
                    case Some(x) => f.isFile shouldEqual false
                }
            }
        }
    }

    "File.contentsFilteredBy" should {
        "return a Some(List[String]) whose elements match the filter if the corresponding object is a directory" in {
            new JavaFile(cwd).contentsFilteredBy("sake*") must beSomething
        }

        "return None if the corresponding object is not a directory" in {
            new JavaFile("..").contents.get.foreach{ fileName =>
                val f = new JavaFile("../"+fileName)
                f.contentsFilteredBy("*") match {
                    case None    => f.isFile shouldEqual true
                    case Some(x) => f.isFile shouldEqual false
                }
            }
        }
    }

    "File.mkdirs" should {
        "fail for files" in {
            val f = new JavaFile("toss/f1")
            f.createNewFile
            f.mkdirs shouldEqual false
            f.delete shouldEqual true
            f.exists shouldEqual false
        }

        "fail for a directory that already exists" in {
            val d = new JavaFile("toss")
            d.mkdirs shouldEqual false
            d.exists shouldEqual true
        }

        "succeed for a non-existent directory" in {
            val d = new JavaFile("toss/f1")
            d.mkdirs shouldEqual true
            d.exists shouldEqual true
            d.delete shouldEqual true
            d.exists shouldEqual false
        }

        "fail for a non-existent file or directory" in {
            val d = new JavaFile("toss/f1")
            d.exists shouldEqual false
            d.delete shouldEqual false
        }
    }

    "File.delete" should {
        "succeed for a file that exists" in {
            val f = new JavaFile("toss/f1")
            f.createNewFile
            f.exists shouldEqual true
            f.delete shouldEqual true
            f.exists shouldEqual false
        }
    }

    "File.delete" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFile("toss/d1")
            d.mkdirs shouldEqual true
            d.exists shouldEqual true
            d.delete shouldEqual true
            d.exists shouldEqual false
            new JavaFile("toss/d1").exists shouldEqual false
        }
    }

    "File.delete" should {
        "fail for a directory that exists and is not empty" in {
            val d = new JavaFile("toss/d1")
            d.mkdirs shouldEqual true
            d.exists shouldEqual true
            val f = new JavaFile("toss/d1/f1")
            f.createNewFile
            f.exists shouldEqual true
            d.delete shouldEqual false
            // cleanup
            f.delete shouldEqual true
            f.exists shouldEqual false
            d.delete shouldEqual true
            d.exists shouldEqual false
            new JavaFile("toss/d1").exists shouldEqual false
        }
    }

    "File.deleteRecursively" should {
        "fail for a non-existent file or directory" in {
            val d = new JavaFile("toss/f1")
            d.exists shouldEqual false
            d.deleteRecursively shouldEqual false
        }
    }

    "File.deleteRecursively" should {
        "succeed for a file that exists" in {
            val f = new JavaFile("toss/f1")
            f.createNewFile
            f.exists shouldEqual true
            f.deleteRecursively shouldEqual true
            f.exists shouldEqual false
        }
    }

    "File.deleteRecursively" should {
        "succeed for a directory that exists and is empty" in {
            val d = new JavaFile("toss/d1")
            d.mkdirs shouldEqual true
            d.exists shouldEqual true
            d.deleteRecursively shouldEqual true
            d.exists shouldEqual false
        }
    }

    "File.deleteRecursively" should {
        "succeed for a directory that exists and is not empty" in {
            val d = new JavaFile("toss/d1")
            d.mkdirs shouldEqual true
            d.exists shouldEqual true
            val f = new JavaFile("toss/d1/f1")
            f.createNewFile shouldEqual true
            f.exists shouldEqual true
            d.deleteRecursively shouldEqual true
            f.exists shouldEqual false
            d.exists shouldEqual false
            new JavaFile("toss/d1").exists shouldEqual false
        }
    }

    def readwrite = {
        val f = new JavaFile("toss.txt")
        f.createNewFile shouldEqual true
        val writer = f.writer
        writer.write("hello world!")
        writer.flush()
        writer.close()
        val f2 = new JavaFile("toss.txt")
        val reader = new JBufferedReader(f2.reader)
        reader.readLine() shouldEqual "hello world!"
        f2.delete
    }

    "File.writer" should {
        "return a writer for writing output to the fail." in { readwrite }
    }

    "File.reader" should {
        "return a reader for reading content from the file." in { readwrite }
    }

}
