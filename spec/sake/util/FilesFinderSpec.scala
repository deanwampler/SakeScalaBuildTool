package sake.util

import org.specs._ 

object FilesFinderSpec extends Specification { 

    import sake.environment._
    
    "FilesFinder" should {
        
        "throw a BuildError if all specified strings are empty" in {
            (new FilesFinder()("")) must throwA[BuildError]
            (new FilesFinder()("", "")) must throwA[BuildError]
            (new FilesFinder()(List("",""))) must throwA[BuildError]
        }
        
        "throw a BuildError if empty lists of strings are specified" in {
            (new FilesFinder()(List())) must throwA[BuildError]
            (new FilesFinder()(Nil)) must throwA[BuildError]
        }

        "ignore empty input strings" in {
            new FilesFinder()("", ".", "") mustEqual List(".")
        }
    
        "return List('.') for specification '.'" in {
            new FilesFinder()(".") mustEqual List(".")
        }
        "return List('..') for specification '..'" in {
            new FilesFinder()("..") mustEqual List("..")
        }
        "return List('foo/bar/baz') for specification 'foo/bar/baz' if it exists" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "foo" => new FakeFile(p, true, true, List("bar"))
                        case "foo/bar" => new FakeFile(p, true, true, List("baz"))
                        case "foo/bar/baz" => new FakeFile(p, true, true, Nil)
                        case _ => new FakeFile(p)
                    }
                }
            }
            f("foo/bar/baz") mustEqual List("foo/bar/baz")
        }
        "return Nil for specification 'foo/bar/baz' if it does not exists" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "foo" => new FakeFile(p, true, true, List("bar"))
                        case "foo/bar" => new FakeFile(p, true, true, List(""))
                        case "foo/bar/baz" => new FakeFile(p)
                        case _ => new FakeFile(p)
                    }
                }
            }
            f("foo/bar/baz") mustEqual Nil
        }
        
        val fFooBar12Baz = new FilesFinder() {
            override def makeFile(path: String) = {
                val p = if (path.length == 0) "." else path
                p match {
                    case "." => new FakeFile(p, true, true, List("foo"))
                    case "foo" => new FakeFile(p, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(p, true, true, List("baz"))
                    case "foo/bar2" => new FakeFile(p, true, true, List("baz"))
                    case "foo/bar1/baz" => new FakeFile(p, true, true, Nil)
                    case "foo/bar2/baz" => new FakeFile(p, true, true, Nil)
                    case _ => new FakeFile(p)
                }
            }
        }

        "return List('foo/bar1/baz', 'foo/bar2/baz') for specification 'foo/*/baz' if they exist..." in {
            fFooBar12Baz("foo/*/baz") mustEqual List("foo/bar1/baz", "foo/bar2/baz")
        }
        
        "return List('foo') for specification '*' if it is the only item in '.'" in {
            fFooBar12Baz("*") mustEqual List("foo")
        }

        "return List('foo/bar1') for specification '*/bar1' 'bar1' is the only item in 'foo'" in {
            fFooBar12Baz("*/bar1") mustEqual List("foo/bar1")
        }

        "return Nil for specification 'foo/*/baz' if neither 'foo/bar1' nor 'foo/bar2' has a 'baz'" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "." => new FakeFile(p, true, true, List("foo"))
                        case "foo" => new FakeFile(p, true, true, List("bar1", "bar2"))
                        case "foo/bar1" => new FakeFile(p, true, true, List("xx"))
                        case "foo/bar2" => new FakeFile(p, true, true, List("yy"))
                        case "foo/bar1/xx" => new FakeFile(p, true, true, Nil)
                        case "foo/bar2/yy" => new FakeFile(p, true, true, Nil)
                        case _ => new FakeFile(p)
                    }
                }
            }
            f("foo/bar/baz") mustEqual Nil
        }

        "return Nil for specification '*' if the current directory is empty" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "." => new FakeFile(p)
                    }
                }
            }
            f("*") mustEqual Nil
        }

        val fFooBar12BazMore = new FilesFinder() {
            override def makeFile(path: String) = {
                val p = if (path.length == 0) "." else path
                p match {
                    case "." => new FakeFile(p, true, true, List("foo"))
                    case "foo" => new FakeFile(p, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(p, true, true, List("baz"))
                    case "foo/bar2" => new FakeFile(p, true, true, List("baz", "a"))
                    case "foo/bar1/baz" => new FakeFile(p, true, true, Nil)
                    case "foo/bar2/baz" => new FakeFile(p, true, false, Nil)
                    case "foo/bar2/a" => new FakeFile(p, true, true, List("b", "baz"))
                    case "foo/bar2/a/b" => new FakeFile(p, true, true, List("c", "baz"))
                    case "foo/bar2/a/baz" => new FakeFile(p, true, true, Nil)
                    case "foo/bar2/a/b/c" => new FakeFile(p, true, true, Nil)
                    case "foo/bar2/a/b/baz" => new FakeFile(p, true, true, Nil)
                    case _ => new FakeFile(p)
                }
            }
        }

        "return List('foo/bar1/baz', ...) for specification 'foo/**/baz' if they exist" in {
            fFooBar12BazMore("foo/**/baz") mustEqual List(
                "foo/bar1/baz",
                "foo/bar2/baz",
                "foo/bar2/a/baz",
                "foo/bar2/a/b/baz")
        }
        
        "return List('foo/bar1/baz', ...) for specification '**/baz' if they exist" in {
            fFooBar12BazMore("**/baz") mustEqual List(
                "foo/bar1/baz",
                "foo/bar2/baz",
                "foo/bar2/a/baz",
                "foo/bar2/a/b/baz")
        }
        

        "return List('foo/bar1/*Spec.class', ...) for specification 'foo/**/*Spec.class' if they exist..." in {
            FakeFileForSpecs.fakeFilesFinder("foo/**/*Spec.class") mustEqual FakeFileForSpecs.fakeFilesExpected
        }

        "return Nil for specification 'foo/**/baz' if neither 'foo/bar1' nor 'foo/bar2' has a 'baz'" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "." => new FakeFile(p, true, true, List("foo"))
                        case "foo" => new FakeFile(p, true, true, List("bar1", "bar2"))
                        case "foo/bar1" => new FakeFile(p, true, true, List("xx"))
                        case "foo/bar2" => new FakeFile(p, true, true, List("yy", "a"))
                        case "foo/bar1/xx" => new FakeFile(p, true, false, Nil)
                        case "foo/bar2/yy" => new FakeFile(p, true, true, Nil)
                        case "foo/bar2/a" => new FakeFile(p, true, true, List("b"))
                        case "foo/bar2/a/b" => new FakeFile(p, true, true, List("c"))
                        case "foo/bar2/a/b/c" => new FakeFile(p, true, false, Nil)
                        case _ => new FakeFile(p)
                    }
                }
            }
            f("foo/**/baz") mustEqual Nil
        }

        "return Nil for specification '**' if the current directory is empty" in {
            val f = new FilesFinder() {
                override def makeFile(path: String) = {
                    val p = if (path.length == 0) "." else path
                    p match {
                        case "." => new FakeFile(p)
                    }
                }
            }
            f("**") mustEqual Nil
        }
    }
    
    "A specification like foo/bar1/**/*.class should match foo/bar1/Foo.scala, e.g., ** maps zero or more subpaths" should {
        "be fixed" in {
            FakeFileForSpecs.oneFakeFile("foo/bar1/**/*.class") mustEqual FakeFileForSpecs.oneFakeFileExpected
        }        
        
    }
    
    "When the specification has elements that don't overlap, apply()" should {
        "return the sum of the lists" in {
            FakeFileForSpecs.fakeFilesFinder("foo/bar1/**/*Spec.class", "foo/bar2/**/*Spec.class") mustEqual FakeFileForSpecs.fakeFilesExpected
        }
    }
    
    "When the specification has elements that overlap, apply()" should {
        "returns the union of the lists (i.e., with duplicates removed)" in {
            FakeFileForSpecs.fakeFilesFinder("foo/**/*Spec.class", "foo/bar2/**/*Spec.class") mustEqual FakeFileForSpecs.fakeFilesExpected
        }
    }
    
    "When one specification is subtracted from another spec., apply()" should {
        "return the first list minus the second" in {
            val fakes = FakeFileForSpecs.fakeFilesFinder("foo/**/*Spec.class")
            val skips = FakeFileForSpecs.fakeFilesFinder("foo/bar1/**")
            (fakes filterNot { x => skips contains x }).sortWith(_.length < _.length) mustEqual FakeFileForSpecs.fakeFilesExpectedBar2a
        }
    }
    
    "FilesFinder.isDirectory" should {
        "return true for a directory" in {
            (new FilesFinder).isDirectory(".") must beTrue
        }
        "return false for a file" in {
            (new FilesFinder).isDirectory("./sake.scala") must beFalse
        }
    }

    "FilesFinder.exists" should {
        "return true for a directory that exists" in {
            (new FilesFinder).exists(".") must beTrue
        }
        "return true for a file that exists" in {
            (new FilesFinder).exists("./sake.scala") must beTrue
        }
        "return false for a file or directory that doesn't exist" in {
            (new FilesFinder).exists("doesnotexist") must beFalse
        }
    }
}
