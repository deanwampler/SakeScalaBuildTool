package sake.util

import org.specs._ 

object FilesSpec extends Specification { 

    import sake.environment._
    
    class FakeFile(val path: String, exists2: Boolean, 
                   val isDirectory2: Boolean, val contents2: List[String]) extends File {

            def this (path: String) = this(path, false, false, Nil)

            def exists = exists2
            def isDirectory = isDirectory2
            def isFile = !isDirectory2
            def contents = contents2
        }
    
    def path(p: String) = p.replaceAll("/", Environment.environment.fileSeparator)
    
    "Constructing a Files object" should {
        
        "throw a BuildError if all specified strings are empty" in {
            (new Files("")) must throwA[BuildError]
            (new Files("", "")) must throwA[BuildError]
            (new Files(List("",""))) must throwA[BuildError]
        }
        
        "throw a BuildError if empty lists of strings are specified" in {
            (new Files(List())) must throwA[BuildError]
            (new Files(Nil)) must throwA[BuildError]
        }
        
        "accept one or more non-empty strings" in {
            val f = new Files(".")
            f.specification must be_==(List("."))
            val f2 = new Files("**/*Spec.class")
            f2.specification must be_==(List("**/*Spec.class"))
        }
    }
    
    "The specification" should {
        "match the input strings, with empty strings removed" in {
            val f = new Files("", ".", "")
            f.specification must be_==(List("."))
            val f2 = new Files("", "**/*Spec.class", "", "")
            f2.specification must be_==(List("**/*Spec.class"))
        }
    }
    
    "apply()" should {
        "return List('.') for specification '.'" in {
            new Files(".")() must be_==(List("."))
        }
        "return List('..') for specification '..'" in {
            new Files("..")() must be_==(List(".."))
        }
        "return List('foo/bar/baz') for specification 'foo/bar/baz' if it exists" in {
            val f = new Files(path("foo/bar/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar"))
                    case "foo/bar" => new FakeFile(path, true, true, List("baz"))
                    case "foo/bar/baz" => new FakeFile(path, true, true, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(List(path("foo/bar/baz")))
        }
        "return Nil for specification 'foo/bar/baz' if it does not exists" in {
            val f = new Files(path("foo/bar/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar"))
                    case "foo/bar" => new FakeFile(path, true, true, List(""))
                    case "foo/bar/baz" => new FakeFile(path)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(Nil)
        }
        
        "return List('foo/bar1/baz', 'foo/bar2/baz') for specification 'foo/*/baz' if they exist..." in {
            val f = new Files(path("foo/*/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("baz"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("baz"))
                    case "foo/bar1/baz" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/baz" => new FakeFile(path, true, true, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(List(path("foo/bar1/baz"), path("foo/bar2/baz")))
        }
        "return Nil for specification 'foo/*/baz' if neither 'foo/bar1' nor 'foo/bar2' has a 'baz'" in {
            val f = new Files(path("foo/bar/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("xx"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("yy"))
                    case "foo/bar1/xx" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/yy" => new FakeFile(path, true, true, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(Nil)
        }

        "return List('foo/bar1/baz', ...) for specification 'foo/**/baz' if they exist..." in {
            val f = new Files(path("foo/**/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("baz"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("baz", "a"))
                    case "foo/bar1/baz" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/baz" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a" => new FakeFile(path, true, true, List("b", "baz"))
                    case "foo/bar2/a/b" => new FakeFile(path, true, true, List("c", "baz"))
                    case "foo/bar2/a/baz" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/a/b/c" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/a/b/baz" => new FakeFile(path, true, true, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(List(path("foo/bar1/baz"), 
                                path("foo/bar2/baz"),
                                path("foo/bar2/a/baz"),
                                path("foo/bar2/a/b/baz")
                            ))
        }
        "return Nil for specification 'foo/**/baz' if neither 'foo/bar1' nor 'foo/bar2' has a 'baz'" in {
            val f = new Files(path("foo/bar/baz")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("xx"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("yy", "a"))
                    case "foo/bar1/xx" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/yy" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/a" => new FakeFile(path, true, true, List("b"))
                    case "foo/bar2/a/b" => new FakeFile(path, true, true, List("c"))
                    case "foo/bar2/a/b/c" => new FakeFile(path, true, false, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(Nil)
        }

        "return List('foo/bar1/*Spec.class', ...) for specification 'foo/**/*Spec.class' if they exist..." in {
            val f = new Files(path("foo/**/*Spec.class")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("A.class", "ASpec.class"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("BSpec.class", "B.class", "a"))
                    case "foo/bar1/A.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar1/ASpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/BSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/B.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a" => new FakeFile(path, true, true, List("b", "ABSpec.class", "AB.class"))
                    case "foo/bar2/a/b" => new FakeFile(path, true, true, List("c", "ABCSpec.class", "ABC.class"))
                    case "foo/bar2/a/ABSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/AB.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/b/c" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/a/b/ABCSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/b/ABC.class" => new FakeFile(path, true, false, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(List(path("foo/bar1/ASpec.class"), 
                                path("foo/bar2/BSpec.class"),
                                path("foo/bar2/a/ABSpec.class"),
                                path("foo/bar2/a/b/ABCSpec.class")
                            ))
        }
    }
    
    "When the specification has elements that overlap, apply()" should {
        "remove the duplicates" in {
            val f = new Files(path("foo/**/*Spec.class"), path("foo/bar2/**/*Spec.class")) {
                override def makeFile(path: String) = path match {
                    case "foo" => new FakeFile(path, true, true, List("bar1", "bar2"))
                    case "foo/bar1" => new FakeFile(path, true, true, List("A.class", "ASpec.class"))
                    case "foo/bar2" => new FakeFile(path, true, true, List("BSpec.class", "B.class", "a"))
                    case "foo/bar1/A.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar1/ASpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/BSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/B.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a" => new FakeFile(path, true, true, List("b", "ABSpec.class", "AB.class"))
                    case "foo/bar2/a/b" => new FakeFile(path, true, true, List("c", "ABCSpec.class", "ABC.class"))
                    case "foo/bar2/a/ABSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/AB.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/b/c" => new FakeFile(path, true, true, Nil)
                    case "foo/bar2/a/b/ABCSpec.class" => new FakeFile(path, true, false, Nil)
                    case "foo/bar2/a/b/ABC.class" => new FakeFile(path, true, false, Nil)
                    case _ => new FakeFile(path)
                }
            }
            f() must be_==(List(path("foo/bar1/ASpec.class"), 
                                path("foo/bar2/BSpec.class"),
                                path("foo/bar2/a/ABSpec.class"),
                                path("foo/bar2/a/b/ABCSpec.class")
                            ))
        }
    }
}
