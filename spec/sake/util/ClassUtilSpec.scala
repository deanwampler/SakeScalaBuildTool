package sake.util

import org.specs._ 

object ClassUtilSpec extends Specification { 
    "toFullyQualifiedName" should {
        "accept an optional prefix to remove from the file name" in {
            ClassUtil.toFullyQualifiedName("foo/bar/Baz.class") must be_==("foo.bar.Baz")
        }

        "accept an optional prefix to remove from the file name" in {
            ClassUtil.toFullyQualifiedName("foo/bar/Baz.class", "") must be_==("foo.bar.Baz")
        }

        "throw a BuildError if the file name does not have the specified prefix to remove" in {
            ClassUtil.toFullyQualifiedName("foo/bar/baz.class", "xxx") must throwA[BuildError]
        }

        "remove the prefix from the file name" in {
            ClassUtil.toFullyQualifiedName("build/foo/bar/Baz.class", "build/") must be_==("foo.bar.Baz")
        }

        "remove the prefix from the file name" in {
            ClassUtil.toFullyQualifiedName("build/foo/bar/Baz.class", "build/") must be_==("foo.bar.Baz")
        }

        "replace all '/' with '.'" in {
            ClassUtil.toFullyQualifiedName("build/foo/bar/Baz.class", "build/") must be_==("foo.bar.Baz")
        }

        "remove the trailing '.class'" in {
            ClassUtil.toFullyQualifiedName("build/foo/bar/Baz.class", "build/") must be_==("foo.bar.Baz")
        }

        "ignore a missing trailing '.class'" in {
            ClassUtil.toFullyQualifiedName("build/foo/bar/Baz", "build/") must be_==("foo.bar.Baz")
        }
    }
}
