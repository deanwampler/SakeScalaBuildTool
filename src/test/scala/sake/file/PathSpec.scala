package sake.util

import org.scalatest._
import org.scalatest.Matchers._
import sake.context._

import sake.files.{File, Path}

// Uses the full path to {@link sake.util.Path} to avoid collisions with Spec's own FileMatchers.Path.
object PathSpec extends FreeSpec {

    "A Path" can {
        "be empty" in {
            sake.util.Path().joined shouldEqual ""
        }
        "have one element" in {
            sake.util.Path("foo").elements shouldEqual List("foo")
        }
        "have 2 or more elements" in {
            sake.util.Path(1.23, "foo").elements shouldEqual List(1.23,"foo")
            sake.util.Path(1.23, "foo", 1, 'a).elements shouldEqual List(1.23,"foo",1,'a)
        }
        "be constructed with no elements" in {
            sake.util.Path().elements shouldEqual Nil
            sake.util.Path(Nil).elements shouldEqual Nil
        }
        "be constructed with one Any element" in {
            sake.util.Path("foo").elements shouldEqual List("foo")
        }
        "be constructed with one String element that is split on the separator" in {
            sake.util.Path("foo:bar")(":").elements shouldEqual List("foo", "bar")
        }
        "be constructed with 2 or more Any elements" in {
            sake.util.Path(1.23, "foo").elements shouldEqual List(1.23,"foo")
            sake.util.Path(1.23, "foo", 1, 'a).elements shouldEqual List(1.23,"foo",1,'a)
        }
        "be constructed with a list with one Any element" in {
            sake.util.Path(List(1.23)).elements shouldEqual List(1.23)
        }
        "be constructed with a list with 2 or more Any elements" in {
            sake.util.Path(List(1.23, "foo")).elements shouldEqual List(1.23,"foo")
            sake.util.Path(List(1.23, "foo", 1, 'a)).elements shouldEqual List(1.23,"foo",1,'a)
        }
    }

    "A Path constructor" should {
        "flatten nested sequences" in {
            sake.util.Path(List(1.23, List("foo", 1), 'a)).elements shouldEqual List(1.23,"foo",1,'a)
        }
    }

    "elements" should {
        "return a List of the elements" in {
            sake.util.Path(List(1.23, "foo", 1, 'a)).elements shouldEqual List(1.23,"foo",1,'a)
        }
        "return Nil for an empty path" in {
            sake.util.Path().elements shouldEqual Nil
        }
    }

    "The Path separator" should {
        "default to the system path separator" in {
             sake.util.Path().separator shouldEqual Environment.pathSeparator
        }
        "be setable during construction" in {
             sake.util.Path()("|").separator shouldEqual "|"
        }
        "be invariant when new Paths are created from this Path" in {
            val p1 = sake.util.Path()("|")
            val p2 = "foo" :: p1
            p2.separator shouldEqual "|"
            p2 must beDifferent(p1)
        }
    }

    "joined and toString()" should {
        "convert the path's list[Any] into a separator-separated string" in {
            val list = List(1, 2.3, "foo", 'foo, List("x", "y"), ("a", 'b))
            sake.util.Path(list).joined shouldEqual "1:2.3:foo:'foo:x:y:(a,'b)"
        }
        "use the user-specified or default separator" in {
            val list = List(1, 2.3, "foo", 'foo, List("x", "y"), ("a", 'b))
            sake.util.Path(list).joined shouldEqual "1:2.3:foo:'foo:x:y:(a,'b)"
            sake.util.Path(list)("|").joined shouldEqual "1|2.3|foo|'foo|x|y|(a,'b)"
        }
        "comvert an empty path into the empty string" in {
            sake.util.Path().joined shouldEqual ""
        }
        "convert a one element path into a string with the element and no separator" in {
            sake.util.Path("foo").joined shouldEqual "foo"
        }
        "comvert a 2 or more elements path into a string with each element, separated by the separator" in {
            sake.util.Path(1.23, "foo").joined shouldEqual "1.23:foo"
            sake.util.Path(1.23, "foo", 1, 'a).joined shouldEqual "1.23:foo:1:'a"
        }
    }

    "+" should {
        "append an element to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            (p + "bar") must beDifferent(p)
            (p + "bar").elements shouldEqual List(1.23, "foo", "bar")
            (p + "bar" + "baz").elements shouldEqual List(1.23, "foo", "bar", "baz")
        }
    }

    "++" should {
        "append a sequence of new elements to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            val p2 = p ++ List("bar", "baz")
            (p2) must beDifferent(p)
            (p2).elements shouldEqual List(1.23, "foo", "bar", "baz")
        }
        "append a second path to the path, returning a new, combined path" in {
            val p1 = sake.util.Path(List(1.23, "foo"))
            val p2 = sake.util.Path(List("bar", "baz"))
            val p12 = p1 ++ p2
            (p12) must beDifferent(p1)
            (p12).elements shouldEqual List(1.23, "foo", "bar", "baz")
        }
    }

    "::" should {
        "prepend an element to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            ("bar" :: p) must beDifferent(p)
            ("bar" :: p).elements shouldEqual List("bar", 1.23, "foo")
            ("baz" :: "bar" :: p).elements shouldEqual List("baz", "bar", 1.23, "foo")
        }
    }

    ":::" should {
        "prepend a sequence of new elements to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            val p2 = List("bar", "baz") ::: p
            (p2) must beDifferent(p)
            (p2).elements shouldEqual List("bar", "baz", 1.23, "foo")
        }
        "prepend a second path to the path, returning a new, combined path" in {
            val p1 = sake.util.Path(List(1.23, "foo"))
            val p2 = sake.util.Path(List("bar", "baz"))
            val p21 = p2 ::: p1
            (p21) must beDifferent(p1)
            (p21) must beDifferent(p2)
            (p21).elements shouldEqual List("bar", "baz", 1.23, "foo")
        }
    }

    "equals" should {
        "be reflexive" in {
            val p = sake.util.Path(1.23, "foo")
            (p == p) shouldEqual true
        }
        "be symmetric" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path("foo", 1.23)
            (p1 == p2) shouldEqual (p2 == p1)
            (p1 == p3) shouldEqual (p3 == p1)
        }
        "be transitive" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path(1.23, "foo")
            (p1 == p2) shouldEqual (p2 == p3)
            (p1 == p2) shouldEqual (p1 == p3)
            (p2 == p3) shouldEqual (p1 == p3)
        }
        "return the same value every time for the same path" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path("foo", 1.23)
            (p1 == p2) shouldEqual (p1 == p2)
            (p1 == p3) shouldEqual (p1 == p3)
        }
        "return false for null objects" in {
            val p1 = sake.util.Path(1.23, "foo")
            (p1 == null) shouldEqual false

        }
        "return false for non-Path objects" in {
            val p1 = sake.util.Path(1.23, "foo")
            val l2 = List(1.23, "foo")
            (p1 == l2) shouldEqual false

        }
        "return true for Paths with identical elements and separators" in {
            val p1 = sake.util.Path(1.23, "foo")("|")
            val p2 = sake.util.Path(1.23, "foo")("|")
            (p1 == p2) shouldEqual true
        }
        "return false for Paths with identical elements but different separators" in {
            val p1 = sake.util.Path(1.23, "foo")("|")
            val p2 = sake.util.Path(1.23, "foo")("/")
            (p1 == p2) shouldEqual false
        }
        "return true for Paths with different elements and identical separators" in {
            val p1 = sake.util.Path(1.24, "foo2")("|")
            val p2 = sake.util.Path(1.23, "foo")("|")
            (p1 == p2) shouldEqual false
        }
    }

    "hashCode" should {
        "always return the same value for the same path" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path()
            (p1.hashCode) shouldEqual p1.hashCode
            (p2.hashCode) shouldEqual p2.hashCode
        }
        "be the same for identical paths" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            (p1.hashCode) shouldEqual p2.hashCode
        }
        "be different for Paths with identical elements but different separators" in {
            val p1 = sake.util.Path(1.23, "foo")("|")
            val p2 = sake.util.Path(1.23, "foo")("/")
            (p1.hashCode) must beDifferent (p2.hashCode)
        }
        "return true for Paths with different elements and identical separators" in {
            val p1 = sake.util.Path(1.24, "foo2")("|")
            val p2 = sake.util.Path(1.23, "foo")("|")
            (p1.hashCode) must beDifferent (p2.hashCode)
        }
    }

}
