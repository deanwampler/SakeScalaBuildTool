package sake.util

import org.specs._
import sake.environment._
import sake.util.Path._

// Uses the full path to {@link sake.util.Path} to avoid collisions with Spec's own FileMatchers.Path.
object PathSpec extends Specification { 

    "A Path" can {
        "be empty" in {
            sake.util.Path().joined mustEqual ""
        }
        "have one element" in {
            sake.util.Path("foo").elements mustEqual List("foo")
        }
        "have 2 or more elements" in {
            sake.util.Path(1.23, "foo").elements mustEqual List(1.23,"foo")
            sake.util.Path(1.23, "foo", 1, 'a).elements mustEqual List(1.23,"foo",1,'a)
        }
        "be constructed with no elements" in {
            sake.util.Path().elements mustEqual Nil
            sake.util.Path(Nil).elements mustEqual Nil
        }
        "be constructed with one Any element" in {
            sake.util.Path("foo").elements mustEqual List("foo")
        }
        "be constructed with one String element that is split on the separator" in {
            sake.util.Path("foo:bar")(":").elements mustEqual List("foo", "bar")
        }
        "be constructed with 2 or more Any elements" in {
            sake.util.Path(1.23, "foo").elements mustEqual List(1.23,"foo")
            sake.util.Path(1.23, "foo", 1, 'a).elements mustEqual List(1.23,"foo",1,'a)
        }
        "be constructed with a list with one Any element" in {
            sake.util.Path(List(1.23)).elements mustEqual List(1.23)
        }
        "be constructed with a list with 2 or more Any elements" in {
            sake.util.Path(List(1.23, "foo")).elements mustEqual List(1.23,"foo")
            sake.util.Path(List(1.23, "foo", 1, 'a)).elements mustEqual List(1.23,"foo",1,'a)
        }
    }
    
    "A Path constructor" should {
        "flatten nested sequences" in {
            sake.util.Path(List(1.23, List("foo", 1), 'a)).elements mustEqual List(1.23,"foo",1,'a)
        }
    }
    
    "elements" should {
        "return a List of the elements" in {
            sake.util.Path(List(1.23, "foo", 1, 'a)).elements mustEqual List(1.23,"foo",1,'a)            
        }
        "return Nil for an empty path" in {
            sake.util.Path().elements mustEqual Nil
        }
    }
    
    "The Path separator" should {
        "default to the system path separator" in {
             sake.util.Path().separator mustEqual Environment.environment.pathSeparator
        }        
        "be setable during construction" in {
             sake.util.Path()("|").separator mustEqual "|"
        }        
        "be invariant when new Paths are created from this Path" in {
            val p1 = sake.util.Path()("|")
            val p2 = "foo" :: p1
            p2.separator mustEqual "|"
            p2 must beDifferent(p1)
        }
    }
    
    "joined and toString()" should {
        "convert the path's list[Any] into a separator-separated string" in {
            val list = List(1, 2.3, "foo", 'foo, List("x", "y"), ("a", 'b))
            sake.util.Path(list).joined mustEqual "1:2.3:foo:'foo:x:y:(a,'b)"
        }
        "use the user-specified or default separator" in {
            val list = List(1, 2.3, "foo", 'foo, List("x", "y"), ("a", 'b))
            sake.util.Path(list).joined mustEqual "1:2.3:foo:'foo:x:y:(a,'b)"
            sake.util.Path(list)("|").joined mustEqual "1|2.3|foo|'foo|x|y|(a,'b)"
        }
        "comvert an empty path into the empty string" in {
            sake.util.Path().joined mustEqual ""
        }
        "convert a one element path into a string with the element and no separator" in {
            sake.util.Path("foo").joined mustEqual "foo"
        }
        "comvert a 2 or more elements path into a string with each element, separated by the separator" in {
            sake.util.Path(1.23, "foo").joined mustEqual "1.23:foo"
            sake.util.Path(1.23, "foo", 1, 'a).joined mustEqual "1.23:foo:1:'a"
        }
    }
    
    "+" should {
        "append an element to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            (p + "bar") must beDifferent(p)
            (p + "bar").elements mustEqual List(1.23, "foo", "bar")
            (p + "bar" + "baz").elements mustEqual List(1.23, "foo", "bar", "baz")
        }
    }
    
    "++" should {
        "append a sequence of new elements to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            val p2 = p ++ List("bar", "baz")
            (p2) must beDifferent(p)
            (p2).elements mustEqual List(1.23, "foo", "bar", "baz")
        }
        "append a second path to the path, returning a new, combined path" in {
            val p1 = sake.util.Path(List(1.23, "foo"))
            val p2 = sake.util.Path(List("bar", "baz"))
            val p12 = p1 ++ p2
            (p12) must beDifferent(p1)
            (p12).elements mustEqual List(1.23, "foo", "bar", "baz")
        }
    }
    
    "::" should {
        "prepend an element to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            ("bar" :: p) must beDifferent(p)
            ("bar" :: p).elements mustEqual List("bar", 1.23, "foo")
            ("baz" :: "bar" :: p).elements mustEqual List("baz", "bar", 1.23, "foo")
        }
    }
    
    ":::" should {
        "prepend a sequence of new elements to the path, returning a new path" in {
            val p = sake.util.Path(List(1.23, "foo"))
            val p2 = List("bar", "baz") ::: p
            (p2) must beDifferent(p)
            (p2).elements mustEqual List("bar", "baz", 1.23, "foo")
        }
        "prepend a second path to the path, returning a new, combined path" in {
            val p1 = sake.util.Path(List(1.23, "foo"))
            val p2 = sake.util.Path(List("bar", "baz"))
            val p21 = p2 ::: p1
            (p21) must beDifferent(p1)
            (p21) must beDifferent(p2)
            (p21).elements mustEqual List("bar", "baz", 1.23, "foo")
        }
    }
    
    "equals" should {
        "be reflexive" in {
            val p = sake.util.Path(1.23, "foo")
            (p == p) mustEqual true
        }
        "be symmetric" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path("foo", 1.23)
            (p1 == p2) mustEqual (p2 == p1)
            (p1 == p3) mustEqual (p3 == p1)
        }
        "be transitive" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path(1.23, "foo")
            (p1 == p2) mustEqual (p2 == p3)
            (p1 == p2) mustEqual (p1 == p3)
            (p2 == p3) mustEqual (p1 == p3)
        }
        "return the same value every time for the same path" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            val p3 = sake.util.Path("foo", 1.23)
            (p1 == p2) mustEqual (p1 == p2)
            (p1 == p3) mustEqual (p1 == p3)
        }
        "return false for null objects" in {
            val p1 = sake.util.Path(1.23, "foo")
            (p1 == null) mustEqual false
            
        }
        "return false for non-Path objects" in {
            val p1 = sake.util.Path(1.23, "foo")
            val l2 = List(1.23, "foo")
            (p1 == l2) mustEqual false
            
        }
        "return true for Paths with identical elements and separators" in {
            val p1 = sake.util.Path(1.23, "foo")("|")
            val p2 = sake.util.Path(1.23, "foo")("|")
            (p1 == p2) mustEqual true
        }
        "return false for Paths with identical elements but different separators" in {
            val p1 = sake.util.Path(1.23, "foo")("|")
            val p2 = sake.util.Path(1.23, "foo")("/")
            (p1 == p2) mustEqual false
        }
        "return true for Paths with different elements and identical separators" in {
            val p1 = sake.util.Path(1.24, "foo2")("|")
            val p2 = sake.util.Path(1.23, "foo")("|")
            (p1 == p2) mustEqual false
        }
    }
    
    "hashCode" should {
        "always return the same value for the same path" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path()
            (p1.hashCode) mustEqual p1.hashCode
            (p2.hashCode) mustEqual p2.hashCode
        }
        "be the same for identical paths" in {
            val p1 = sake.util.Path(1.23, "foo")
            val p2 = sake.util.Path(1.23, "foo")
            (p1.hashCode) mustEqual p2.hashCode
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
