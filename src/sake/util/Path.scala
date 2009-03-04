package sake.util

import sake.environment._

/**
 * Encapsulate path-like structures, sequences of strings separated with a delimeter.
 */
class Path(elems: Seq[Any])(implicit val separator: String) {

    val elements = elems.toList flatten {_ match {
            case i:Iterable[_] => i
            case elem => List(elem)
        }
    }
    
    def this()(implicit sep: String) = this(Nil)(sep)
    def this(element: String)(implicit sep: String) = this(element.split(sep).toList)(sep)
    def this(elem1: Any, elem2: Any, elems: Any*)(implicit sep: String) = 
        this(elem1:: elem2 :: elems.toList)(sep)
    
    /**
     * Append an element to the path, returning a new Path.
     * @note Avoids use of the deprecated List.+() method.
     * @note O(n)
     */
    def +(appendedElement: Any) = 
        new Path(elements ::: List(appendedElement))(separator)

    /**
     * Append the sequence to the path, returning a new Path.
     * @note O(n+m)
     */
    def ++(appendedElements: Seq[Any]) = 
        new Path(elements ++ appendedElements)(separator)

    /**
     * Append another path to the path, returning a new, combined Path.
     * @note O(n+m)
     */
    def ++(path: Path) = 
        new Path(elements ++ path.elements)(separator)

    /**
     * Prepend an element to the path, returning a new Path.
     */
    def ::(prependedElement: Any) = 
        new Path(prependedElement :: elements)(separator)

    /**
     * Prepend the sequence to the path, returning a new Path.
     */
    def :::(prependedElements: Seq[Any]) = 
        new Path(prependedElements.toList ::: elements)(separator)

    /**
     * Prepend another path to the path, returning a new, combined Path.
     * @note O(n+m)
     */
    def :::(path: Path) = 
        new Path(path.elements ::: elements)(separator)

    /**
     * Join into a {@ link separator} separated string, with element converted to a string.
     */
    def joined: String = {
        val strings = elements.map(_.toString())
        strings.length match {
            case 0 => ""
            case _ => strings.reduceLeft(_ + separator + _)
        }
    }
    
    override def toString() = joined
    
    override def equals(other: Any) = other match {
        case p:Path => p.isInstanceOf[Path] && (elements == p.elements) && (separator == p.separator)
        case _ => false
    }
    
    override def hashCode = 41*(41 + elements.hashCode)*(41 + separator.hashCode)
}

object Path {
    
    def apply()(implicit separator: String)  = new Path()(separator)
    def apply(element: String)(implicit separator: String)  = new Path(element)(separator)
    def apply(elements: Seq[Any])(implicit separator: String)  = new Path(elements)(separator)
    def apply(elem1: Any, elem2: Any, elems: Any*)(implicit separator: String) = 
            new Path(elem1:: elem2 :: elems.toList)(separator)
    
    implicit val pathStringSeparator: String = Environment.environment.pathSeparator
}

object NilPath extends Path(Nil)(Path.pathStringSeparator)
