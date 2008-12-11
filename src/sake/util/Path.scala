package sake.util

import sake.environment.Environment._

object Path {
    
    val delim:String = environment.pathSeparator
    
    def apply(elements: Seq[_]):String = 
        elements.map(_.toString()).reduceRight(_ + Path.delim + _)
}