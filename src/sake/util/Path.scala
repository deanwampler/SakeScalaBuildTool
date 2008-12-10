package sake.util

import sake.environment.Environment._

object Path {
    
    val delim:String = environment.pathSeparator
    
    // TODO: Must be a more elegant way...
    def apply(elements: Seq[_]):String = 
        elements.foldLeft("") { (s,elem) => 
            if (s == "")
                elem.toString()
            else if (elem == "")
                s
            else
                s + Path.delim + elem.toString() 
        }
}