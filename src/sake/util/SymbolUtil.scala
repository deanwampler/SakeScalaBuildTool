package sake.util

object SymbolUtil {
    def toList(item: Any) = item match { 
        case list: List[_] => toSymbols(list)
        case _ => List(toSymbol(item))
    }

    def toSymbols(list: List[Any]) = {
        for (item <- list) 
            yield toSymbol(item)
    }

    def toSymbol(item: Any) = item match {
        case s: Symbol => s
        case s: String => Symbol(s)
        case _         => Exit.error("Unrecognized type of object: \""+item+"\". Expected a Symbol or String.")
    }
}