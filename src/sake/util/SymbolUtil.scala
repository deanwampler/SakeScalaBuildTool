package sake.util

object SymbolUtil {

    def toSymbols(list: Collection[Any]): List[Symbol] = 
        Nil ++ (for (item <- list) yield toSymbol(item))

    def toSymbol(item: Any) = item match {
        case s: Symbol => s
        case s: String => Symbol(s)
        case _         => Exit.error("Unrecognized type of object: \""+item+"\". Expected a Symbol or String.")
    }
    
    def removeDuplicates(list: Collection[Symbol]) = {
        list.foldLeft(Set.empty[Symbol]) { (set, item) => 
            set + item
        }.toList
    }
}