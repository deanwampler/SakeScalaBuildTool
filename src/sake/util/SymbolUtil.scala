package sake.util

object SymbolUtil {

    def toSymbols(list: Iterable[Any]): List[Symbol] = 
        Nil ++ (for (item <- list) yield toSymbol(item))

    def toSymbol(item: Any) = item match {
        case s: Symbol => s
        case s: String => Symbol(s)
        case _         => Exit.error("Unrecognized type of object: \""+item+"\". Expected a Symbol or String.")
    }
    
    def removeDuplicates(list: Iterable[Symbol]) = {
        // Preserve the order. ListSet doesn't seem to do this. Hmmm.
        list.foldLeft(List[Symbol]()) { (list2, item) => 
            if (list2.contains(item)) 
                list2
            else 
                item :: list2
        }.reverse
    }
}