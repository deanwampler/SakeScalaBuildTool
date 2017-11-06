package sake.command

class OptionsProcessor[A,B] {
    private[this] var procs: List[(A, B) => Option[List[B]]] = List()
    
    def processors = procs
    
    def addProcessor(proc: (A, B) => Option[List[B]]) = {
        procs ::= proc
        this
    }
    
    /**
     * Process the map into a list, which is built in reverse order, 
     * due to list semantics, then reversed.
     */
    def processOptionsToList(options: Map[A,B]): List[B] = {
        options.foldLeft(List[B]()) { (list, k_v) => 
            processOption(k_v._1, k_v._2) ::: list
        }.reverse
    }

    def processOption(key: A, value: B): List[B] = {
        for {
            f <- procs
            list = f(key, value)
        } list match {
            case Some(s) => return s.reverse
            case None =>
        }
        Nil
    }
}

