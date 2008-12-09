package sake

import org.specs._ 
object SymbolUtilSpec extends Specification {

    import sake.util._
    import sake.util.SymbolUtil._

    "toSymbol" should {
        "return the same symbol if given a symbol" in {
            toSymbol('symbol) must be_==('symbol)
        }

        "return the corresponding symbol if given a string" in {
            toSymbol("symbol") must be_==('symbol)
        }
        
        "throw a BuildError if the given item is not a string and not a symbol" in {
            toSymbol(1) must throwA[BuildError]
        }
    }

    "toSymbols" should {
        "return an empty list if given an empty list" in {
            toSymbols(Nil) must be_==(Nil)
            toSymbols(List()) must be_==(List())
        }

        "return a list of symbols if given all symbols" in {
            toSymbols(List('symbol1, 'symbol2)) must be_==(List('symbol1, 'symbol2))
        }

        "return a list of symbols if given mixed symbols and strings" in {
            toSymbols(List('symbol1, "symbol2", "symbol3", 'symbol4)) must be_==(
                List('symbol1, 'symbol2, 'symbol3, 'symbol4))
        }
        
        "throw a BuildError if any List item is not a string and not a symbol" in {
            toSymbols(List('symbol1, 2, "symbol3", 'symbol4)) must throwA[BuildError]
        }
    }
    
    "removeDuplicates" should {
        "return an empty list if given an empty list" in {
            removeDuplicates(Nil) must be_==(Nil)
            removeDuplicates(List()) must be_==(List())
        }
    }
}
