package sake

import org.scalatest._
import org.scalatest.Matchers._

object SymbolUtilSpec extends FreeSpec {

    import sake.util._
    import sake.util.SymbolUtil._

    "toSymbol" should {
        "return the same symbol if given a symbol" in {
            toSymbol('symbol) shouldEqual 'symbol
        }

        "return the corresponding symbol if given a string" in {
            toSymbol("symbol") shouldEqual 'symbol
        }

        "throw a BuildError if the given item is not a string and not a symbol" in {
            toSymbol(1) must throwA[BuildError]
        }
    }

    "toSymbols" should {
        "return an empty list if given an empty list" in {
            toSymbols(Nil) shouldEqual Nil
            toSymbols(List()) shouldEqual List()
        }

        "return a list of symbols if given all symbols" in {
            toSymbols(List('symbol1, 'symbol2)) shouldEqual List('symbol1, 'symbol2)
        }

        "return a list of symbols if given mixed symbols and strings" in {
            toSymbols(List('symbol1, "symbol2", "symbol3", 'symbol4)) shouldEqual List('symbol1, 'symbol2, 'symbol3, 'symbol4)
        }

        "throw a BuildError if any List item is not a string and not a symbol" in {
            toSymbols(List('symbol1, 2, "symbol3", 'symbol4)) must throwA[BuildError]
        }
    }

    "removeDuplicates" should {
        "return an empty list if given an empty list" in {
            removeDuplicates(Nil) shouldEqual Nil
            removeDuplicates(List()) shouldEqual List()
        }

        "return an equivalent list if given a list without duplicates" in {
            removeDuplicates(List('t1)) shouldEqual List('t1)
            removeDuplicates(List('t1, 't2)) shouldEqual List('t1, 't2)
            removeDuplicates(List('t1, 't2, 't3)) shouldEqual List('t1, 't2, 't3)
        }

        "return a list with duplicates removed preserving the order of the original entries" in {
            removeDuplicates(List('t1, 't2, 't2, 't3, 't4, 't4, 't5, 't5)) shouldEqual List('t1, 't2, 't3, 't4, 't5)
        }
    }
}
