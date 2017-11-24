package sake.command

import org.scalatest._
import org.scalatest.Matchers._

object CommandSpec extends FreeSpec {

    import sake.command._
    import sake.util._

    def checkOption[A,B](opts: Map[A,B], key: A, expected: B) = opts.get(key) match {
        case Some(expected) =>
        case x => fail("for key: "+key+", "+x+" != Some("+expected+")")
    }

    "Constructing a Command with just a name" should {
        "set the name" in {
            (new Command("command")).name shouldEqual "command"
        }
        "have no default options" in {
            (new Command("command")).defaultOptions must beNone
        }
    }

    "Constructing a Command with a name and default options map" should {
        "set the name" in {
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).name shouldEqual "command"
        }
        "set the default options" in {
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).defaultOptions match {
                case None => fail ()
                case Some(opts) => {
                    opts.size shouldEqual 2
                    opts.get('x) must be like { case Some("x") => true }
                    opts.get('y) must be like { case Some("y") => true }
                }
            }
        }
    }

    "Constructing a Command with a name and default options pairs" should {
        "set the default options" in {
            (new Command("command", 'x -> "x", 'y -> "y")).defaultOptions match {
                case None => fail ()
                case Some(opts) => {
                    opts.size shouldEqual 2
                    opts.get('x) must be like { case Some("x") => true }
                    opts.get('y) must be like { case Some("y") => true }
                }
            }
        }
    }

    "Invoking a Command" should {
        "merge the passed-in options with the default options, overriding the later" in {
            val c = new Command("command", Map('x -> "x2", 'y -> "y")) {
                override def optionsPostFilter(opts: Map[Symbol,String]) = {
                    opts.get('x) must be like { case Some("x2") => true }
                    opts.get('y) must be like { case Some("y")  => true }
                    opts
                }
            }
            c('x -> "x2")
        }

        "throw a BuildError if the passed-in options are missing required options" in {
            val c = new Command("command", Map('x -> "x")) {
                override val requiredOptions = List('y)
            }
            c('x -> "x2") must throwA[BuildError]
        }

        "invoke the action of the command" in {
            var invoked = false
            val c = new Command("command", Map('x -> "x")) {
                override def action(options: Map[Symbol, String]) = {
                    invoked = true
                    Passed()
                }
            }
            c()
            invoked shouldEqual true
        }

        "throw a BuildError if the command fails" in {
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = Failed()
            }
            c() must throwA[BuildError]
        }

        "not throw a BuildError if the command's postFilterResult restores the status to passed" in {
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = Failed()
                override def postFilterResult(r: Result) = Passed()
            }
            c() must not(throwA[BuildError])
        }

        "invoke a user-specified 'and' block after the action of the command" in {
            var invoked = List[Int]()
            val c = new Command("command", Map('x -> "x")) {
                override def action(options: Map[Symbol,String]) = {
                    invoked ::= 1
                    Passed()
                }
            }
            c() and { result =>
                invoked ::= 2
                result
            }
            invoked must be like { case 2 :: List(1) => true }
        }

        "allow a user-specified 'and' block to process a successful result" in {
            var result:Result = Passed(Some(1))
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = result
            }
            (c() and { r => result = Passed(Some(2)); result }) must not(throwA[BuildError])
            result match {
                case Passed(r,msg) => r shouldEqual Some(2)
                case Failed(r,msg) => fail()
            }
        }

        "not allow a user-specified 'and' block to process a failed result (i.e., execution never gets to the block)" in {
            var result:Result = Failed()
            val expected = result
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = result
            }
            (c() and { r => result = Passed(Some(2)); result }) must throwA[BuildError]
            (result eq expected) shouldEqual true
        }
    }

    "Subclasses" should {
        "be able to filter the specified options" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def optionsPostFilter(opts: Map[Symbol,String]) = {
                    invoked = true
                    opts
                }
            }
            c('x -> "x2")
            invoked shouldEqual true
        }

        "be able to define the command's action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def action(options: Map[Symbol, String]) = {
                    invoked = true
                    Passed()
                }
            }
            c('x -> "x2")
            invoked shouldEqual true
        }

        "be able to filter the result to be returned by 'action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def postFilterResult(result: Result) = {
                    invoked = true
                    Passed()
                }
            }
            c('x -> "x2")
            invoked shouldEqual true
        }
    }
}
