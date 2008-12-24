package sake.command

import org.specs._ 

object CommandSpec extends Specification {
    
    import sake.command._
    import sake.util._
    
    def checkOption[A,B](opts: Map[A,B], key: A, expected: B) = opts.get(key) match {
        case Some(expected) =>
        case x => fail("for key: "+key+", "+x+" != Some("+expected+")")
    }
    
    "Constructing a Command with just a name" should {
        "set the name" in {
            (new Command("command")).name mustEqual "command"
        }
        "have no default options" in {
            (new Command("command")).defaultOptions match {
                case None =>
                case Some(opts) => fail()
            }
        }
    }
    
    "Constructing a Command with a name and default options map" should {
        "set the name" in {
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).name mustEqual "command"
        }
        "set the default options" in {
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).defaultOptions match {
                case None => fail ()
                case Some(opts) => {
                    opts.size mustEqual 2
                    checkOption(opts, 'x, "x")
                    checkOption(opts, 'y, "y")
                }
            }
        }
    }
    
    "Constructing a Command with a name and default options pairs" should {
        "set the default options" in {
            (new Command("command", 'x -> "x", 'y -> "y")).defaultOptions match {
                case None => fail ()
                case Some(opts) => {
                    opts.size mustEqual 2
                    checkOption(opts, 'x, "x")
                    checkOption(opts, 'y, "y")
                }
            }
        }
    }
    
    "Invoking a Command" should {
        "merge the passed-in options with the default options, overriding the later" in {
            val c = new Command("command", Map('x -> "x", 'y -> "y")) {
                override def optionsPostFilter(opts: Map[Symbol,String]) = {
                    checkOption(opts, 'x, "x2")
                    checkOption(opts, 'y, "y")
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
                    new Passed()
                }
            }
            c()
            invoked mustEqual true
        }
        
        "throw a BuildError if the command fails" in {
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = new Failed()
            }
            c() must throwA[BuildError]
        }
        
        "not throw a BuildError if the command's postFilterResult restores the status to passed" in {
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = new Failed()
                override def postFilterResult(r: Result) = new Passed()
            }
            c() must not(throwA[BuildError])
        }
        
        "invoke a user-specified 'and' block after the action of the command" in {
            var invoked = List[Int]()
            val c = new Command("command", Map('x -> "x")) {
                override def action(options: Map[Symbol,String]) = {
                    invoked ::= 1
                    new Passed()
                }
            }
            c() and { result => 
                invoked ::= 2 
                result
            }
            invoked match {
                case 2 :: List(1) =>
                case _ => fail(invoked.toString())
            }
        }
        
        "allow a user-specified 'and' block to process a successful result" in {
            var result:Result = new Passed(Some(1))
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = result
            }
            (c() and { r => result = new Passed(Some(2)); result }) must not(throwA[BuildError])
            result match {
                case Passed(r,msg) => r mustEqual Some(2)
                case Failed(r,msg) => fail()
            }
        }

        "not allow a user-specified 'and' block to process a failed result (i.e., execution never gets to the block)" in {
            var result:Result = new Failed()
            val expected = result
            val c = new Command[Symbol,String]("fail") {
                override def action(o: Map[Symbol,String]) = result
            }
            (c() and { r => result = new Passed(Some(2)); result }) must throwA[BuildError]
            (result eq expected) mustEqual true
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
            invoked mustEqual true
        }

        "be able to define the command's action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def action(options: Map[Symbol, String]) = {
                    invoked = true
                    new Passed()
                }
            }
            c('x -> "x2")
            invoked mustEqual true
        }

        "be able to filter the result to be returned by 'action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def postFilterResult(result: Result) = {
                    invoked = true
                    new Passed()
                }
            }
            c('x -> "x2")
            invoked mustEqual true
        }
    }
}