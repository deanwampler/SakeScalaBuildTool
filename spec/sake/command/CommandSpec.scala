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
            (new Command("command")).name must be_==("command")
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
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).name must be_==("command")
        }
        "set the default options" in {
            (new Command("command", Map[Symbol,String]('x -> "x", 'y -> "y"))).defaultOptions match {
                case None => fail ()
                case Some(opts) => {
                    opts.size must be_==(2)
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
                    opts.size must be_==(2)
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
        
        "throw a BuildError if the passed-in options include specified unknown options" in {
            val c = new Command("command", Map('x -> "x")) {
                override val knownOptions = Some(List('y))
            }
            c('x -> "x2") must throwA[BuildError]
        }
        
        "invoke the action of the command" in {
            var invoked = false
            val c = new Command("command", Map('x -> "x")) {
                override def action(result: Result, options: Map[Symbol, String]) = {
                    invoked = true
                    result
                }
            }
            c()
            invoked must be_==(true)
        }
        
        "invoke a user-specified 'and' block after the action of the command" in {
            var invoked = List[Int]()
            val c = new Command("command", Map('x -> "x")) {
                override def action(result: Result, options: Map[Symbol, String]) = {
                    invoked ::= 1
                    result
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
            invoked must be_==(true)
        }

        "be able to define the command's action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def action(result: Result, options: Map[Symbol, String]) = {
                    invoked = true
                    result
                }
            }
            c('x -> "x2")
            invoked must be_==(true)
        }

        "be able to filter the result to be returned by 'action'" in {
            var invoked = false
            val c = new Command[Symbol,String]("command") {
                override def postFilterResult(result: Result) = {
                    invoked = true
                    result
                }
            }
            c('x -> "x2")
            invoked must be_==(true)
        }
    }
}