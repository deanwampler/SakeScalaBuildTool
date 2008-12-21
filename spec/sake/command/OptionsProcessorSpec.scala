package sake.command

import org.specs._ 

import sake.command._

object OptionsProcessorSpec extends Specification {
    "OptionsProcessor" should {
        "have no processor functions by default" in {
            new OptionsProcessor[String,String]().processors.length must be_==(0)
        }
    }
    
    "addProcessor" should {
        "should increment the number of processors" in {
            val op = new OptionsProcessor[String,String]()
            op.processors.length must be_==(0)
            op.addProcessor((key, value) => Some(List(value)))
            op.processors.length must be_==(1)
        }
        
        "should give the last-added processor the highest priority" in {
            val op = new OptionsProcessor[String,String]()
            op.processors.length must be_==(0)
            op.addProcessor((key, value) => Some(List("1")))
            op.addProcessor((key, value) => Some(List("2")))
            op.processors.length must be_==(2)
            op.processOptionsToList(Map("a" -> "A", "b" -> "B")) must be_==(List("2", "2"))
        }
    }

    "processOptionsToList" should {
        "return Nil for unhandled options" in {
            val op = new OptionsProcessor[String,String]()
            op.addProcessor((key, value) => None)
            op.processOptionsToList(Map("a" -> "A", "b" -> "B")) must be_==(Nil)
        }

        "return a non-empty list for handled options" in {
            val op = new OptionsProcessor[String,String]()
            op.addProcessor((key, value) => key match {
                case "a" => Some(List(value))
                case "b" => Some(List(value, value))
                case _ => None
            })
            op.processOptionsToList(Map("a" -> "A", "b" -> "B")) must be_==(List("A", "B", "B"))
        }
    }
    
}