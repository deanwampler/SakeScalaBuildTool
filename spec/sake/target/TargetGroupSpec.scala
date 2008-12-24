package sake

import org.specs._ 

object TargetGroupSpec extends Specification {
    
    import sake.target._
        
    "An empty TargetGroup" should {
        "have no targets" in {
            (new TargetGroup).targets mustEqual Nil
        }
    }
    
    "A TargetGroup with one target" should {
        "have a list with the single target" in {
            val t1 = Target('t1)
            val tg = new TargetGroup(t1)
            tg.targets.size mustEqual 1
            tg.targets.head mustEqual t1
        }
    }
    
    "A TargetGroup with N targets" should {
        "have a list with the N targets" in {
            val t1 = Target('t1)
            val t2 = Target('t2)
            val t3 = Target('t3)
            val list = List(t1,t2,t3)
            val tg = new TargetGroup(list)
            tg.targets.size mustEqual 3
            tg.targets mustEqual list
        }
    }
    
    "Using '::'" should {
        "return a new TargetGroup with the added Target prepended to the original list." in {
            val t1 = Target('t1)
            val t2 = Target('t2)
            val t3 = Target('t3)
            val tg1 = new TargetGroup(List(t2,t3))
            val tg2 = t1 :: tg1
            tg1.targets mustEqual List(t2,t3)
            tg2.targets mustEqual List(t1,t2,t3)
            
        }
    }

    "Concatenation using ':::'" should {
        "return a new TargetGroup with the added Target list prepended to the original list." in {
            val t1 = Target('t1)
            val t2 = Target('t2)
            val t3 = Target('t3)
            val t4 = Target('t4)
            val tg1 = new TargetGroup(List(t1,t2))
            val tg2 = new TargetGroup(List(t3,t4))
            val tg3 = tg1 ::: tg2
            tg1.targets mustEqual List(t1,t2)
            tg2.targets mustEqual List(t3,t4)
            tg3.targets mustEqual List(t1,t2,t3,t4)
        }
    }

    "apply()" should {
        "return an updated TargetGroup with new Targets, each of which has the new 'action'." in {
            val t1 = Target('t1)
            val t2 = Target('t2)
            val t3 = Target('t3)
            val tg1 = new TargetGroup(List(t1,t2,t3))
            val tg1Targets = tg1.targets
            val tg2 = tg1 {println("hello!")}
            tg2.targets foreach { t2 =>
                tg1Targets.find(t => t.name == t2.name) match {
                    case None => fail(t2.name.toString())
                    case Some(t1) => (t1 eq t2) mustEqual false
                }
            }
        }
    }
}