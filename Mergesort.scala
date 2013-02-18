import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.HashMap;


case class Sort
case class DoneSorting
case class GetNextLargestNumber
case class NextLargestNumber(number: Double, cluster: Cluster)

//Receives a message to start sorting, when this is done, sends a done sorting message
//back to the sender.  

class Cluster extends Actor {
  
  def act = {

    var sum = 0
    var numbers = List[Double]()
    var index = -1
    var lowestValsReceived = scala.collection.mutable.MutableList[Double]()
    
    loop {
      react {
      case numberList : List[Double] => numbers = numberList
      case Sort => numbers = numbers.sort((a,b) => a < b); index = 0; reply(numbers(0))
      case GetNextLargestNumber => if (index == numbers.length - 1) {sender ! NextLargestNumber(Double.MaxValue, this); exit}; index += 1; sender ! NextLargestNumber(numbers(index), this)
      case Total => reply(sum); exit
      case _ => println("Invalid Message")
      }
    }
  }
  
}

class Mergesort extends Actor {

	var allNumberSets = scala.collection.mutable.MutableList[List[Double]]()
	var clusters = scala.collection.mutable.MutableList[Cluster]()
	
	def addCluster(numberList : List[Double]) = {
	  val newCluster = new Cluster()
	  newCluster.start()
	  newCluster ! numberList
	  clusters += newCluster
	}
	
	def act = {

	  
		var smallestVals = new HashMap[Cluster, Double]()
		var sortedLst = scala.collection.mutable.MutableList[Double]()
		var clustersFinished = 0
		
		
		for(cluster <- clusters) {

		  cluster !? Sort match {
		    case smallestValue : Double => smallestVals.put(cluster, smallestValue)
		    case _ => println("Message not found")
		  }
		}

		var clusterPing = smallestVals.minBy(_._2)._1
		sortedLst += smallestVals.get(clusterPing).get
		
		clusterPing ! GetNextLargestNumber
		
		loop {
			var smallestValue = Double.MaxValue
			react {
			  	case NextLargestNumber(i, cluster) =>  smallestVals.put(cluster, i);
			  		clusterPing = smallestVals.minBy(_._2)._1;
			  		var smallestValFound = smallestVals.get(clusterPing).get;
			  		if (smallestValFound == Double.MaxValue) println(sortedLst);
			  		sortedLst += smallestValFound;
			  		clusterPing ! GetNextLargestNumber
			  		
				case msg : String => println(msg); exit
				case _ => println("No match on message sent to mergesort")
			}
		}
		

		
	}
  
}

object Mergesort extends Application {
  
  
  val Mergesort = new Mergesort()
  
  val list1 = List[Double](10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  val list2 = List[Double](20, 19, 18, 17, 16, 14, 11, 12, 13)


  
  Mergesort.addCluster(list1)
  Mergesort.addCluster(list2)

  
  Mergesort.start()
  
  
}



