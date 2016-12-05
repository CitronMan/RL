package tutorial1

object Prime1 
{
  def main (args:Array[String])
  {
    prime(readLine().toInt).foreach {println}
  }
  
  def prime(n:Int): Array[Int]= {
	  val A1=n
    if (A1<2) print("Error")
    import scala.collection.mutable.ArrayBuffer
    val A2=ArrayBuffer[Int]()
    if (A1==2) A2+=2
    for (i<-3 to A1)
    {
      if (i%2==1) A2+=i
    }
    import scala.util.control.Breaks._
    var count=0
    for (j<-1 until A2.length)
    {
      breakable
      {
        for (k<-0 until A2.length)
        {
          if (A2(k)==A2(j)) break
          if (A2(j)%A2(k)==0&&A2(k)!=1)
          {
            count+=1
            A2(j)=1
//            println(A2)
          }
        }
      }
    }
    val A2Sorted=A2.sorted
    A2Sorted.remove(0,count)
    A2Sorted.toArray
  }
}