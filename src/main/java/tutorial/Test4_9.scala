package tutorial
import breeze.linalg._
import breeze.plot._
import scala.util.control.Breaks
import util.Draw
object Test4_9 {
//  def max_action(mdp:Gamber_Mdp) :Int = {
//	  val actions = mdp.getActions
////	  val actions = Array('n','e','s','w')
//	  val ns = mdp.receive(actions(0))
//	  var max_v = mdp.ph * ((ns.r) + mdp.gamma *   
//	  actions.indices.foreach { a => {
//	 	  
//	  } }
//	  val r = scala.util.Random.nextInt(actions.length)
//	  actions(r)
//  }
  
  def compute_random_pi_state_value{
	  //draw
	  val draw = new Draw()
	  val draw1 = new Draw()
	   
	  val value = Array.fill[Float](101)(1f)
//	  val value = Array.range(0,101).reverse.map(_.toFloat/100.0f)
//	  var policy_action = Array.fill[Int](101)(1)
	  val n_iter = 600
	  var theta = 1e-8
	  val loop = new Breaks
	  loop.breakable{
		  for(k<- 1 until n_iter){
//		 	  println(k)
		 	  for(i<- 1 until 100){
		 	 	  val mdp = new Gamber_Mdp()
		 	 	  mdp.start(i)
		 	 	  var is_terminal = false
		 	 	  var gamma = 1f
		 	 	  var state_value = 0f
	 	 	 	  val actions = mdp.getActions
				  var mdpe = mdp.expectation(actions(0), value)
				  var maxe = mdpe._2
				  var maxa = actions(0)
				  actions.indices.foreach(a => {
				 	  val e = mdp.expectation(a, value)._2
				 	  if(maxe < e){
				 	 	  maxe  = e
				 	 	  maxa = actions(a)
//				 	 	  println(maxa)
				 	  }
				  })
				  val delta = math.max(theta,math.abs(value(mdp.current_state) -  maxe))
				  println("*************")
				  println(delta)
				  if(delta<theta)
				 	  loop.break()
	 	 	 	  value(mdp.current_state) = maxe
//		 	 	  policy_action(mdp.current_state) = maxa
//		 	 	 	  val ns = mdp.receive(maxa)//is_terminal,state, reward
//		 	 	 	  is_terminal = ns.is_terminal
//		 	 	 	  mdp.current_state = ns.next_state
//		 	 	  }
		 	  }
		 	   
		 	  if(k < 4){
		 	 	  
					draw.add_line(value.indices.toArray.take(value.length-1), value.take(value.length-1))
//					draw1.add_line(policy_action.indices.toArray.take(policy_action.length-1), policy_action.take(policy_action.length-1),'.')
//		 	 	  value.foreach(x => print(x + "  "))
//		 	 	  println
		 	  }
		 	   
//		 	  if(k==10)
//		 	 	  draw1.add_line(policy_action.indices.toArray.take(policy_action.length-1), policy_action.take(policy_action.length-1),'.')
		 	 
		  }
		    //get policy
			var policy_action = Array.fill[Int](100)(1)
	 	    val mdp = new Gamber_Mdp()
			for(i<- 1 until 100){
		 	   	mdp.start(i)
 	 	 	  	var actions = mdp.getActions
 	 	 	  	actions = actions.takeRight(actions.length)
 	 	 	  	var maxa1 = actions(0)
			  	var mdpe = mdp.expectation(maxa1, value)
				var maxe = mdpe._2
  			  	
  			  	println(s"[state]:$i")
				actions.indices.foreach(a => {
					val e = mdp.expectation(a, value)._2
					println(s"[expectation]:\t$e")
					if(maxe < e){
					 	 	  maxe  = e
					 	 	  maxa1 = actions(a)
					 	 	  
					 	  }
//					println(maxa1)
//					println("**")
				})
				
				policy_action(i) = maxa1
				println(s"[take actions]:\t\t $maxa1")
			}
//			   policy_action.foreach(print)
		 	   draw1.add_line(policy_action.indices.toArray, policy_action,'.')
		 	   draw1.draw()
	  }
	  
	  draw.add_line(value.indices.toArray.take(value.length-1), value.take(value.length-1))
//	  draw1.add_line(policy_action.indices.toArray.take(policy_action.length-1), policy_action.take(policy_action.length-1),'.')
	  draw.draw() 
//	  draw1.draw()
	  //value.foreach(x => print(x + "  "))
  }
//  
  def main(args:Array[String]){
	  compute_random_pi_state_value
  }
}