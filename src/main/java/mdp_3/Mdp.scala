package mdp_3
/**
 * author:liuxianggen
 * Markow Decision Processes model
 * the property of environment includes states,terminal states,and transform matrix, 
 * the rewards with respect to the action 
 */
class Mdp {
  val states  = Array.range(1,9) //1-8,0 indicates end
  val terminal_states = Map(6->1.0f,7->1f,8->1f)
  val actions = Array('n','e','s','w')
  val rewards = Map("1_s"-> -1.0f,"3_s"->1f,"5_s"-> -1f)
  
  //transfrom matrix
  val t = Map("1_e"-> 2,
		  "2_w"-> 1,
		  "2_e"-> 3,
		  "1_s"-> 6,
		  "3_s"-> 7,
		  "3_w"-> 2,
		  "3_e"-> 4,
		  "4_w"-> 3,
		  "4_e"-> 5,
		  "5_s"-> 8,
		  "5_w"-> 4)
		  
   val gamma = 0.8f
   
 	
   def gen_random_pi_sample(num:Int) :(Array[Array[Int]],Array[Array[Char]],Array[Array[Float]])= {
	  var s_sample = Array[Array[Int]]()
	  var a_sample = Array[Array[Char]]()
	  var r_sample = Array[Array[Float]]()
	 	  
	  
	  for(i <- 0 until num){
	 	  var s_tmp = Array[Int]()
	 	  var a_tmp = Array[Char]()
	 	  var r_tmp = Array[Float]()
	 	  
	 	  var s = this.states(scala.util.Random.nextInt(this.states.length))
	 	  var t = false
	 	  while(false == t){
	 	 	  var a = this.actions(scala.util.Random.nextInt(this.actions.length))
	 	 	  val nextState = this.transform(s, a) 
	 	 	  s_tmp = s_tmp :+ s
	 	 	  r_tmp = r_tmp :+ nextState.r
	 	 	  a_tmp = a_tmp :+ a
	 	 	  s = nextState.next_state
	 	 	  t = nextState.is_terminal
	 	  }
 	  	  s_sample = s_sample :+ s_tmp
 	 	  r_sample = r_sample :+ r_tmp
 	 	  a_sample = a_sample :+ a_tmp
	  }
	  return (s_sample,a_sample,r_sample)
  }
   
   /**
    * given the action, return the state and the reward of transform
    */
   def transform(state:Int, action:Char): NextState = {//n is_terminal,state, reward
        if(this.terminal_states.contains(state))
           (true, state, 0)

        val key = s"${state}_$action"
        var next_state = state  
        //if it's out of bound, then the agent stay  
        if(this.t.contains(key)) 
            next_state = this.t(key); 
        var is_terminal = false
        if(this.terminal_states.contains(next_state))
            is_terminal = true
        var r = 0.0f
        if(this.rewards.contains(key))
            r = this.rewards(key);
           
        new NextState(is_terminal, next_state, r); 
  }
  

}

class NextState(var is_terminal:Boolean,var next_state:Int,var r:Float)
//

object Mdp{
	def main(args:Array[String]){
		val mdp = new Mdp()
//		println(mdp.transform(1, 's'))
		val res = mdp.gen_random_pi_sample(10)
		res._1 zip res._2 zip res._3 foreach(x =>{
			println(s"state:${x._1._1}\t action:${x._1._2} \t reward:${x._2}")
		})
		
	}
}