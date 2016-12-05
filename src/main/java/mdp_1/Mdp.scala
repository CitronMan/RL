package mdp_1
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
		  
   val gamma = 0.8
   
   /**
    * given the action, return the state and the reward of transform
    */
   def transform(state:Int, action:Char):(Boolean,Int,Float) = {//n is_terminal,state, reward
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
           
        (is_terminal, next_state, r); 
  }
  

}


object Mdp{
	def main(args:Array[String]){
		val mdp = new Mdp()
		println(mdp.transform(1, 's'))
	}
}