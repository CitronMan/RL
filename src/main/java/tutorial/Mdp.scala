package tutorial
/**
 * author:liuxianggen
 * Markow Decision Processes model
 * the property of environment includes states,terminal states,and transform matrix, 
 * the rewards with respect to the action 
 */


class Gamber_Mdp() {
	val ph = 0.4f
    val states: Array[Int] = Array.range(0,101) //0-100,100 indicates end
  	val terminal_states = Map(100->1.0f,0->1.0f)
  	
  	var current_state = scala.util.Random.nextInt(99)+1 
  	
	  
    def getActions:Array[Int] = {
    	 val max_action = math.min(this.current_state,100-this.current_state)
    	(0 to max_action).map(x => x).toArray
    }
 
  	//transfrom matrix
    
    
  	val t = Map("0_s"-> 5,
  		  "0_e"-> 1,
		  "1_w"-> 0,
		  "1_e"-> 2,
		  "2_s"-> 6,
		  "2_w"-> 1,
		  "2_e"-> 3,
		  "3_w"-> 2,
		  "3_e"-> 4,
		  "4_s"-> 7,
		  "4_w"-> 3)
		  
   val gamma = 0.95f
   

   
   
   /**
    * given the action, return the state and the reward of transform
    */
 
  
   //initial , and return the feature of state
   def start(initial_state:Int = 1) {
	  this.current_state = initial_state
  }
   
  	def expectation(action:Int, value:Array[Float]):(Boolean,Float) = {
  		var is_terminal = false
        var r = 0f
	   	val state = this.current_state
	   if(100 == (this.current_state+action)){
	  	   is_terminal = true
	  	   r = 1
	   }
	   if(0 == this.current_state-action){
	  	   is_terminal = true
	   }
       val max_action = math.min(state,100-state)
	   if(action>max_action)
	  	   throw new java.lang.IllegalArgumentException("action error")
	   val e = this.ph * (r + this.gamma * value(this.current_state + action)) +
	   (1-this.ph) * (this.gamma * value(this.current_state - action))
    	   
    	(is_terminal,e.toFloat)
           
  	}
    
   /**
    * given the action,update the current state,
    *  return the state_feature and the reward of transform
    */
   def receive(action:Int): NextState = {//n is_terminal,state, reward
	   var is_terminal = false
       var r = 0f
	   val state = this.current_state
       if(this.terminal_states.contains(state))
           (true, state, 0)
       else{
    	   val max_action = math.min(state,100-state)
    	   if(action>max_action)
    	  	   throw new java.lang.IllegalArgumentException("action error")
    	   if(scala.util.Random.nextFloat()<this.ph)
    	   		this.current_state += action
    	   else {
    	        this.current_state -= action
    	   }
    	   
    	   if(100 == this.current_state){
    	  	   is_terminal = true
    	  	   r = 1f
    	   }
    	     
    	     
    	   if(0 == this.current_state){
    	  	   is_terminal = true
    	  	   r = -1f
    	   }
            	
           
           
       }
       new NextState(is_terminal, this.current_state, r) 
  }
  
}

class NextState(var is_terminal:Boolean,var next_state:Int,var r:Float)
//

object Mdp{
	def main(args:Array[String]){
		val mdp = new Gamber_Mdp()
		mdp.start(20)
//		mdp.receive(2)
//		println(mdp.current_state)
//		expectation(10,)
		
//		println(mdp.transform(1, 's'))
//		val res = mdp.gen_random_pi_sample(10)
//		res._1 zip res._2 zip res._3 foreach(x =>{
//			println(s"state:${x._1._1}\t action:${x._1._2} \t reward:${x._2}")
//		})
//		
		println("dd")
	}
}