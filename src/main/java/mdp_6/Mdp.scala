package mdp_6
/**
 * author:liuxianggen
 * Markow Decision Processes model
 * the property of environment includes states,terminal states,and transform matrix, 
 * the rewards with respect to the action 
 */

abstract class Mdp(val initial_state:Int = -1){
	val states: Array[Int] = Array.range(0,8) //1-8,0 indicates end
  	val terminal_states = Map(5->1.0f,6->1f,7->1f)
  	var current_state   = 0
  	var current         = current_init(initial_state)
  	def current_init(initial_state:Int) : Int = {
	  if(initial_state == -1) scala.util.Random.nextInt(5) 
  	  else if(initial_state >4) throw new java.lang.IllegalAccessError(s"${initial_state} is a terminal state")
  	  else initial_state
  }
	  
  
  
    val actions         = Array('n','e','s','w')
    val rewards         = Map("0_s"-> -1.0f,"2_s"->1f,"4_s"-> -1f)
  
  //features of states
  //8*8 Int
  	val feas:Array[Array[Int]]
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
		  
   val gamma = 0.8f
   
   def start(initial_state:Int = -1):Array[Int]
   
   
   /**
    * given the action, return the state and the reward of transform
    */
   def receive(action:Char): NextStateFeat 
  
}

class Mdp_ID() extends Mdp {  
  //features of states
  //8*8 Int
  override val feas = Array.fill[Array[Int]](states.length)(Array.fill[Int](states.length)(0))
  for(i<- 0 until 8){
	  feas(i)(i) = 1
  }
  
  /**
   * as feature of states
        self.feas    = dict();
        self.feas[1] = np.array([1,0,0,0,0,0,0,0]);
        self.feas[2] = np.array([0,1,0,0,0,0,0,0]);
        self.feas[3] = np.array([0,0,1,0,0,0,0,0]);
        self.feas[4] = np.array([0,0,0,1,0,0,0,0]);
        self.feas[5] = np.array([0,0,0,0,1,0,0,0]);        
        self.feas[6] = np.array([0,0,0,0,0,1,0,0]);
        self.feas[7] = np.array([0,0,0,0,0,0,1,0]);
        self.feas[8] = np.array([0,0,0,0,0,0,0,1]);
   */
   
   override def start(initial_state:Int = -1):Array[Int]= {
	  this.current_state  =  0
	  this.current        = current_init(initial_state)
	  this.feas(this.current)
  }
   
   
   /**
    * given the action, return the state and the reward of transform
    */
   override def receive(action:Char): NextStateFeat = {//n is_terminal,state, reward
       val state = this.current
       if(this.terminal_states.contains(state))
           (true, this.feas(state), 0)

        val key = s"${state}_$action"
        //if it's out of bound, then the agent stay  
        if(this.t.contains(key)) 
            this.current = this.t(key); 
        var is_terminal = false
        if(this.terminal_states.contains(this.current))
            is_terminal = true
        var r = 0.0f
        if(this.rewards.contains(key))
            r = this.rewards(key);
           
        new NextStateFeat(is_terminal, this.feas(state), r); 
  }
  

}

class Mdp_Code() extends Mdp {
  
   //8*8 Int
  	override val feas = Array.fill[Array[Int]](this.states.length)(Array.fill[Int](states.length)(0))
		  //why the last three state feature is the same?  
		   this.feas(0) = Array(1,0,0,1)
		   this.feas(1) = Array(1,0,1,0)
		   this.feas(2) = Array(1,0,0,0)
		   this.feas(3) = Array(1,0,1,0)
		   this.feas(4) = Array(1,1,0,0)
		   this.feas(5) = Array(0,1,1,1)
		   this.feas(6) = Array(0,1,1,1)
		   this.feas(7) = Array(0,1,1,1)
  
   //initial , and return the feature of state
   override def start(initial_state:Int = 0):Array[Int]= {
	  this.current_state  =  1
	  this.current        = current_init(initial_state)
	  this.feas(this.current)
  }
   
   /**
    * given the action,update the current state,
    *  return the state_feature and the reward of transform
    */
   override def receive(action:Char): NextStateFeat = {//n is_terminal,state, reward
	   
	   val state = this.current
       if(this.terminal_states.contains(state))
           (true, this.feas(state-1), 0)

        val key = s"${state}_$action"
        //if it's out of bound, then the agent stay  
        if(this.t.contains(key)) 
            this.current = this.t(key); 
        var is_terminal = false
        if(this.terminal_states.contains(this.current))
            is_terminal = true
        var r = 0.0f
        if(this.rewards.contains(key))
            r = this.rewards(key);
           
        new NextStateFeat(is_terminal, this.feas(state-1), r); 
  }
  

}




class NextStateFeat(var is_terminal:Boolean,var next_state:Array[Int],var r:Float)
//

object Mdp{
	def main(args:Array[String]){
		val mdp = new Mdp_ID()
//		println(mdp.transform(1, 's'))
//		val res = mdp.gen_random_pi_sample(10)
//		res._1 zip res._2 zip res._3 foreach(x =>{
//			println(s"state:${x._1._1}\t action:${x._1._2} \t reward:${x._2}")
//		})
//		
	}
}