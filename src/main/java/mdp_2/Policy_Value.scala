package mdp_2
import scala.util.control.Breaks
class Policy_Value(val mdp:Mdp) {
	
	//state value
  	val value  = Array.fill[Float](mdp.states.length+1)(0f)
  	
  	// pi: the policy, set to the action(0) when initial
  	var pi = mdp.states.filter(x => !mdp.terminal_states.contains(x)).map(
  			(_,mdp.actions(0))).toMap
  	
  	//updates:traverse for each states for each action,and choose the best action for each state
  	def policy_improve(mdp:Mdp){
		for(state <- mdp.states){
			if(!mdp.terminal_states.contains(state)){
				//initial a value
				var a1 = mdp.actions(0)
				var trans = mdp.transform(state, a1)
				var v1 = trans.r + mdp.gamma * this.value(trans.next_state)
				
				// select the action which performs max 
				for(action<-mdp.actions){
					trans = mdp.transform(state, action)
					if(v1 < (trans.r + mdp.gamma * this.value(trans.next_state))){
						a1 = action
						v1 = trans.r + mdp.gamma*this.value(trans.next_state)
					}
				}
				this.pi = this.pi.updated(state, a1)
			}
		}
	}
	
  	/**
  	 * evaluate the policy,and update the state values
  	 */
	def policy_evaluate(mdp:Mdp){
		val loop = new Breaks
    	loop.breakable {
			for(i <- 0 until 100){
				var delta = 0f
				for(state <- mdp.states){
					if(!mdp.terminal_states.contains(state)){
						val action = this.pi(state)
						val nextstate = mdp.transform(state, action)
						val new_v = nextstate.r + mdp.gamma * this.value(nextstate.next_state)
						delta += math.abs(this.value(state)-new_v)
						this.value(state) = new_v
					}
				}

				if(delta < 1e-6)
					loop.break()
			
			}
		}
	}
	
	def policy_iterate(mdp:Mdp){
		for(i <- 0 until 100){
			
			this.policy_evaluate(mdp)
			this.policy_improve(mdp)
		}
	}
	
}

object Policy_Value{
	def main(args:Array[String]){
		test()
	}
	
	def test(){
		val mdp = new Mdp()
		val pv = new Policy_Value(mdp)
		pv.policy_iterate(mdp)
		
		println("Value:")
		pv.value.zipWithIndex.foreach(a => println(s"state:${a._2} \t value:${a._1}"))
		println("Policy:")
		pv.pi.zipWithIndex.foreach(a => println(s"state:${a._2} \t policy:${a._1}"))
		
	}
	
	def test_pi(){
		val mdp = new Mdp()
		val pv = new Policy_Value(mdp)
		pv.pi = pv.pi.updated(3, 's')
		println(pv.pi)
	}
}