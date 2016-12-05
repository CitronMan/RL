package mdp_3

object model_free_policy_eval {
	val mdp = new Mdp()
	val states = mdp.states
	val actions = mdp.actions
	val gamma = mdp.gamma
	
    def mc(gamma:Float,sample:(Array[Array[Int]],Array[Array[Char]],Array[Array[Float]])): Map[Int, Float] = {
		val (state_sample, action_sample, reward_sample) = sample
		var vfunc = states.map(x =>{
			(x,0f)
		}).toMap  
		var nfunc = states.map(x =>{
			(x,0f)
		}).toMap 
		for(iter <- 0 until state_sample.length){
			var G = 0f
			for(step <- (0 until state_sample(iter).length).reverse){
				G = G * gamma
				G += reward_sample(iter)(step)
			}
			for(step <- 0 until state_sample(iter).length){
				val s = state_sample(iter)(step)
				vfunc = vfunc.updated(s, vfunc(s)+G)
				nfunc = nfunc.updated(s, nfunc(s)+1f)
				G    -= reward_sample(iter)(step)
				G    /= gamma
			}
		}
		for(s <- states){
			if(nfunc(s) >1e-6)
				vfunc = vfunc.updated(s, vfunc(s)/nfunc(s))
		}
		
		vfunc
    }
	
	def td(alpha:Float,gamma:Float,sample:(Array[Array[Int]],Array[Array[Char]],Array[Array[Float]])): Map[Int, Float] = {
		val (state_sample, action_sample, reward_sample) = sample
		var vfunc = states.map(x =>{
			(x,scala.util.Random.nextFloat())
		}).toMap  
		
		for(iter <- 0 until state_sample.length){
			for(step <- 0 until state_sample(iter).length){
				val s = state_sample(iter)(step)
				val r = reward_sample(iter)(step)
				//V(S_t+1)
				var next_v = 0f
				if(step < state_sample(iter).length-1){
					next_v  = vfunc(state_sample(iter)(step+1))
				}
				
				vfunc = vfunc.updated(s, vfunc(s)+ alpha * (r + gamma * next_v - vfunc(s)))
			}
		}
		
		vfunc
	}
	
	
	
	def main(args:Array[String]){
		val sample = mdp.gen_random_pi_sample(1000)
		
//		print("mc:"+mc(0.5f,sample))
		print("td:"+td(0.15f,0.5f,sample))
	}
	
}