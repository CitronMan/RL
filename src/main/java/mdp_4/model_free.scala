package mdp_4
import scala.io.Source
import scala.util.control.Breaks
import util.Util
object model_free {
	val mdp      = new Mdp()
	val states   = mdp.states
	val actions  = mdp.actions
	val gamma    = mdp.gamma
	val source   = Source.fromFile("./data/best_qfunc")
	val iterator = source.getLines()
	val best     = iterator.map(line => {
									val elems = line.split(":")
									(elems(0),elems(1).toFloat)
								}).toMap

	def epsilon_greedy(qfunc: Map[String,Float],state:Int,epsilon:Float) :Char = {
		var amax = 0 //store the index of the right action
		var key  = s"${state}_${actions(0)}"
		var qmax = qfunc(key)
		actions.indices.foreach{i => {
			key   = s"${state}_${actions(i)}"
			val q = qfunc(key)
			if(qmax<q){
				qmax = q
				amax = i
			}
		}}
		
		//probability
		var pro = Array.fill[Float](actions.length)(0f)
		pro(amax) += 1-epsilon
		actions.indices.foreach { i => {
			pro(i) += epsilon / actions.length
		} }
		
		//choose
		var r = scala.util.Random.nextFloat()
		var s = 0f
		var res = actions(actions.length-1)
		val loop = new Breaks
		loop.breakable{
			actions.indices.foreach { x => {
				s += pro(x)
				if(s>r){
					res = actions(x)
					loop.break()
				}
			}}
		}
		res
	}
	
	//update the q_value after process of policy 
	def mc(num_iter:Int,epsilon:Float){
		var qfunc = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0f)
			})
		}).flatten.toMap
		var n = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0.001f)
			})		
		}).flatten.toMap
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
			y = y :+ compute_error(qfunc)
			//simulate one policy
			var s_tmp = Array[Int]()
		 	var a_tmp = Array[Char]()
		 	var r_tmp = Array[Float]()	 	  
			var s = states(scala.util.Random.nextInt(states.length)) 
			var t = false
			var count = 0
			while(false == t && count < 100){
				var a  = epsilon_greedy(qfunc, s, epsilon)
				var ns = mdp.transform(s, a)
				s_tmp  = s_tmp :+ s
		 	 	r_tmp  = r_tmp :+ ns.r
		 	 	a_tmp  = a_tmp :+ a
		 	 	s 	   = ns.next_state
		 	 	t      = ns.is_terminal 
		 	 	count += 1
			}
		 	var G = 0f
		 	for(step <- (0 until s_tmp.length).reverse){
					G = G * gamma
					G += r_tmp(step)
			}
			for(step <- 0 until s_tmp.length){
				var key = s"${s_tmp(step)}_${a_tmp(step)}"
//				println(key)
				n       = n.updated(key, n(key)+1f)
				qfunc   = qfunc.updated(key,(qfunc(key)*(n(key)-1f)+ G)/n(key))
				G  	   -= r_tmp(step)
				G      /= gamma
				}
		
		}
		Util.Util_plot(x,y)
	}
	
	def sarsa(num_iter:Int, alpha:Float, epsilon:Float){
		var qfunc = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0f)
			})
		}).flatten.toMap
		var n = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0.001f)
			})		
		}).flatten.toMap
		var x = Array[Int]()
		var y = Array[Float]()
		
		for(iter <- 0 to num_iter){
			//plot info
			x = x :+ iter	
			y = y :+ compute_error(qfunc)
			
			//initial state
			var s     = states(scala.util.Random.nextInt(states.length)) 
			var a     = actions(scala.util.Random.nextInt(actions.length))
			var t     = false
			var count = 0
			
			//simulate policy, and update the q_value step by step
			while(false == t && count < 100){
				var key = s"${s}_$a"
				var ns  = mdp.transform(s, a)
				s       = ns.next_state
				var a1  = epsilon_greedy(qfunc, s, epsilon) //next actions in policy
				var ke  = s"${s}_$a1"    
				qfunc   = qfunc.updated(key,qfunc(key) + alpha * (ns.r + 
						gamma * qfunc(ke)-qfunc(key)))
		 	 	t       = ns.is_terminal 
		 	 	a       = a1
		 	 	count  += 1
			}
		 }
		Util.Util_plot(x,y)
	}
	
	def qlearning(num_iter:Int, alpha:Float, epsilon:Float){
		var qfunc = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0f)
			})
		}).flatten.toMap
		var n = this.states.map(s => {
			this.actions.map(a =>{
				(s"${s}_$a",0.001f)
			})		
		}).flatten.toMap
		var x = Array[Int]()
		var y = Array[Float]()
		
		for(iter <- 0 to num_iter){
			//plot info
			x = x :+ iter	
			y = y :+ compute_error(qfunc)
			
			//initial state
			var s     = states(scala.util.Random.nextInt(states.length)) 
			var a     = actions(scala.util.Random.nextInt(actions.length))
			var t     = false
			var count = 0
			
			//simulate policy, and update the q_value step by step
			while(false == t && count < 100){
				var key = s"${s}_$a"
				var ns  = mdp.transform(s,a)
				val s1 = ns.next_state
				
				var key1 = ""
				var qmax = -1.0f
				for(a1 <- actions){
					if(qmax < qfunc(s"${s1}_$a1")){
						qmax = qfunc(s"${s1}_$a1")
						key1 = s"${s1}_$a1"
					}
					
				}
				qfunc   = qfunc.updated(key,qfunc(key) + alpha * (ns.r + 
						gamma * qfunc(key1)-qfunc(key)))
				s      = s1
				a      = epsilon_greedy(qfunc, s1, epsilon) //next actions in policy
		 	 	count += 1
			}
		 }
		Util.Util_plot(x,y)
	}
	
	
	def compute_error(qfunc: Map[String,Float]) :Float = {
		var sum = 0f
		qfunc.keys.foreach { key => {
			val error = (qfunc(key) - best(key))
			sum      += error * error
		}}
		sum
	}
	
	
	
	def main(args:Array[String]){
//		val sample = mdp.gen_random_pi_sample(1000)
		
		mc(1000,0.1f)
		sarsa(1000,0.2f,0.1f)
		qlearning(1000,0.2f,0.1f)
//		print("mc:"+mc(0.5f,sample))
	}
	
}