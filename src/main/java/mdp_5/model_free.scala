package mdp_5
import scala.io.Source
import scala.util.control.Breaks
import util.Util
object model_free {

	def update(policy:Policy,f:Array[Int],a:Char,tvalue:Float,alpha:Float){
		val pvalue = policy.qfunc(f, a)
		val error  = pvalue - tvalue
		val fea    = policy.get_fea_vec(f, a)
		policy.theta = fea zip policy.theta map(x => (x._2 - x._1 * alpha * error))
	}
	
	//update the q_value after process of policy 
	def mc(mdp:Mdp,policy:Policy,evaler:Evaler,num_iter:Int,alpha:Float){
		
		val gamma  = mdp.gamma
		policy.theta.indices.foreach {i => policy.theta(i) = 0.1f}
		
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
			y = y :+ evaler.eval(policy)
		
			//simulate one policy
//			var s_tmp = Array[Int]()
		 	var a_tmp = Array[Char]()
		 	var r_tmp = Array[Float]()	 	  
		 	var f_tmp = Array[Array[Int]]()
		 	
		 	var f = mdp.start()
		 	//var s = states(scala.util.Random.nextInt(states.length)) 
			var t = false
			var count = 0
			while(false == t && count < 100){
				var a  = policy.epsilon_greedy(f) 
//				s_tmp  = s_tmp :+ mdp.current
				var ns = mdp.receive(a)
				f_tmp  = f_tmp :+ f
		 	 	r_tmp  = r_tmp :+ ns.r
		 	 	a_tmp  = a_tmp :+ a
		 	 	t      = ns.is_terminal //replace
		 	 	f      = ns.next_state//replace
		 	 	count += 1
//		 	 	println(a)
			}
		 	var G = 0f
		 	for(step <- (0 until f_tmp.length).reverse){
					G  = G * gamma
					G += r_tmp(step)
			}
			for(step <- 0 until f_tmp.length){
				update(policy,f_tmp(step),a_tmp(step),G,alpha)
//				policy.theta.foreach { x => print(x+"  ") }
//				println
				G  	   -= r_tmp(step)
				G      /= gamma
				}
		
		}
		Util.Util_plot(x,y)
	}
	
	def sarsa(mdp:Mdp,policy:Policy,evaler:Evaler,num_iter:Int,alpha:Float){
		val gamma  = mdp.gamma
		val actions_sarsa = mdp.actions
		policy.theta.indices.foreach {i => policy.theta(i) = 0.1f}
		
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
			y = y :+ evaler.eval(policy)
		
		 	var f = mdp.start()
		 	var a = actions_sarsa(scala.util.Random.nextInt(actions_sarsa.length))
			var t = false
			var count = 0

			while(false == t && count < 100){
				var ns = mdp.receive(a)
				val a1 = policy.epsilon_greedy(ns.next_state)
				update(policy, f, a, ns.r+gamma*policy.qfunc(ns.next_state,a1), alpha)
				f  = ns.next_state
				a  = a1
				count += 1
			}
		 	
		}
		Util.Util_plot(x,y)
	}
def qlearning(mdp:Mdp,policy:Policy,evaler:Evaler,num_iter:Int,alpha:Float){
		val gamma  = mdp.gamma
		policy.theta.indices.foreach {i => policy.theta(i) = 0.1f}
		
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
			y = y :+ evaler.eval(policy)
		
		 	var f = mdp.start()
		 	var a = mdp.actions(scala.util.Random.nextInt(mdp.actions.length))
			var t = false
			var count = 0

			while(false == t && count < 100){
				var ns = mdp.receive(a)
				var qmax = -1f
				mdp.actions.foreach(a1 =>{
					val pvalue = policy.qfunc(ns.next_state, a1)
					if(qmax < pvalue){
						qmax = pvalue
					}
				})
				update(policy,f,a, ns.r+gamma*qmax,alpha)
				f = ns.next_state
				a = policy.epsilon_greedy(ns.next_state)
				count += 1
			}
		 	
		}
		Util.Util_plot(x,y)
	}
	
	def main(args:Array[String]){
		val mdp = new Mdp_ID()
		val evaler = new Evaler(mdp)
		val policy = new Policy(mdp,0.5f)
//		mc(mdp,policy,evaler,10000,0.01f)
//		sarsa(mdp,policy,evaler,10000,0.01f)
		qlearning(mdp,policy,evaler,10000,0.01f)
		
		
		
//		sarsa(1000,0.2f,0.1f)
//		qlearning(1000,0.2f,0.1f)
//		print("mc:"+mc(0.5f,sample))
	}
	
}