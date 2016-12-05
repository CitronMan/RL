package mdp_6
import scala.util.control.Breaks
import util.Util
object model_free {

	def update_valuepolicy(valuepolicy:ValuePolicy,f:Array[Int],a:Char,tvalue:Float,alpha:Float){
		val pvalue = valuepolicy.qfunc(f, a)
		val error  = pvalue - tvalue
		val fea    = valuepolicy.get_fea_vec(f, a)
		valuepolicy.theta = fea zip valuepolicy.theta map(x => (x._2 - x._1 * alpha * error))
	}
	
	def update_softmaxpolicy(softmaxpolicy:SofrmaxPolicy,f:Array[Int],a:Char,qvalue:Float,alpha:Float){
		val fea = softmaxpolicy.get_fea_vec(f, a)
		val prob = softmaxpolicy.pi(f)
		var delta_logJ = fea
		softmaxpolicy.actions.indices.foreach { x => {
			val a1 = softmaxpolicy.actions(x)
			val fea1 = softmaxpolicy.get_fea_vec(f, a1)
			delta_logJ = delta_logJ zip fea1.map(_ * prob(x)) map(y => y._2 - y._1)  // delta_logJ -= fea1 * prob[i];
		}}
		
		softmaxpolicy.theta = softmaxpolicy.theta zip delta_logJ map(x =>{
			x._1 - x._2 * alpha * qvalue
		})
	}
	
	//update the q_value after the whole process of policy
	def mc(mdp:Mdp,softmaxpolicy:SofrmaxPolicy,evaler:Evaler,num_iter:Int,alpha:Float){
		
		val gamma  = mdp.gamma
		softmaxpolicy.theta.indices.foreach {i => softmaxpolicy.theta(i) = 0.1f}
		
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
//			y = y :+ evaler.eval(softmaxpolicy)
		
			//simulate one policy
//			var s_tmp = Array[Int]()
		 	var a_tmp = Array[Char]()
		 	var r_tmp = Array[Float]()	 	  
		 	var f_tmp = Array[Array[Int]]()
		 	var f = mdp.start()
			var t = false
			var count = 0
			while(false == t && count < 100){
				var a  = softmaxpolicy.take_action(f) 
				var ns = mdp.receive(a)
				f_tmp  = f_tmp :+ f
		 	 	r_tmp  = r_tmp :+ ns.r
		 	 	a_tmp  = a_tmp :+ a

		 	 	t      = ns.is_terminal //replace
		 	 	f      = ns.next_state//replace
		 	 	count += 1
			}
		 	var G = 0f
		 	for(step <- (0 until f_tmp.length).reverse){
					G  = G * gamma
					G += r_tmp(step)
			}
			for(step <- 0 until f_tmp.length){
				update_softmaxpolicy(softmaxpolicy,f_tmp(step),a_tmp(step),G,alpha)
//				policy.theta.foreach { x => print(x+"  ") }
//				println
				G  	   -= r_tmp(step)
				G      /= gamma
			
			
			}
			
			
			
		
		}
		Util.Util_plot(x,y)
	}
	
	def sarsa(mdp:Mdp,softmaxpolicy:SofrmaxPolicy,valuepolicy:ValuePolicy,evaler:Evaler,num_iter:Int,alpha:Float){
		val gamma  = mdp.gamma
		val actions_sarsa = mdp.actions
		softmaxpolicy.theta.indices.foreach {i => softmaxpolicy.theta(i) = 0.1f}
		valuepolicy.theta.indices.foreach {i => valuepolicy.theta(i) = 0.1f}
		
		var x = Array[Int]()
		var y = Array[Float]()
		for(iter <- 0 to num_iter){
			x = x :+ iter	
			y = y :+ evaler.eval(valuepolicy)
		
		 	var f = mdp.start()
		 	var a = actions_sarsa(scala.util.Random.nextInt(actions_sarsa.length))
			var t = false
			var count = 0

			while(false == t && count < 100){
				var ns = mdp.receive(a)
				val a1 = softmaxpolicy.take_action(ns.next_state)
				update_valuepolicy(valuepolicy, f, a, ns.r+gamma*valuepolicy.qfunc(ns.next_state,a1), alpha)
				update_softmaxpolicy(softmaxpolicy, f, a, ns.r + valuepolicy.qfunc(f,a), alpha)
				f  = ns.next_state
				a  = a1
				count += 1
			}
		 	
		}
		Util.Util_plot(x,y)
	}
//def qlearning(mdp:Mdp,policy:Policy,evaler:Evaler,num_iter:Int,alpha:Float){
//		val gamma  = mdp.gamma
//		policy.theta.indices.foreach {i => policy.theta(i) = 0.1f}
//		
//		var x = Array[Int]()
//		var y = Array[Float]()
//		for(iter <- 0 to num_iter){
//			x = x :+ iter	
//			y = y :+ evaler.eval(policy)
//		
//		 	var f = mdp.start()
//		 	var a = mdp.actions(scala.util.Random.nextInt(mdp.actions.length))
//			var t = false
//			var count = 0
//
//			while(false == t && count < 100){
//				var ns = mdp.receive(a)
//				var qmax = -1f
//				mdp.actions.foreach(a1 =>{
//					val pvalue = policy.qfunc(ns.next_state, a1)
//					if(qmax < pvalue){
//						qmax = pvalue
//					}
//				})
//				update(policy,f,a, ns.r+gamma*qmax,alpha)
//				f = ns.next_state
//				a = policy.epsilon_greedy(ns.next_state)
//				count += 1
//			}
//		 	
//		}
//		Util.Util_plot(x,y)
//	}
	
	def main(args:Array[String]){
		val mdp = new Mdp_ID()
		val evaler = new Evaler(mdp)
		val softmaxpolicy = new SofrmaxPolicy(mdp,0.5f)
		val valuepolicy = new ValuePolicy(mdp,0.5f)
		
		sarsa(mdp,softmaxpolicy,valuepolicy,evaler,100,0.01f)
//		sarsa(mdp,policy,evaler,10000,0.01f)
//		qlearning(mdp,policy,evaler,10000,0.01f)
		
		
		
//		sarsa(1000,0.2f,0.1f)
//		qlearning(1000,0.2f,0.1f)
//		print("mc:"+mc(0.5f,sample))
	}
	
}