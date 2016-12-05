package mdp_5
import scala.util.control.Breaks
class Policy(val mdp:Mdp,val epsilon:Float) {
  val actions = mdp.actions
  mdp.start()
  val nsf   = mdp.receive(actions(0))
  //as said in the blog for learning parameter W
  var theta = Array.fill[Float](nsf.next_state.length*actions.length)(0f) //16
  //need turns to matrix

  def get_fea_vec(fea:Array[Int],a:Char):Array[Float] = { //16
	  val f = Array.fill[Float](this.theta.length)(0f)
	  var idx = 0
	  this.actions.indices.foreach(i =>{
	 	  if(a == this.actions(i))
	 	 	  idx = i
	  })
	  fea.indices.foreach(i =>{
	 	  f(idx*fea.length+i) = fea(i)
	  })
	  f
  }

    //given state and action , return the q value
    def qfunc(fea:Array[Int],a:Char):Float = {
	    val f = this.get_fea_vec(fea, a)
//	    f foreach(x => print(x+ " ") )
//	    println
	    var sum = 0f
	    f zip theta map(a =>{
	 	    sum += a._1 * a._2
	    })
	    sum
    }


	def epsilon_greedy(fea:Array[Int]) :Char = {
	    val eps = this.epsilon
	    var amax = 0 //store the index of the right action
	  	var qmax = qfunc(fea,this.actions(0))
		this.actions.indices.foreach{i => {
			val a = this.actions(i)
			val q = this.qfunc(fea,a)
			if(qmax < q){
				qmax = q
				amax = i
			}
		}}
		
		//probability
		var pro = Array.fill[Float](this.actions.length)(0f)
		pro(amax) += 1-eps
		this.actions.indices.foreach { i => {
			pro(i) += eps / this.actions.length
		} }
		
		//choose
		var r = scala.util.Random.nextFloat()
		var s = 0f
		var res = this.actions(actions.length-1)
		val loop = new Breaks
		loop.breakable{
			this.actions.indices.foreach { x => {
				s += pro(x)
				if(s >= r){
					res = this.actions(x)
					loop.break()
				}
			}}
		}
		res
	}


}