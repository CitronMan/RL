package mdp_1
/**
 * author:liuxianggen
 * taking the action randomly,and update the state value each samples
 */
object Mdp_value {
  def random_pi :Char = {
	  val actions = Array('n','e','s','w')
	  val r = scala.util.Random.nextInt(actions.length)
	  actions(r)
  }
  
  def compute_random_pi_state_value{
	  val value = Array.fill[Float](9)(0f)
	  val n_iter = 1000000
	  for(k<- 1 until n_iter){
	 	  for(i<- 1 until 6){
	 	 	  val mdp = new Mdp()
	 	 	  var state = i
	 	 	  var is_terminal = false
	 	 	  var gamma = 1f
	 	 	  var state_value = 0f
	 	 	  while(false == is_terminal){
	 	 	 	  val action = random_pi
	 	 	 	  val trans = mdp.transform(state, action)//is_terminal,state, reward
	 	 	 	  state_value += gamma * trans._3  
	 	 	 	  is_terminal = trans._1
	 	 	 	  state = trans._2
	 	 	 	  gamma *= 0.5f
	 	 	  }
	 	 	  value(i) = (value(i)*(k-1) + state_value)/k
	 	  }
	 	  if(k%10000 == 0){
	 	 	  value.foreach(x => print(x + "  "))
	 	 	  println
	 	  }
	  }
		value.foreach(x => print(x + "  "))
  }
  
  def main(args:Array[String]){
	  compute_random_pi_state_value
  }
}