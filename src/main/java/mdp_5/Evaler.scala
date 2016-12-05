package mdp_5
import scala.io.Source
class Evaler(mdp:Mdp) {
	val source   = Source.fromFile("./data/best_qfunc")
	val iterator = source.getLines()
    val best_map = iterator.map(line => {
									val elems = line.split(":")
									(elems(0),elems(1).toFloat)
								}).toMap
	def eval(policy:Policy):Float = {
		var sum = 0f
		for(key<-this.best_map.keysIterator){
			val key_state = key.split("_")
			val s         = key_state(0).toInt-1
			if(!this.mdp.terminal_states.contains(s)){
				val f = mdp.start(s)
				val a = key_state(1).toCharArray()(0)
				val error = policy.qfunc(f, a) - this.best_map(key)
				sum      += error * error
			}
		}
		sum
	}
}