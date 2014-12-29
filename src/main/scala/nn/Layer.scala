package nn

/**
 * Created by petr on 26/12/2014.
 */
case class Layer(neurons:Vector[Neuron]) {
  def run(input:Vector[Double]):Vector[Double] = {
    neurons.map(_.run(input))
  }

  override def toString = "L[" + neurons.map(_.toString) + "]"
}