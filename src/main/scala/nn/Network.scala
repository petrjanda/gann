package nn

import scala.collection.mutable.ListBuffer

/**
 * Created by petr on 26/12/2014.
 */
case class Network(layers:Vector[Layer]) {
  def run(input:Vector[Double]) = {
    val iterator = layers.iterator
    var in = input

    while(iterator.hasNext) { in = iterator.next.run(in) }

    in
  }

  def weights:List[Double] = {
    layers.flatMap(layer => layer.neurons.flatMap(neuron => neuron.weights)).toList
  }

  override def toString = "N[" + layers.map("\r\n  " + _.toString) + "\r\n]"
}

object Network {
  def size(counts:List[Int]):Int = {
    counts(0) + counts.iterator.sliding(2).toList.map(item => item.foldLeft(1){ _ * _ }).sum
  }

  def apply(weights:List[List[List[Double]]]):Network = {
    Network(weights.map(layer =>
      Layer(layer.map(neuron =>
        Neuron(neuron.toVector)
      ).toVector)
    ).toVector)
  }

  def apply(counts:List[Int], weights:Vector[Double]):Network = {
    val i = counts.iterator
    var last = counts(0)

    var rest = weights

    def take(count:Int):List[Double] = {
      val weights = rest.take(count)

      rest = rest.drop(count)

      weights.toList
    }

    val buffer = ListBuffer[List[List[Double]]]()

    do {
      val current = i.next()

      buffer += (1 to current).toList.map(item => take(last))

      last = current
    } while(i.hasNext)

    Network(buffer.toList)
  }
}