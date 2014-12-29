package nn

/**
 * Created by petr on 26/12/2014.
 */
case class Neuron(weights:Vector[Double]) {
  lazy val length = weights.length

  def run(input:Vector[Double]):Double = {
    if(input.length != length) throw new RuntimeException("Invalid input length. Expected: " + length + ", Given: " + input.length + "!")

    if(input.zip(weights).map { Function.tupled(_ * _) }.sum > 0) 1.0 else 0.0
  }

  override def toString = "N[" + weights.map(w => (100.0 * w).round / 100.0).mkString(",") + "]"
}
