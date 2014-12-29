package ga

import Types._

import scala.util.Random

case class Chromosomes[T](list:Vector[Chromosome[T]])(implicit fitness:FitnessFunction[T]) {
  lazy val length = list.length

  def random = {
    list(Random.nextInt(length))
  }

  def best = {
    selection(1).take(1)(0)
  }

  def selection(count:Int) = {
    new Chromosomes(list.sortWith { fitness(_) > fitness(_) }.take(count))
  }

  def totalFitness = {
    list.map(fitness(_)).foldLeft(0.0)(_ + _)
  }

  def take(count:Int) = {
    list.take(count)
  }

}
