package ga

import scala.collection.mutable.ListBuffer
import scala.util.Random
import Types._

/**
 * Created by petr on 26/12/2014.
 */
object Generation {
  def random[T](generationSize:Int, genesCount:Int)(implicit fitness:FitnessFunction[T], geneGenerator:GeneGenerator[T]):Generation[T] = {
    val chromosomes = (1 to generationSize).toVector.map(i =>
      Chromosome.random(genesCount)
    )

    Generation(Chromosomes(chromosomes))
  }
}

case class Generation[T](chromosomes:Chromosomes[T])(implicit fitness:FitnessFunction[T]) {
  val length = chromosomes.length

  def totalFitness:Double = {
    chromosomes.totalFitness
  }

  def next(learningRate:Double, mutationRate:Double):Generation[T] = {
    val list:ListBuffer[Chromosome[T]] = new ListBuffer[Chromosome[T]]()

    val childrenCount:Int = (length * learningRate).toInt
    val top = chromosomes.selection(childrenCount)

    (0 to childrenCount / 2).foreach { _ =>
      val mother = top.random
      val father = top.random

      list ++= mother.crossover(father)
    }

    list ++= chromosomes.take(length - list.length)

    // Mutate
    val mutateCount:Int = (length * mutationRate).toInt

    (0 to mutateCount).foreach { _ =>
      val index = Random.nextInt(list.length)

      list(index) = list(index).mutate
    }

    println("children: " + childrenCount)
    println("mutants: " + mutateCount)
    println("survives: " + (length - childrenCount))

    Generation[T](Chromosomes(list.toVector))
  }

  def randomChromosome = {
    chromosomes.random
  }

  def bestChromosome = {
    chromosomes.best
  }
}

