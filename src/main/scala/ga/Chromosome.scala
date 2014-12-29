package ga

import ga.Types.GeneGenerator

import scala.util.Random

object Chromosome {
  /**
   * Factory method to create new random chromosome.
   *
   * @param length length of the genes list
   * @param geneGenerator random gene generator
   * @tparam T type of the information stored in the chromosome (genes)
   * @return new chromosome
   */
  def random[T](length:Int)(implicit geneGenerator:GeneGenerator[T]):Chromosome[T] = {
    Chromosome[T](Vector.fill(length) { geneGenerator() })
  }
}

/**
 * Chromosome holds genetic information (genes) and is able to perform cross-ver
 * and mutation operation to create new specimen for the next generation.
 *
 * @param genes genes information for the chromosome
 * @param geneGenerator random gene generator
 * @tparam T type of the information stored in the chromosome (genes)
 */
case class Chromosome[T](genes:Vector[T])(implicit geneGenerator:GeneGenerator[T]) {
  val length = genes.length

  /**
   * Perform mutation operation.
   * @return new chromosome after the mutation.
   */
  def mutate:Chromosome[T] = {
    val pivot = Random.nextInt(length)

    Chromosome[T](
      genes.take(pivot) ++
        Vector(geneGenerator()) ++
        genes.drop(pivot + 1)
    )
  }

  /**
   * Perform cross over operation with other chromosome.
   *
   * @param other the other chromosome
   * @return list of 2 children chromosomes
   */
  def crossover(other:Chromosome[T]):List[Chromosome[T]] = {
    val pivot = Random.nextInt(length)

    List(
      Chromosome[T](genes.take(pivot) ++ other.genes.drop(pivot)),
      Chromosome[T](genes.drop(pivot) ++ other.genes.take(pivot))
    )
  }
}
