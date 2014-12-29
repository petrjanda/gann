package ga

object Types {
  /*
    Fitness function to measure the quality of the chromosome.
   */
  type FitnessFunction[T] = Chromosome[T] => Double

  /*
    Random gene generator.
   */
  type GeneGenerator[T] = () => T
}
