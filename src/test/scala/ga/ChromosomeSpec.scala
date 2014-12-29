package ga

import org.scalatest.{ShouldMatchers, FlatSpec}

class ChromosomeSpec extends FlatSpec with ShouldMatchers {
  implicit val geneGenerator = () => 'x'

  "Chromosome#crossover" should "perform cross over operation" in {
    val a = new Chromosome[Char](Vector('a', 'b'))
    val b = new Chromosome[Char](Vector('c', 'd'))

    val children = a.crossover(b)

    children(0).genes should have length(2)
    children(0).genes.mkString should fullyMatch regex("""[abcd]*""")
  }

  "Chromosome#mutate" should "perform mutation operation" in {
    val a = new Chromosome[Char](Vector('a', 'b'))
    val b = a.mutate

    b.genes should have length(2)
    b.genes.mkString should fullyMatch regex("""[abx]*""")
  }

  "Chromosome.random" should "create new chromosome" in {
    val a = Chromosome.random[Char](3)

    // Contains 3 'x'es because of our simple generator
    a.genes should have length 3
    a.genes.mkString shouldEqual "xxx"
  }
}
