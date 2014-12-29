package com.example

import ga._
import Types._
import nn._

import scala.util.Random

object Hello {
  def main(args: Array[String]): Unit = {
    val trainingSamples = 100
    val trainingSet = (1 to trainingSamples).map(i => Vector(1.0 / i)).toList

    val topology = List(1, 3, 1)

    val fitness: FitnessFunction[Double] = (chromosome: Chromosome[Double]) => {
      val network = Network(topology, chromosome.genes)

      val total = trainingSet.map(_(0)).map(Math.sin(_)).sum

      1 / Math.abs(trainingSet.map(network.run(_)(0)).sum - total)
    }

    val geneGenerator:GeneGenerator[Double] = () => Random.nextDouble()

    var generation = Generation.random[Double](
      generationSize = 200,
      genesCount = Network.size(topology)
    )(fitness, geneGenerator)

    (1 to 5).foreach(i => {
      val network = Network(topology, generation.bestChromosome.genes)

      val samples = 10
      val totalError = (1 to samples).map(i => {
        Math.sin(1.0 / i) - network.run(Vector(1.0 / i))(0)
      }).sum

      println("Average error: " + totalError / samples)
      println(network)

      generation = generation.next(0.001, 0.001)
    })
  }
}

class Gann(generationSize:Int, layerCounts:List[Int], fitness:FitnessFunction[Double]) {
  def train(iterations:Int):Network = {
    val geneGenerator:GeneGenerator[Double] = () => Random.nextDouble()

    var generation = Generation.random[Double](
      generationSize = generationSize,
      genesCount = Network.size(layerCounts)
    )(fitness, geneGenerator)

    (1 to iterations).foreach(i =>
      generation = generation.next(0, 0)
    )

    Network(layerCounts, generation.chromosomes.list(0).genes)
  }
}
