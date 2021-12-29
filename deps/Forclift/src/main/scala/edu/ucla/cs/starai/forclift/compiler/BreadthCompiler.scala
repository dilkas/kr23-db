package edu.ucla.cs.starai.forclift.compiler

import scala.collection.immutable.Queue

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._

object BreadthCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, true)

  val numSolutions = 3

}

class BreadthCompiler(sizeHint: Compiler.SizeHints =
                        Compiler.SizeHints.unknown(_),
                      grounding: Boolean = false) extends Compiler {

  final case class EndSearchException(private val message: String = "",
                                      private val cause: Throwable =
                                        None.orNull)
      extends Exception(message, cause)

  var circuits: List[NNFNode] = List[NNFNode]()

  override def foundSolution(circuit: NNFNode): Unit = {
    println("FOUND A SOLUTION")
    circuits = circuit :: circuits
    if (circuits.size >= BreadthCompiler.numSolutions)
      throw new EndSearchException
  }

  def breadthFirstTraverse(q: Queue[PartialCircuit]): Unit = if (!q.isEmpty) {
    println("Breadth first search is running on a queue of size " + q.size)
    val (partialCircuit, tail) = q.dequeue
    breadthFirstTraverse(tail ++ partialCircuit.nextCircuits(this))
  }

  def compilerBuilder = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  /** A simple BFS mainly for testing purposes */
  override def compile(cnf: CNF): List[NNFNode] = {
    val compiler = compilerBuilder
    val initialQueue = Queue(compiler.applySinkRules(cnf))
    try {
      breadthFirstTraverse(initialQueue)
    } catch {
      case e: EndSearchException => {}
    }
    // circuits.foreach { _.showPDF(DomainSizes.empty, PredicateWeights.empty) }
    circuits
  }

}
