package edu.ucla.cs.starai.forclift.compiler

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf._

object BreadthCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, true)

  // Two limits on the extensiveness of search
  val NumSolutions = 10
  val MaxDepth = 4

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
    circuits = circuit :: circuits
    println("FOUND " + circuits.size + " SOLUTION(S)")
    if (circuits.size >= BreadthCompiler.NumSolutions)
      throw new EndSearchException
  }

  def compilerBuilder = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  /** A simple BFS */
  override def compile(cnf: CNF): List[NNFNode] = {
    val compiler = compilerBuilder
    val q = Queue(compiler.applySinkRules(cnf))
    // val q = PriorityQueue(compiler.applySinkRules(cnf))(Ordering.by(_.priority))
    var depth = 0
    try {
      while (depth <= BreadthCompiler.MaxDepth) {
        val partialCircuit = q.dequeue
        if (partialCircuit.depth > depth) {
          depth = partialCircuit.depth
          println("depth: " + partialCircuit.depth)
        }
        if (depth <= BreadthCompiler.MaxDepth)
          q ++= partialCircuit.nextCircuits(this)
      }
    } catch {
      case e: EndSearchException => {}
    }
    // circuits.foreach { _.showPDF(DomainSizes.empty, PredicateWeights.empty) }
    circuits
  }

}
