package edu.ucla.cs.starai.forclift.compiler

import scala.collection.immutable.Queue

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._

object BreadthCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new BreadthCompiler(sizeHint, true)

}

class BreadthCompiler(sizeHint: Compiler.SizeHints =
                        Compiler.SizeHints.unknown(_),
                      grounding: Boolean = false) extends Compiler {

  // NOTE: We assume that CNF->NNFNode updates are accepted by NNFNodes in the
  // same order as the corresponding CNFs are found in the Queue.
  type PartialCircuit = (AbstractCompiler, NNFNode, List[CNF])

  def compilerBuilder = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  // The compiler stays the same because we're only applying a single rule,
  // i.e., the search tree doesn't branch out
  def applySinkRules(cnf: CNF, compiler: AbstractCompiler)
      : Option[Either[CNF, PartialCircuit]] = {
    //println("applySinkRules started")
    var rules = compiler.sinkRules
    while (rules.nonEmpty) {
      //println("applySinkRules: about to apply a rule on " + cnf)
      val tryRule = rules.head(cnf)
      //println("applySinkRules: just applied a rule")
      if (tryRule.nonEmpty) {
        val (node, successors) = tryRule.get
        if (node.isEmpty) {
          require(successors.size == 1)
          val recursiveResult = applySinkRules(successors.head, compiler)
          return if (recursiveResult.isDefined) recursiveResult
                 else Some(Left(successors.head))
        } else {
          compiler.updateCache(cnf, node.get)
          return Some(Right((compiler, node.get, successors)))
        }
      } else {
        rules = rules.tail
      }
    }
    //println("applySinkRules finished")
    None
  }

  /** Apply all rules to the formula. The NNFNodes are just placeholders
    whenever this function is called by nextCircuits. A new copy of the
    compiler cache is instantiated for each element of the queue. */
  def applyRules(cnf: CNF,
                 compiler: AbstractCompiler): Queue[PartialCircuit] = {
    //println("applyRules started")
    Compiler.checkCnfInput(cnf)
    applySinkRules(cnf, compiler) match {
      case Some(Left(cnf2)) => applyRules(cnf2, compiler)
      case Some(Right(partialCircuit)) => Queue(partialCircuit)
      case None => {
        val answer = compiler.nonSinkRules.flatMap {
          println(".")
          _(cnf) match {
            case None => None // the rule is not applicable
            case Some((node: Option[NNFNode], successors: List[CNF])) => {
              node match {
                case None => {
                  // rerun on the updated theory
                  require(successors.size == 1)
                  println("The theory was modified without adding a circuit node")
                  applyRules(successors.head, compiler)
                }
                case Some(node2) => Some((compiler.myClone, node2, successors))
              }
            }
          }
        }
        println("applyRules: returning a queue of size " + answer.size)
        Queue(answer: _*)
      }
    }
  }

  /** Run applyRules on the first loose end in the partial circuit,
    generating new states in the search tree. */
  def nextCircuits(partialCircuit: PartialCircuit): Queue[PartialCircuit] = {
    require(!partialCircuit._3.isEmpty)
    //println("nextCircuits started")
    val answer = applyRules(partialCircuit._3.head, partialCircuit._1).map {
      case (newCompiler: AbstractCompiler, newNode: NNFNode,
            successors: List[CNF]) => {
        NNFNode.updateCache.clear()
        val newSuccessors = successors ++ partialCircuit._3.tail
        println("The number of uncompiled theories changed from " +
                  partialCircuit._3.size + " to " + newSuccessors.size)
        (newCompiler, partialCircuit._2.addNode(newNode)._1, newSuccessors)
      }
    }
    //println("nextCircuits finished")
    Queue(answer: _*)
  }

  /** (Lazily) produces a stream of circuits that compute the WFOMC of the
    given theory */
  def breadthFirstTraverse(q: Queue[PartialCircuit]): Stream[NNFNode] =
    if (q.isEmpty) {
      Stream.Empty
    } else {
      val (partialCircuit, tail) = q.dequeue
      if (partialCircuit._3.isEmpty) { // a complete circuit
        println("Found a circuit!")
        partialCircuit._2 #::
          breadthFirstTraverse(tail ++ nextCircuits(partialCircuit))
      }
      else {
        breadthFirstTraverse(tail ++ nextCircuits(partialCircuit))
      }
    }

  /** A simple BFS mainly for testing purposes */
  override def compile(cnf: CNF): NNFNode = {
    val maxSolutionCount = 1
    val circuits = breadthFirstTraverse(Queue.empty ++
                                          applyRules(cnf, compilerBuilder)).
      take(maxSolutionCount)
    circuits.foreach { _.showPDF(null, null) }
    circuits.head
  }

}
