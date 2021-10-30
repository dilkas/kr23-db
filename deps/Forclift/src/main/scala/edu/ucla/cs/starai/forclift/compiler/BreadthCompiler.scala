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
    // println("applySinkRules started for theory:")
    // println(cnf)
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
          val nnf = node.get
          compiler.updateCache(cnf, nnf)
          val recursiveResults = successors.map { applySinkRules(_, compiler) }
          val remainingSuccessors = (successors zip recursiveResults).flatMap {
            case (theory, result) => result match {
              case None => Some(theory)
              case Some(Left(newTheory)) => Some(newTheory)
              case Some(Right(partialCircuit)) => {
                nnf.updateFirst(partialCircuit._2)
                partialCircuit._3
              }
            }
          }
          println("applySinkRules is returning a partial circuit with " +
                    remainingSuccessors.size + " successors")
          return Some(Right((compiler, nnf, remainingSuccessors)))
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
    // println("applyRules started for theory:")
    // println(cnf)
    Compiler.checkCnfInput(cnf)
    applySinkRules(cnf, compiler) match {
      case Some(Left(cnf2)) => applyRules(cnf2, compiler)
      case Some(Right(partialCircuit)) => Queue(partialCircuit)
      case None => {
        val answer = compiler.nonSinkRules.flatMap {
          _(cnf) match {
            case None => None // the rule is not applicable
            case Some((node: Option[NNFNode], successors: List[CNF])) => {
              node match {
                case None => {
                  // rerun on the updated theory
                  require(successors.size == 1)
                  // println("The theory was modified without adding a circuit node")
                  applyRules(successors.head, compiler)
                }
                case Some(node2) => {
                  val newCompiler = compiler.myClone
                  newCompiler.updateCache(cnf, node2)
                  println("applyRules is returning a partial circuit with " +
                            successors.size + " successors")
                  Some((newCompiler, node2, successors))
                }
              }
            }
          }
        }
        // println("applyRules: returning a queue of size " + answer.size)
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
        if (!newSuccessors.isEmpty) {
          println("The first uncompiled theory is:")
          println(newSuccessors.head)
        }
        (newCompiler, partialCircuit._2.addNode(newNode)._1, newSuccessors)
      }
    }
    //println("nextCircuits finished")
    Queue(answer: _*)
  }

  // TODO (Paulius): it would be better to identify the solution earlier
  /** (Lazily) produces a stream of circuits that compute the WFOMC of the
    given theory */
  def breadthFirstTraverse(q: Queue[PartialCircuit]): Stream[NNFNode] =
    if (q.isEmpty) {
      Stream.Empty
    } else {
      println("Breadth first search is running on a queue of size " + q.size)
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
