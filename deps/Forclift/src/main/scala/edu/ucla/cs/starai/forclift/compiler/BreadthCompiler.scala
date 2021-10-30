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
  type PartialCircuit = (AbstractCompiler, Option[NNFNode], List[CNF])

  def compilerBuilder = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  def applyToList(compiler: AbstractCompiler, circuit: NNFNode,
                  formulas: List[CNF]): List[CNF] = {
    formulas.map { applySinkRules(_, compiler) }.flatMap {
      case (_, node, successors) => {
        if (node.isDefined) circuit.updateFirst(node.get)
        successors
      }
    }
  }

  // The compiler stays the same because we're only applying a single rule,
  // i.e., the search tree doesn't branch out
  def applySinkRules(cnf: CNF, compiler: AbstractCompiler): PartialCircuit = {
    var rules = compiler.sinkRules
    while (rules.nonEmpty) {
      rules.head(cnf) match {
        case None => rules = rules.tail
        case Some((node, successors)) => {
          node match {
            case None => {
              require(successors.size == 1)
              return applySinkRules(successors.head, compiler)
            }
            case Some(nnf) => {
              compiler.updateCache(cnf, nnf)
              val remainingSuccessors = applyToList(compiler, nnf, successors)
              println("applySinkRules is returning a partial circuit with " +
                        remainingSuccessors.size + " successors")
              return (compiler, Some(nnf), remainingSuccessors)
            }
          }
        }
      }
    }
    (compiler, None, List(cnf))
  }

  /** Run the applySinkRules function on all of the successors (i.e.,
    uncompiled formulas) of the given partial circuit, incorporate all results
    into the main partial circuit, and return it. */
  def applySinkRules(partialCircuit: PartialCircuit): PartialCircuit = {
    val (compiler, node, successors) = partialCircuit
    require(node.isDefined)
    val newSuccessors = applyToList(compiler, node.get, successors)
    (compiler, node, newSuccessors)
  }

  /** Apply all rules to the formula. The NNFNodes are just placeholders
    whenever this function is called by nextCircuits. A new copy of the
    compiler cache is instantiated for each element of the queue. */
  def applyRules(cnf: CNF,
                 compiler: AbstractCompiler): Queue[PartialCircuit] = {
    Compiler.checkCnfInput(cnf)
    val answer = compiler.nonSinkRules.flatMap {
      _(cnf) match {
        case None => None // the rule is not applicable
        case Some((node: Option[NNFNode], successors: List[CNF])) => {
          node match {
            case None => {
              // rerun on the updated theory
              require(successors.size == 1)
              applyRules(successors.head, compiler)
            }
            case Some(node2) => {
              val newCompiler = compiler.myClone
              newCompiler.updateCache(cnf, node2)
              println("applyRules is returning a partial circuit with " +
                        successors.size + " successors")
              Some(applySinkRules(newCompiler, Some(node2), successors))
            }
          }
        }
      }
    }
    Queue(answer: _*)
  }

  /** Run applyRules on the first loose end in the partial circuit,
    generating new states in the search tree. */
  def nextCircuits(partialCircuit: PartialCircuit): Queue[PartialCircuit] = {
    require(!partialCircuit._3.isEmpty)
    //println("nextCircuits started")
    val answer = applyRules(partialCircuit._3.head, partialCircuit._1).map {
      case (newCompiler, newNode, successors) => {
        NNFNode.updateCache.clear()
        val newSuccessors = successors ++ partialCircuit._3.tail
        println("The number of uncompiled theories changed from " +
                  partialCircuit._3.size + " to " + newSuccessors.size)
        if (!newSuccessors.isEmpty) {
          println("The first uncompiled theory is:")
          println(newSuccessors.head)
        }
        val newNewNode = partialCircuit._2 match {
          case Some(node) => node.addNode(newNode.get)._1
          case None => newNode.get
        }
        (newCompiler, Some(newNewNode), newSuccessors)
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
      partialCircuit match {
        case (_, Some(node), Nil) => {
          println("Found a circuit!")
          node #:: breadthFirstTraverse(tail ++ nextCircuits(partialCircuit))
        }
        case partialCircuit =>
          breadthFirstTraverse(tail ++ nextCircuits(partialCircuit))
      }
    }

  /** A simple BFS mainly for testing purposes */
  override def compile(cnf: CNF): NNFNode = {
    val maxSolutionCount = 1
    val initialQueue = Queue(applySinkRules(cnf, compilerBuilder))
    val circuits = breadthFirstTraverse(initialQueue).take(maxSolutionCount)
    circuits.foreach { _.showPDF(null, null) }
    circuits.head
  }

}
