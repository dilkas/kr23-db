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

  final case class EndSearchException(private val message: String = "",
                                      private val cause: Throwable =
                                        None.orNull)
      extends Exception(message, cause)

  // NOTE: We assume that CNF->NNFNode updates are accepted by NNFNodes in the
  // same order as the corresponding CNFs are found in the Queue.
  type PartialCircuit = (AbstractCompiler, Option[NNFNode], List[CNF])

  var circuits: List[NNFNode] = List[NNFNode]()

  def foundSolution(circuit: NNFNode): Unit = {
    circuits = circuit :: circuits
    if (circuits.size >= 1) throw new EndSearchException
  }

  def applyToList(compiler: AbstractCompiler, circuit: NNFNode,
                  formulas: List[CNF]): List[CNF] = {
    formulas.map { applySinkRules(_, compiler) }.flatMap {
      case (_, node, successors) => {
        if (node.isDefined) {
          println("applyToList: adding " + node.get.getClass.getSimpleName +
                    " below " + circuit.getClass.getSimpleName)
          require(circuit.updateFirst(node.get))
        }
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
              // println("applySinkRules is returning a partial circuit with " +
              //           remainingSuccessors.size + " successors")
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
                 compiler: AbstractCompiler): Stream[PartialCircuit] = {
    Compiler.checkCnfInput(cnf)
    compiler.nonSinkRules.toStream.flatMap {
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
              // println("applyRules is returning a partial circuit with " +
              //           successors.size + " successors")
              Some(applySinkRules(newCompiler, Some(node2), successors))
            }
          }
        }
      }
    }
  }

  /** Run applyRules on the first loose end in the partial circuit,
    generating new states in the search tree. */
  def nextCircuits(partialCircuit: PartialCircuit): Queue[PartialCircuit] =
    partialCircuit match {
      case (_, circuit, Nil) => {
        println("nextCircuits: found a solution in the queue")
        foundSolution(circuit.get)
        Queue.empty
      }
      case (compiler, circuit, successors) => {
        val newPartialCircuits = applyRules(successors.head, compiler).flatMap {
          case (newCompiler, newNode, newSuccessors) => {
            val newNewSuccessors = newSuccessors ++ successors.tail

            println("nextCircuits: after applying " + newNode.get.explanation +
                      ", the number of uncompiled theories changed from " +
                      successors.size + " to " + newNewSuccessors.size)
            if (!newNewSuccessors.isEmpty) {
              println("The first uncompiled theory is:")
              println(newNewSuccessors.head)
            }

            val newNewNode = circuit match {
              case Some(node) => {
                println("nextCircuits: adding " +
                          newNode.get.getClass.getSimpleName + " below " +
                          node.getClass.getSimpleName)
                NNFNode.updateCache.clear
                val newCircuit = node.myClone
                require(newCircuit.addNode(newNode.get))
                newCircuit
              }
              case None => newNode.get
            }
            if (newNewSuccessors.isEmpty) {
              foundSolution(newNewNode)
              None
            } else {
              Some((newCompiler, Some(newNewNode), newNewSuccessors))
            }
          }
        }
        Queue(newPartialCircuits: _*)
      }
    }

  def breadthFirstTraverse(q: Queue[PartialCircuit]): Unit = if (!q.isEmpty) {
    println("Breadth first search is running on a queue of size " + q.size)
    val (partialCircuit, tail) = q.dequeue
    breadthFirstTraverse(tail ++ nextCircuits(partialCircuit))
  }

  def compilerBuilder = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  /** A simple BFS mainly for testing purposes */
  override def compile(cnf: CNF): NNFNode = {
    val initialQueue = Queue(applySinkRules(cnf, compilerBuilder))
    try {
      breadthFirstTraverse(initialQueue)
    } catch {
      case e: EndSearchException => {}
    }
    circuits.foreach { _.showPDF(null, null) }
    circuits.head
  }

}
