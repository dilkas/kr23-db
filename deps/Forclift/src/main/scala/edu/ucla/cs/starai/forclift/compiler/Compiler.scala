/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.forclift.compiler

import collection._
import scala.collection.immutable.Queue

import edu.ucla.cs.starai.forclift.nnf.visitors.DomainsVisitor
import edu.ucla.cs.starai.forclift.nnf.visitors.PostOrderVisitor
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object Compiler {

  object Builder {
    val default: Builder = MyCompiler.builder
    val defaultWithGrounding: Builder = MyCompiler.builderWithGrounding
  }

  type Builder = ((Domain => Int) => Compiler)

  def default: Compiler = new MyCompiler() with LiftedCompiler

  object SizeHints {
    def unknown(d: Domain) = 1
  }

  type SizeHints = (Domain => Int)

  val greedy: Boolean = false
  var neverRun: Boolean = true

}

trait Compiler {

  def compile(cnf: CNF): NNFNode

  def compileSmooth(cnf: CNF): NNFNode = {
    compileSmooth(cnf, cnf.predicates, Set.empty)
  }

  def compileSmooth(cnf: CNF, predicates: Set[Predicate], excluded: Set[PositiveUnitClause] = Set.empty): NNFNode = {
    val nnf = compile(cnf)
    nnf.smoothWithPredicates(predicates, excluded)
  }

}

trait GroundingCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    new GroundingNode(cnf, "No rule fires.")
  }

}

trait LiftedCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    throw new IllegalArgumentException("Cannot compile " + cnf + " without grounding.")
  }

}

abstract class AbstractCompiler extends Compiler {

  // Partition CNFs w.r.t. hash codes
  val nnfCache = new mutable.HashMap[Int, List[(CNF, NNFNode)]]

  def updateCache(cnf: CNF, nnf: NNFNode) {
    assume(nnf != null)
    if (!nnfCache.contains(cnf.hashCode) || !nnfCache(cnf.hashCode).exists {
          case (theory, _) => theory == cnf } )
      nnfCache(cnf.hashCode) = (cnf, nnf) ::
        nnfCache.getOrElse(cnf.hashCode, List[(CNF, NNFNode)]())
  }

  def tryCache(cnf: CNF): InferenceResult = {
    if (!nnfCache.contains(cnf.hashCode)) {
      None
    } else {
      nnfCache(cnf.hashCode).flatMap { case (theory, circuit) =>
        CNF.identifyRecursion(cnf, theory) match {
          case Some(recursion) => Some((circuit, recursion))
          case None => None
        } }.headOption match {
        case Some(results) => {
          println("\nCache hit.")
          println("Before:")
          println(cnf)
          println("After:")
          println(results._1.cnf + "\n")
          val node = new Ref(cnf, Some(results._1), results._2, "Cache hit.")
          updateCache(cnf, node)
          Some((Some(node), List[CNF]()))
        }
        case None => None
      }
    }
  }

  // None means that the inference rule doesn't apply. (None, _) means that a
  // new node wasn't created but the theory was updated (we assume that in this
  // case the list has exactly one element). In all other cases, the list holds
  // theories that will be compiled into direct successors of the returned node.
  type InferenceResult = Option[(Option[NNFNode], List[CNF])]
  type InferenceRule = CNF => InferenceResult

  def sinkRules: List[InferenceRule]
  def nonSinkRules: List[InferenceRule]
  // NOTE: we assume that inferenceRules = sinkRules ++ nonSinkRules (possibly
  // in a different order)
  def inferenceRules: List[InferenceRule]

  var nbCompilationSteps = 0;

  def checkCnfInput(cnf: CNF) {
    require(!cnf.domains.contains(Universe), s"Cannot compile CNFs containing the universe domain: $cnf")
    require(!cnf.domains.contains(EmptyDomain), s"Cannot compile CNFs containing the empty domain: $cnf")
  }

  def greedySearch(cnf: CNF): NNFNode = {
    checkCnfInput(cnf)
    var rules = inferenceRules
    var nnf: NNFNode = null
    while (nnf == null && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) {
        val (node, successors) = tryRule.get
        if (node.isEmpty) {
          require(successors.size == 1)
          nnf = greedySearch(successors.head)
        } else {
          nnf = node.get
          // the important bit
          updateCache(cnf, nnf)
          nnf.update(successors.map(successor => Some(greedySearch(successor))))
        }
      }
      else rules = rules.tail
    }
    if (nnf == null) {
      nnf = cannotCompile(cnf)
    }
    nnf
  }

  // NOTE: We assume that CNF->NNFNode updates are accepted by NNFNodes in the
  // same order as the corresponding CNFs are found in the Queue.
  type PartialCircuit = (NNFNode, List[CNF])

  def applySinkRules(cnf: CNF): Option[PartialCircuit] = {
    var rules = sinkRules
    var nnf: NNFNode = null
    var outEdges: List[CNF] = List[CNF]()
    while (nnf == null && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) {
        val (node, successors) = tryRule.get
        nnf = node.get // this always exists
        outEdges = successors
        updateCache(cnf, nnf)
      } else {
        rules = rules.tail
      }
    }
    if (nnf == null) None else Some((nnf, outEdges))
  }

  /** Apply all rules to the formula. The NNFNodes are just placeholders
    whenever this function is called by nextCircuits. */
  def applyRules(cnf: CNF): Queue[PartialCircuit] = {
    checkCnfInput(cnf)
    val potentialAnswer = applySinkRules(cnf)
    if (potentialAnswer.isDefined) {
      Queue(potentialAnswer.get)
    } else {
      val answer = nonSinkRules.flatMap {
        println(".")
        _(cnf) match {
          case None => None // the rule is not applicable
          case Some((node: Option[NNFNode], successors: List[CNF])) => {
            node match {
              case None => {
                // rerun on the updated theory
                require(successors.size == 1)
                println("The theory was modified without adding a circuit node")
                applyRules(successors.head)
              }
              case Some(node2) => Some((node2, successors))
            }
          }
        }
      }
      println("applyRules: returning a queue of size " + answer.size)
      Queue(answer: _*)
    }
  }

  /** Apply applyRules to the first loose end in the partial circuit,
    generating new states in the search tree. */
  def nextCircuits(partialCircuit: PartialCircuit): Queue[PartialCircuit] = {
    require(!partialCircuit._2.isEmpty)
    val answer = applyRules(partialCircuit._2.head).map {
      case (newNode: NNFNode, successors: List[CNF]) => {
        NNFNode.updateCache.clear()
        val newSuccessors = successors ++ partialCircuit._2.tail
        println("The number of uncompiled theories changed from " +
                  partialCircuit._2.size + " to " + newSuccessors.size)
        (partialCircuit._1.addNode(newNode)._1, newSuccessors)
      }
    }
    Queue(answer: _*)
  }

  /** (Lazily) produces a stream of circuits that compute the WFOMC of the
    given theory */
  def breadthFirstTraverse(cnf: CNF): Stream[NNFNode] = {
    def recurse(q: Queue[PartialCircuit]): Stream[NNFNode] = if (q.isEmpty) {
      Stream.Empty
    } else {
      val (partialCircuit, tail) = q.dequeue
      if (partialCircuit._2.isEmpty) { // a complete circuit
        println("Found a circuit!")
        partialCircuit._1 #:: recurse(tail ++ nextCircuits(partialCircuit))
      }
      else {
        recurse(tail ++ nextCircuits(partialCircuit))
      }
    }
    recurse(Queue.empty ++ applyRules(cnf))
  }

  /** A simple BFS mainly for testing purposes */
  def breadthFirstSearch(cnf: CNF): NNFNode = {
    val maxSolutionCount = 1 // TODO: maybe replace this with max depth
    val circuits = breadthFirstTraverse(cnf).take(maxSolutionCount)
    circuits.foreach { _.showPDF(null, null) }
    circuits.head
  }

  def compile(cnf: CNF): NNFNode = {
    if (Compiler.neverRun) {
      Compiler.neverRun = false
      val nnf = if (Compiler.greedy) greedySearch(cnf)
                else breadthFirstSearch(cnf)
      val postOrderVisitor = new PostOrderVisitor
      postOrderVisitor.visit(nnf)
      val domainsVisitor = new DomainsVisitor(postOrderVisitor.nodeOrder)
      domainsVisitor.updateDomains
      nnf
    } else {
      greedySearch(cnf)
    }
  }

  def cannotCompile(cnf: CNF): NNFNode

}
