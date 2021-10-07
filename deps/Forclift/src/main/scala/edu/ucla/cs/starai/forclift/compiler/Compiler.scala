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
          updateCache(cnf, nnf)
          nnf.update(successors.map(greedySearch))
        }
      }
      else rules = rules.tail
    }
    if (nnf == null) {
      nnf = cannotCompile(cnf)
    }
    nnf
  }

  // def breadthFirstTraverse(cnf: CNF, f: CNF => Queue[CNF]): Stream[CNF] = {
  //   def recurse(q: Queue[CNF]): Stream[CNF] = {
  //     if (q.isEmpty) {
  //       Stream.Empty
  //     } else {
  //       val (cnf, tail) = q.dequeue
  //       cnf #:: recurse(tail ++ f(cnf))
  //     }
  //   }

  //   cnf #:: recurse(Queue.empty ++ f(cnf))
  // }

  // TODO: each rule calls compile(). Need to change that. Instead, it should return Option[NNFNode, List[CNF]]. The compilation function then runs updateCache, makes the recursive calls, and calls update on the NNFNode.
  // ???: catch IllegalArgumentException
  // TODO: when to stop?
  // TODO: what combinations of operations should I explore?

  def compile(cnf: CNF): NNFNode = {
    if (Compiler.neverRun) {
      Compiler.neverRun = false
      val nnf = greedySearch(cnf)
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
