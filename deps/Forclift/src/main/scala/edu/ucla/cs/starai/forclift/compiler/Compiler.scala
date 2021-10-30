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

import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object Compiler {

  object Builder {
    val default: Builder = CompilerWrapper.builder
    val defaultWithGrounding: Builder = CompilerWrapper.builderWithGrounding
  }

  type Builder = ((Domain => Int) => Compiler)

  def default: Compiler = new CompilerWrapper(grounding = false)

  object SizeHints {
    def unknown(d: Domain) = 1
  }

  type SizeHints = (Domain => Int)

  def checkCnfInput(cnf: CNF) {
    require(!cnf.domains.contains(Universe), s"Cannot compile CNFs containing the universe domain: $cnf")
    require(!cnf.domains.contains(EmptyDomain), s"Cannot compile CNFs containing the empty domain: $cnf")
  }

}

trait Compiler {

  def compile(cnf: CNF): NNFNode = {
    throw new IllegalStateException("The compiler you are trying to use does " +
                                      "not implement the 'compile' method.")
  }

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

abstract class AbstractCompiler(
  val nnfCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] =
    new mutable.HashMap[Int, List[(CNF, NNFNode)]]) extends Compiler {

  def myClone: AbstractCompiler

  def updateCache(cnf: CNF, nnf: NNFNode) = {
    assume(nnf != null)
    if (!nnfCache.contains(cnf.hashCode) || !nnfCache(cnf.hashCode).exists {
          case (theory, _) => theory == cnf } )
      nnfCache(cnf.hashCode) = (cnf, nnf) ::
        nnfCache.getOrElse(cnf.hashCode, List[(CNF, NNFNode)]())
  }

  def tryCache(cnf: CNF): InferenceResult = {
    // println("tryCache started for theory:")
    // println(cnf)
    // println("tryCache: looking for " + cnf.hashCode + " among " +
    //           nnfCache.keySet.toList.sorted)
    if (!nnfCache.contains(cnf.hashCode)) {
      // println("tryCache: not found")
      None
    } else {
      // println("tryCache: found")
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
          // println("tryCache finished")
          Some((Some(node), List[CNF]()))
        }
        case None => {
          // println("tryCache finished")
          None
        }
      }
    }
  }

  // None means that the inference rule doesn't apply. (None, _) means that a
  // new node wasn't created but the theory was updated (we assume that in this
  // case the list has exactly one element). In all other cases, the list holds
  // theories that will be compiled into direct successors of the returned node.
  type InferenceResult = Option[(Option[NNFNode], List[CNF])]
  type InferenceRule = CNF => InferenceResult

  // NOTE: we assume that inferenceRules = sinkRules ++ nonSinkRules (possibly
  // in a different order)
  def sinkRules: List[InferenceRule]
  def nonSinkRules: List[InferenceRule]
  def inferenceRules: List[InferenceRule]

  var nbCompilationSteps = 0;

  def cannotCompile(cnf: CNF): NNFNode

}
