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

import java.util.concurrent._

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

  def compile(cnf: CNF): List[NNFNode] = {
    throw new IllegalStateException("The compiler you are trying to use does " +
                                      "not implement the 'compile' method.")
  }

  def compileSmooth(cnf: CNF): List[NNFNode] = {
    compileSmooth(cnf, cnf.predicates, Set.empty)
  }

  def compileSmooth(cnf: CNF, predicates: Set[Predicate],
                    excluded: Set[PositiveUnitClause] = Set.empty)
      : List[NNFNode] = {
    val nnfs = compile(cnf)
    nnfs.map { _.smoothWithPredicates(predicates, excluded) }
  }

  def foundSolution(circuit: NNFNode): Unit = {}

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

/** nnfCache maps hash codes of formulas to pairs of formulas and their circuit
  nodes. */
abstract class AbstractCompiler(
  val nnfCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] =
    new mutable.HashMap[Int, List[(CNF, NNFNode)]]) extends Compiler {

  // None means that the inference rule doesn't apply. (None, _) means that a
  // new node wasn't created but the theory was updated (we assume that in this
  // case the list has exactly one element). In all other cases, the list holds
  // theories that will be compiled into direct successors of the returned node.
  type InferenceResult = Option[(Option[NNFNode], List[CNF])]
  type InferenceRule = CNF => InferenceResult

  // val Timeout = 1 // s, for tryCache
  val Verbose = false

  def log(s: String) = if (Verbose) println(s)

  def cloneCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] = nnfCache.map {
    case (key, value) => {
      val newValue = value.map {
        case (formula, node) => {
          // println("Does cloningCache of size " + NNFNode.cloningCache.size +
          //           " contain the " + node.getClass.getSimpleName + " " +
          //           node.hashCode + ": " + NNFNode.cloningCache.contains(node))

          (formula, NNFNode.cloningCache(node))
        }
      }
      (key, newValue)
    }
  }

  def myClone: AbstractCompiler

  def updateCache(cnf: CNF, nnf: NNFNode) = {
    assume(nnf != null)

    // println("updateCache: trying to add " + nnf.getClass.getSimpleName + " " +
    //           nnf.hashCode)
    // println("updateCache: hash code already exists: " +
    //           nnfCache.contains(cnf.hashCode))
    // println("updateCache: a cache entry for the same node exists: " +
    //           (nnfCache.contains(cnf.hashCode) && nnfCache(cnf.hashCode).exists
    //              { case (_, node) => node == nnf }))

    if (!nnfCache.contains(cnf.hashCode) || !nnfCache(cnf.hashCode).exists {
          case (_, node) => node == nnf } )
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
      // println("tryCache: found. The bucket has " + nnfCache(cnf.hashCode).size +
      //           " elements.")
      nnfCache(cnf.hashCode).flatMap {
        case (theory, circuit) => {
          // println("tryCache: calling identifyRecursion on:")
          // println(cnf)
          // println("tryCache: AND")
          // println(theory)

          CNF.identifyRecursion(cnf, theory) match {
            case Some(recursion) => Some((circuit, recursion))
            case None => None
          }} }.headOption match {
        case Some(results) => {
          // println("\nCache hit.")
          // println("Before:")
          // println(cnf)
          // println("After:")
          // println(results._1.cnf)
          // println("Domain map:")
          // println(results._2)

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

  /* With support for timeout! */
  // def tryCache(cnf: CNF): InferenceResult = {
  //   val task = new FutureTask(new Callable[InferenceResult]() {
  //     def call() = tryCache2(cnf)
  //   })

  //   try {
  //     new Thread(task).start()
  //     task.get(Timeout, TimeUnit.SECONDS)
  //   } catch {
  //     case _: TimeoutException => None
  //   }
  // }

  // NOTE: we assume that inferenceRules = sinkRules ++ nonSinkRules (possibly
  // in a different order)
  def sinkRules: List[InferenceRule]
  def nonSinkRules: List[InferenceRule]
  def inferenceRules: List[InferenceRule]

  def applyIthRule(i: Int, cnf: CNF): InferenceResult = nonSinkRules(i)(cnf)

  def applySinkRules(cnf: CNF): PartialCircuit = {
    // println("applySinkRules: started on:")
    // println(cnf)
    var rules = sinkRules
    while (rules.nonEmpty) {
      // println("applySinkRules: attempting a new rule")
      rules.head(cnf) match {
        case None => {
          rules = rules.tail
          // println("applySinkRules: the rule doesn't apply here")
        }
        case Some((node, successors)) => {
          // println("applySinkRules: successfully applied a rule")
          node match {
            case None => {
              require(successors.size == 1)
              return applySinkRules(successors.head)
            }
            case Some(nnf) => {
              updateCache(cnf, nnf)
              val newSuccessors = applySinkRulesToAllFormulas(nnf, successors)
              return new PartialCircuit(this, Some(nnf), newSuccessors)
            }
          }
        }
      }
    }
    // println("applySinkRules: finished")
    new PartialCircuit(this, None, List(cnf))
  }

  /** Updates the circuit and returns a list of new formulas */
  def applySinkRulesToAllFormulas(node: NNFNode,
                                  formulas: List[CNF]): List[CNF] = {
    require(formulas.size == node.directSuccessors.count(_.isEmpty))
    val newFormulas = formulas.map { applySinkRules(_) } .flatMap {
      partialCircuit => {
        if (partialCircuit.circuit.isDefined) {

          // println("applySinkRulesToAllFormulas: adding " +
          //           partialCircuit.circuit.get.getClass.getSimpleName +
          //           " as a direct successors of " + node.getClass.getSimpleName)

          require(node.updateFirst(partialCircuit.circuit.get))
        }
        partialCircuit.formulas
      }
    }
    // println("applySinkRulesToAllFormulas: finished")
    newFormulas
  }

  var nbCompilationSteps = 0;

  def cannotCompile(cnf: CNF): NNFNode

}
