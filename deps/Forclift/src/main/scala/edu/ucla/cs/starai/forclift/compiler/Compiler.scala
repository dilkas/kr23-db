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

  val nnfCache = new mutable.HashMap[CNF, NNFNode]

  def updateCache(cnf: CNF, nnf: NNFNode) {
    assume(nnf != null)
    if (!nnfCache.contains(cnf)) nnfCache(cnf) = nnf
  }

  def tryCache(cnf: CNF) = {
    println("The cache has " + nnfCache.size + " elements.");
    nnfCache.get(cnf).map {
      n: NNFNode => {
        println("Cache hit.")
        new Ref(cnf, Some(n), createDomainMap(cnf, n.cnf), "Cache hit.")
      }
    }
  }

  type InferenceRule = CNF => Option[NNFNode]

  def inferenceRules: List[InferenceRule]

  var nbCompilationSteps = 0;

  def checkCnfInput(cnf: CNF) {
    require(!cnf.domains.contains(Universe), s"Cannot compile CNFs containing the universe domain: $cnf")
    require(!cnf.domains.contains(EmptyDomain), s"Cannot compile CNFs containing the empty domain: $cnf")
  }

  def compile2(cnf: CNF): NNFNode = {
    checkCnfInput(cnf)
    var rules = inferenceRules
    var nnf: NNFNode = null
    while (nnf == null && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) nnf = tryRule.get
      else rules = rules.tail
    }
    if (nnf == null) {
      nnf = cannotCompile(cnf)
    }
    updateCache(cnf, nnf)
    nnf
  }

  def compile(cnf: CNF): NNFNode = {
    if (Compiler.neverRun) {
      Compiler.neverRun = false
      val nnf = compile2(cnf)
      val postOrderVisitor = new PostOrderVisitor
      postOrderVisitor.visit(nnf)
      val domainsVisitor = new DomainsVisitor(postOrderVisitor.nodeOrder)
      domainsVisitor.updateDomains
      nnf
    } else {
      compile2(tryConstraintRemoval(cnf))
    }
  }

  def cannotCompile(cnf: CNF): NNFNode

  def tryConstraintRemoval(cnf: CNF): CNF = {
    for (originalClause <- cnf) {
      for ((variable, terms) <- originalClause.constrs.ineqConstrs) {
        println("tryConstraintRemoval: inequality constraints for variable " +
                  variable)
        val originalDomain = originalClause.constrs.domainFor(variable)
        for (term <- terms) {
          term match {
            case constant: Constant => {
              // We have a "v != c" constraint. Does it apply to all variables
              // from the same domain across all clauses? And does c occur in atoms?
              // I.e., for each clause, for each variable, either domain is
              // different or there is the same inequality constraint.
              println("tryConstraintRemoval: can " + constant +
                        " be that lucky constant?")
              if (cnf.forall { clause => clause.atoms.forall { atom: Atom =>
                                !atom.constants.contains(constant) } } &&
                    cnf.forall { clause => clause.allVariables.forall {
                                  variable: Var =>
                                  clause.constrs.domainFor(variable) !=
                                    originalDomain ||
                                    clause.constrs.ineqConstrs(variable).
                                    contains(constant) } }) {
                // TODO (Paulius): I think this needs to hold more information, e.g., the decrement
                val newDomain = originalDomain.subdomain(excludedConstants =
                                                           Set(constant))
                val newCnf = CNF(cnf.map { clause =>
                                   clause.removeConstraints(originalDomain,
                                                            constant).
                                     replaceDomains(originalDomain,
                                                    newDomain) }.toList: _*)
                println("constraint removal succeeded")
                println(newCnf)
                return newCnf
              }
            }
            case _ => {}
          }
        }
      }
    }
    cnf
  }

  // TODO (Paulius): rework this.
  def createDomainMap(cnf1: CNF, cnf2: CNF): Map[Domain, Either[Domain, Int]] = {
    val variableMap = cnf1.variableBijections(cnf2).find(cnf1.substitute(_).
                                                           exactlyEquals(cnf2))
    variableMap.map { case (v1, v2) => (cnf1.constrs.domainFor(v1),
                                        cnf2.constrs.domainFor(v2)) }
  }

}
