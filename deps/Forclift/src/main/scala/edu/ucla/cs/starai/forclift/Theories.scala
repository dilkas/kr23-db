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

package edu.ucla.cs.starai.forclift

import collection._
import scala.collection.immutable.Stream
import edu.ucla.cs.starai.forclift.inference._

final case class CNF(val clauses: List[Clause]) extends SetProxy[Clause] {

  lazy val self = clauses.toSet

  override lazy val hashCode = super.hashCode

  lazy val variables = clauses.flatMap { _.literalVariables }.toSet

  def isGround = variables.isEmpty

  lazy val constants = clauses.flatMap { _.constants }.toSet

  lazy val predicates = clauses.flatMap { _.predicates }.toSet

  def domains = clauses.flatMap { _.domains }.toSet

  lazy val atoms = clauses.flatMap { _.atoms }.toSet

  def isSingleton = clauses.nonEmpty && clauses.tail.isEmpty

  def toLatex(showRootDomains: Boolean = false): String = {
    if (clauses.isEmpty) """$\top$"""
    else {
      clauses.map { _.toLatex(showRootDomains) }.mkString("""$\begin{array}{c} """, """ \\ """, """ \end{array}$""")
    }
  }

  override def +(clause: Clause) = new CNF(clause :: clauses)

  def isTautology = clauses.isEmpty

  def removeTautologies: CNF = {
    val tautologyFreeClauses = clauses.filterNot { _.isTautology }
    if (tautologyFreeClauses.size == clauses.size) this
    else new CNF(tautologyFreeClauses)
  }

  def toPositiveUnitClauses: Set[PositiveUnitClause] = clauses.flatMap { _.toPositiveUnitClauses }.toSet

  lazy val distinctPositiveUnitClauses = {
    val allCAtoms = clauses.flatMap { _.toPositiveUnitClauses }
    val distinctCAtoms = disjoin(allCAtoms)
    distinctCAtoms
  }

  def coShatter = shatterInternalEqualities.shatter

  def equiprobableClasses = {
    val coShatteredThis = coShatter
    val allCAtoms = coShatteredThis.clauses.flatMap { _.toPositiveUnitClauses.map(_.removeExternalConstraints) }
    disjoin(allCAtoms)
  }

  def shatterInternalEqualities: CNF = {
    new CNF(clauses.flatMap { _.shatterInternalEqualities })
  }

  lazy val shatter: CNF = {
    shatterIneqs.shatterDomains
  }

  private[this] final def disjoin(cAtoms: List[PositiveUnitClause]): List[PositiveUnitClause] = {
    val atomsByPredicate = cAtoms.groupBy { _.atom.predicate }
    atomsByPredicate.values.flatMap { predAtoms =>
      val uniqueStringAtoms = predAtoms.map { a => (a.toString -> a) }.toMap
      disjoinHard(uniqueStringAtoms.values.toList)
    }.toList
  }

  private[this] final def disjoinHard(cAtoms: List[PositiveUnitClause]): List[PositiveUnitClause] = cAtoms match {
    case Nil => Nil
    case cAtom :: restAtoms => {
      val standerdizedAtom = cAtom.standardizeApart
      standerdizedAtom :: disjoin(restAtoms.filterNot(standerdizedAtom.equivalent(_)))
    }

  }

  final def shatterIneqs: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          val shatteredForCAtom = shClause.flatMap { _.shatter(cAtom.atom, cAtom.constrs) }
          shClause = shatteredForCAtom
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap { _.toPositiveUnitClauses } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      cAtoms = disjoin(nextCAtoms.filter { next => !cAtoms.exists { next.subsumes(_) } })
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(clauses.flatMap { _.shatterIneqDomains })).shatterIneqs
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses)
    }
  }

  final private def shatterDomains: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          shClause = shClause.flatMap { _.shatterDomains(cAtom.atom, cAtom.constrs) }
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap { _.toPositiveUnitClauses } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      cAtoms = disjoin(nextCAtoms.filter { next => !cAtoms.exists { next.subsumes(_) } })
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(clauses.flatMap { _.shatterIneqDomains })).shatterDomains
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses)
    }
  }

  def ++(other: CNF) = {
    new CNF(clauses ++ other.clauses)
  }

  def ground(domainSizes: DomainSizes): CNF = {
    new CNF(clauses.flatMap { _.ground(domainSizes) })
  }

  def condition(cliteral: UnitClause): CNF = {
    val conditionedClauses = cliteral match {
      case posLit: PositiveUnitClause =>
        clauses.flatMap { _.condition(true, posLit.atom, posLit.constrs) }
      case negLit: NegativeUnitClause =>
        clauses.flatMap { _.condition(false, negLit.atom, negLit.constrs) }
      case _ => throw new IllegalStateException("Clause not positive or negative")
    }
    new CNF(conditionedClauses)
  }

  def simplify(ignoredClauses: Set[Clause] = Set.empty): CNF = {
    val propagatableUnitClauses = clauses.filter { propClause =>
      !ignoredClauses(propClause) && propClause.isUnitClause && clauses.exists { clause =>
        !(clause eq propClause) && !propClause.independent(clause)
      }
    }
    if (propagatableUnitClauses.nonEmpty) {
      def propagate(unitClauses: List[Clause], theory: List[Clause]): (List[Clause], List[Clause]) = unitClauses match {
        case Nil => (Nil, theory)
        case unitClause :: otherUnitClauses => {
          val unitLiteral = unitClause.atoms.head
          val otherClauses: List[Clause] = theory filterNot (_ == unitClause)
          val propagatedClauses = otherClauses.flatMap { clause =>
            val conditionedClause = clause.condition(unitClause.isPositiveUnitClause, unitLiteral, unitClause.constrs)
            conditionedClause
          }.toList
          val propagatedOtherUnitClauses = otherUnitClauses.flatMap { _.condition(unitClause.isPositiveUnitClause, unitLiteral, unitClause.constrs) }
          val (propagatedUnitClauses, simplifiedRest) = propagate(propagatedOtherUnitClauses, propagatedClauses)
          (unitClause :: propagatedUnitClauses, simplifiedRest)
        }
      }
      val (propagated, simplifiedRest) = propagate(propagatableUnitClauses, clauses)
      val newCnf = (new CNF(propagated ++ simplifiedRest)).simplify(ignoredClauses ++ propagated)
      newCnf
    } else this
  }

  def independent(other: Clause) = clauses.forall(_.independent(other))
  def dependent(other: Clause) = !independent(other)

  def independentSubtheories: List[CNF] = {
    def partition(depClauses: List[Clause], indepClauses: List[Clause]): (List[Clause], List[Clause]) = {
      if (indepClauses.isEmpty) (depClauses, Nil)
      else depClauses match {
        case clause :: rest => {
          val (indep, dep) = indepClauses.partition(clause.independent(_))
          val (depAll, indepAll) = partition(rest ++ dep, indep)
          (clause :: depAll, indepAll)
        }
        case Nil => (Nil, indepClauses)
      }
    }
    val (dep, indep) = partition(List(clauses.head), clauses.tail)
    if (indep.isEmpty) List(this)
    else (new CNF(dep)) :: (new CNF(indep)).independentSubtheories
  }

  override def toString = {
    if (isTautology) """true"""
    else clauses.toArray.sortBy { _.toString }.mkString("\n")
  }

  def eqToConstraints: CNF = {
    new CNF(clauses.map(_.eqToConstraints))
  }

}

object CNF {

  def apply(clauses: Clause*) = new CNF(clauses.toList)

  // The first element of each pair is the domain to be compared,
  // and the second element is its 'origin' domain.
  type State = (Int, Iterable[(Domain, Domain)])

  final case class DomainNotMatchedException(
    private val message: String = "",
    private val cause: Throwable = None.orNull) extends
      Exception(message, cause)

  /** Increment the time counter and replace each candidate domain with its
    parent if possible, and eliminate it completely if not. */
  def update(state: State): State = (
    state._1 + 1,
    state._2.flatMap { candidate => candidate._1.parents.headOption match {
                        case Some(p)  => Some((p, candidate._2))
                        case None => None
                      } }
  )

  def findDomain(domain: Domain)(state: State): Option[(Domain, Int)] =
    if (state._2.isEmpty) {
      throw DomainNotMatchedException()
    } else {
      state._2.find { _._1 == domain } match {
        case Some(candidate) => Some((candidate._2, state._1))
        case None => None
      }
    }

  /** If possible, identify the values in the map as subsets of some of the
    keys. */
  def postprocess(domainMap: Map[Domain, Domain])
      : Option[Map[Domain, (Domain, Int)]] = {
    println("Postprocessing " + domainMap)
    try {
      Some(domainMap.map {
             case (d1, d2) => {
               lazy val stateStream: Stream[State] = (0, domainMap.keys.map {
                                                        d: Domain => (d, d) }
               ) #:: stateStream.map(update)
               stateStream.flatMap(findDomain(d2)).headOption match {
                 case Some(d) => (d1, d)
                 case None => throw DomainNotMatchedException()
               }
             } }.toMap)
    } catch {
      case c: DomainNotMatchedException => None
    }
  }

  // NewTheory is the theory that corresponds to the new circuit node which is
  // being added whereas oldTheory is the theory for an already-existing node.
  // PartialMap maps domains of oldTheory to domains of newTheory. Before
  // returning this map, we express all domains of newTheory that appear in
  // partialMap as (subsets of) domains of oldTheory.
  def identifyRecursion(newTheory: CNF, oldTheory: CNF,
                        partialMap: Map[Domain, Domain] = Map.empty)
      : Option[Map[Domain, (Domain, Int)]] = {
    if (newTheory.size != oldTheory.size ||
          newTheory.hashCode != oldTheory.hashCode) {
      println("different: " + newTheory + " AND " + oldTheory)
      None
    } else if (oldTheory.isEmpty) {
      postprocess(partialMap)
    } else {
      for (clause1 <- oldTheory) {
        val updatedOldTheory = new CNF((oldTheory - clause1).toList)
        for (clause2 <- newTheory.filter { _.hashCode == clause1.hashCode } ) {
          val updatedNewTheory = new CNF((newTheory - clause2).toList)

          def myFilter(bijection: Map[Var, Var]): Boolean = bijection.forall {
            case (v1, v2) => {
              val d1 = clause1.constrs.domainFor(v1)
                !partialMap.contains(d1) ||
                partialMap(d1) == clause2.constrs.domainFor(v2)
            }
          }

          for {
            bijection <- clause1.variableBijections(clause2, myFilter)
            if clause1.substitute(bijection).exactlyEquals(clause2)
          } {
            println("clause1 has " + clause1.allVariables.size + " variables")
            val updatedMap = partialMap ++ clause1.allVariables.map {
              v => {
                println("Adding domains (" + clause1.constrs.domainFor(v) +
                          ", " + clause2.constrs.domainFor(bijection(v)) +
                          ") for variable " + v)
                (clause1.constrs.domainFor(v),
                 clause2.constrs.domainFor(bijection(v)))
              } }
            val recursion = identifyRecursion(updatedNewTheory,
                                              updatedOldTheory, updatedMap)
            if (recursion.isDefined) {
              println("compatible: " + newTheory + " AND " + oldTheory)
              return recursion
            }
          }
        }
      }
      println("different: " + newTheory + " AND " + oldTheory)
      None
    }
  }

}
