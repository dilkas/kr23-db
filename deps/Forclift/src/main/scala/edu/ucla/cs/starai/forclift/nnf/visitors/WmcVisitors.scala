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

package edu.ucla.cs.starai.forclift.nnf.visitors

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference
import scala.language.implicitConversions

import edu.ucla.cs.starai.forclift.inference.DomainSize
import edu.ucla.cs.starai.forclift.inference.DomainSizes
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.SignLogDouble
import edu.ucla.cs.starai.forclift.util.Binomial
import edu.ucla.cs.starai.forclift.inference.WeightedCNF
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.constraints.Constraints
import edu.ucla.cs.starai.forclift.propositional.C2DError
import edu.ucla.cs.starai.forclift.util.LogDouble
import edu.ucla.cs.starai.forclift.util.SoftMemCache
import edu.ucla.cs.starai.forclift.Domain

trait WmcVisitor {
  // For each ParametrisedNode, we hold the new cardinalities for the two
  // domains that were introduced at that node.
  // NOTE: Duplicating this type info 2-3 times is still better than duplicating
  // it ~50 times.
  type ParameterMap = Map[ParametrisedNode, (Int, Int)]
  def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
          predicateWeights: PredicateWeights): SignLogDouble
}

object WmcVisitor {

  // the 'false' option is mostly for debugging purposes unless combined with
  // numSolutions = 1
  val Parallel = true

  val Verbose = false

  val latch = new CountDownLatch(1)
  val wmc = new AtomicReference[SignLogDouble]

  type ParameterMap = Map[ParametrisedNode, (Int, Int)]

  object ParameterMap {
    def empty: ParameterMap = Map.empty[ParametrisedNode, (Int, Int)]
  }

  def apply(predicateWeights: PredicateWeights): WmcVisitor = {
    val hasNegativeWeight = predicateWeights.values.exists(w => w.negW < 0 ||
                                                             w.posW < 0)
    // NOTE: caching needs to be disabled for cycles to make sense
    if (hasNegativeWeight) {
      new SignLogDoubleWmc
      //new CachingSignLogDoubleWmc
    } else {
      new LogDoubleWmc
      //new CachingLogDoubleWmc
    }
  }

}

protected class LogDoubleWmc(val circuit: NNFNode = null,
                             val domainSizes: DomainSizes = null,
                             val predicateWeights: PredicateWeights = null)
    extends NnfVisitor[(DomainSizes, PredicateWeights,
                        WmcVisitor.ParameterMap), LogDouble]
    with Runnable with WmcVisitor {

  import edu.ucla.cs.starai.forclift.util.LogDouble._

  // TODO (Paulius, later): extract the following two functions to reduce
  // duplication
  def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
          predicateWeights: PredicateWeights): SignLogDouble =
    if (WmcVisitor.Parallel) {
      val executor: ExecutorService = Executors.newFixedThreadPool(nnfs.size)
      nnfs.foreach {
        nnf => executor.execute(new LogDoubleWmc(nnf, domainSizes,
                                                 predicateWeights))
      }
      try {
        WmcVisitor.latch.await
        executor.shutdownNow
      } catch {
        case e: InterruptedException => println(e)
      }
      WmcVisitor.wmc.get
    } else {
      nnfs.map {
        nnf => visit(nnf, (domainSizes, predicateWeights,
                           WmcVisitor.ParameterMap.empty))
      }.head
    }

  def run {
    try {
      val wmc = visit(circuit, (domainSizes, predicateWeights,
                                WmcVisitor.ParameterMap.empty))
      WmcVisitor.wmc.set(wmc)
      WmcVisitor.latch.countDown
    } catch {
      case e: InterruptedException => {}
    }
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode,
                                     params: (DomainSizes, PredicateWeights,
                                              ParameterMap))
      : LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      val power = (maxSize * (maxSize - 1)) / 2
      val answer = groundChildWmc.pow(maxSize) * childchildWmc.pow(power)
      if (WmcVisitor.Verbose)
        println(s"$groundChildWmc ^ $maxSize * $childchildWmc ^ $power = $answer (domain recursion)")
      answer
    }
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      if (WmcVisitor.Verbose)
        println("1 (improved domain recursion, base case)")
      one
    }
    else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      if (WmcVisitor.Verbose)
        println(s"$childchildWmc (improved domain recursion)")
      childchildWmc
    }
  }

  protected def visitExists(exists: CountingNode,
                            params: (DomainSizes, PredicateWeights,
                                     ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero
    if (WmcVisitor.Verbose)
      println("exists/counting:")
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
                              + (exists.subdomain, nbTrue)
                              + (exists.subdomain.complement,
                                 (maxSize - nbTrue)))
      val newParams = (newDomainSizes, predicateWeights,
                       parameterMap + (exists -> (nbTrue, maxSize - nbTrue)))
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
      if (WmcVisitor.Verbose)
        println(s" + $maxSize C $nbTrue * $childWeight")
    }
    if (WmcVisitor.Verbose)
      println(s"= $logWeight\n")
    logWeight
  }

  protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode, params: (DomainSizes, PredicateWeights,
                                        ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) 0
    else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1) +
        (cr.subdomain.complement, 1)
      val child = visit(cr.child.get,
                        (newDomainSizes, predicateWeights,
                         parameterMap + (cr -> (domainSize - 1, 1))))
      if (WmcVisitor.Verbose)
        println(s"$child (constraint removal)")
      child
    }
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode,
                                params: (DomainSizes, PredicateWeights,
                                         ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)")
      answer
    }
  }

  protected def visitInclusionExclusionNode(
    ie: InclusionExclusion, params: (DomainSizes, PredicateWeights,
                                     ParameterMap)): LogDouble = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    val answer = plus1lwmc + plus2lwmc - minlwmc
    if (WmcVisitor.Verbose)
      println(s"$plus1lwmc + $plus2lwmc - $minlwmc = $answer (inclusion-exclusion)")
    answer
  }

  protected def visitOrNode(or: Or, params: (DomainSizes, PredicateWeights,
                                             ParameterMap)): LogDouble = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    val answer = llwmcc + rlwmcc
    if (WmcVisitor.Verbose)
      println(s"$llwmcc + $rlwmcc = $answer (or)")
    answer
  }

  protected def visitAndNode(
    and: And, params: (DomainSizes, PredicateWeights, ParameterMap))
      : LogDouble = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      val answer = llwmcc * rlwmcc
      if (WmcVisitor.Verbose)
        println(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  protected def visitRefNode(ref: Ref,
                             params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): LogDouble = try {
    val (domainSizes, predicateWeights, parameterMap) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap, parameterMap)
    val answer = visit(ref.nnfNode.get, (newDomainSizes, predicateWeights,
                                         parameterMap))
    if (WmcVisitor.Verbose)
      println(s"$answer (ref)")
    answer
  } catch {
    case e: DomainSize.CantShrinkDomainException => {
      if (WmcVisitor.Verbose)
        println("1 (ref, base case)")
      1
    }
  }

  protected def visitSmoothingNode(
    leaf: SmoothingNode, params: (DomainSizes, PredicateWeights,
                                  ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    val weight = weights.negWPlusPosWLogDouble
    val answer = weight.pow(nbGroundings)
    if (WmcVisitor.Verbose)
      println(s"$weight ^ $nbGroundings = $answer (smoothing for " + leaf.cnf +
                ")")
    answer
  }

  protected def visitContradictionLeaf(
    leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(
    leaf: UnitLeaf, params: (DomainSizes, PredicateWeights, ParameterMap))
      : LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) {
      one
    } else if (leaf.positive) {
      val weight = weights.posWLogDouble
      val answer = weight.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$weight ^ $nbGroundings = $answer (positive leaf)")
      answer
    } else {
      val weight = weights.negWLogDouble
      val answer = weight.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$weight ^ $nbGroundings = $answer (negative leaf)")
      answer
    }
  }

  protected def visitGroundingNode(
    leaf: GroundingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc.toLogDouble
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitFalse(params: (DomainSizes, PredicateWeights,
                                    ParameterMap)): LogDouble = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights,
                                   ParameterMap)): LogDouble = one

}


object NnfVisitorCache {

  var hit = 0;
  var miss = 0;

  @inline def addHit(){}
  @inline def addMiss(){}
  @inline def info(){}

//  @inline def addHit(){ hit += 1; info}
//  @inline def addMiss(){ miss += 1; info}
//  @inline def info(){
//    if((hit+miss)%10000 == 0 ){
//      println(s"hits: $hit, miss: $miss, hitRate: ${hit*1.0/(hit+miss)}")
//    }
//  }

  class Key(val node: NNFNode, val domainSizes: IndexedSeq[Int]) {

    override def equals(that: Any) = {
      if (that == null || !that.isInstanceOf[Key]) false
      else {
        val thatKey = that.asInstanceOf[Key]
        this.node == thatKey.node && this.domainSizes == thatKey.domainSizes
      }
    }

    override val hashCode = (node, domainSizes).hashCode

  }

}

protected class CachingLogDoubleWmc extends LogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.LogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, LogDouble]

  override def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
                   predicateWeights: PredicateWeights): SignLogDouble = {
    cache.clear()
    super.wmc(nnfs, domainSizes, predicateWeights)
  }

  // only decomposition nodes can reduce the number of relevant domains!

  override protected def visitAndNode(
    and: And, params: (DomainSizes, PredicateWeights, ParameterMap))
      : LogDouble = {
    val llwmcc = retrieveWmc(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r.get, params)
      val answer = llwmcc * rlwmcc
      if (WmcVisitor.Verbose)
        println(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  override protected def visitForallNode(
    forall: IndependentPartialGroundingNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): LogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val childlwmc = retrieveWmc(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)")
      answer
    }
  }

  @inline private def retrieveWmc(
    node: NNFNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : LogDouble = {
    if(node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1){
      return visit(node, params)
    }else {
	    val domains: IndexedSeq[Domain] = node.orderedDomains
	    val (domainSizes, predicateWeights, parameterMap) = params
	    val key = new Key(node, domains.map(domainSizes(_).size))
	    val result = cache.get(key)
	    if (result.nonEmpty) {
	      addHit()
	      result.get
	    }else {
	      addMiss()
	      val childWeight = visit(node, params)
	      cache.update(key, childWeight)
	      childWeight
	    }
    }
  }

}

protected class SignLogDoubleWmc(val circuit: NNFNode = null,
                                 val domainSizes: DomainSizes = null,
                                 val predicateWeights: PredicateWeights = null)
    extends NnfVisitor[(DomainSizes, PredicateWeights,
                        WmcVisitor.ParameterMap), SignLogDouble]
    with Runnable with WmcVisitor {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._

  def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
          predicateWeights: PredicateWeights): SignLogDouble =
    if (WmcVisitor.Parallel) {
      val executor: ExecutorService = Executors.newFixedThreadPool(nnfs.size)
      nnfs.foreach {
        nnf => executor.execute(new SignLogDoubleWmc(nnf, domainSizes,
                                                     predicateWeights))
      }
      try {
        WmcVisitor.latch.await
        executor.shutdownNow
      } catch {
        case e: InterruptedException => println(e)
      }
      WmcVisitor.wmc.get
    } else {
      nnfs.map {
        nnf => visit(nnf, (domainSizes, predicateWeights,
                           WmcVisitor.ParameterMap.empty))
      }.head
    }

  def run {
    try {
      val wmc = visit(circuit, (domainSizes, predicateWeights,
                                WmcVisitor.ParameterMap.empty))
      WmcVisitor.wmc.set(wmc)
      WmcVisitor.latch.countDown
    } catch {
      case e: InterruptedException => {}
    }
  }

  protected def visitDomainRecursion(
    dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      val power = (maxSize * (maxSize - 1)) / 2
      val answer = groundChildWmc.pow(maxSize) * childchildWmc.pow(power)
      if (WmcVisitor.Verbose)
        println(s"$groundChildWmc ^ $maxSize * $childchildWmc ^ $power = $answer (domain recursion)")
      answer
    }
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                               ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      if (WmcVisitor.Verbose)
        println("1 (improved domain recursion, base case)")
      one
    }
    else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      if (WmcVisitor.Verbose)
        println(s"$childchildWmc (improved domain recursion)")
      childchildWmc
    }
  }

  protected def visitExists(
    exists: CountingNode, params: (DomainSizes, PredicateWeights,
                                   ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
                              + (exists.subdomain, nbTrue)
                              + (exists.subdomain.complement,
                                 (maxSize - nbTrue)));
      val newParams = (newDomainSizes, predicateWeights,
                       parameterMap + (exists -> (nbTrue, maxSize - nbTrue)))
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = Binomial.coeff(maxSize, nbTrue).toSignDouble
      logWeight += binomialCoeff * childWeight
      if (WmcVisitor.Verbose)
        println(s" + $maxSize C $nbTrue * $childWeight")
    }
    if (WmcVisitor.Verbose)
      println(s"= $logWeight\n")
    logWeight
  }

  protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode, params: (DomainSizes, PredicateWeights,
                                        ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) 0
    else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1) +
        (cr.subdomain.complement, 1)
      //println("Adding domain " + cr.subdomain + " of cardinality " + (newDomainSize - 1))
      val child = visit(cr.child.get,
                        (newDomainSizes, predicateWeights,
                         parameterMap + (cr -> (domainSize - 1, 1))))
      if (WmcVisitor.Verbose)
        println(s"$child (constraint removal)")
      child
    }
  }

  protected def visitForallNode(
    forall: IndependentPartialGroundingNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)")
      answer
    }
  }

  protected def visitInclusionExclusionNode(
    ie: InclusionExclusion, params: (DomainSizes, PredicateWeights,
                                     ParameterMap)): SignLogDouble = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    val answer = plus1lwmc + plus2lwmc - minlwmc
    if (WmcVisitor.Verbose)
      println(s"$plus1lwmc + $plus2lwmc - $minlwmc = $answer (inclusion-exclusion)")
    answer
  }

  protected def visitOrNode(
    or: Or, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    val answer = llwmcc + rlwmcc
    if (WmcVisitor.Verbose)
      println(s"$llwmcc + $rlwmcc = $answer (or)")
    answer
  }

  protected def visitAndNode(
    and: And, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      val answer = llwmcc * rlwmcc
      if (WmcVisitor.Verbose)
        println(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  protected def visitRefNode(
    ref: Ref, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = try {
    val (domainSizes, predicateWeights, parameterMap) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap, parameterMap)
    val answer = visit(ref.nnfNode.get, (newDomainSizes, predicateWeights, parameterMap))
    if (WmcVisitor.Verbose)
      println(s"$answer (ref)")
    answer
  } catch {
    case e: DomainSize.CantShrinkDomainException => {
      if (WmcVisitor.Verbose)
        println("1 (ref, base case)")
      1
    }
  }

  protected def visitSmoothingNode(
    leaf: SmoothingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    val weight = weights.negWPlusPosW
    val answer = weight.pow(nbGroundings)
    if (WmcVisitor.Verbose)
      println(s"$weight ^ $nbGroundings = $answer (smoothing for " + leaf.cnf +
                ")")
    answer
  }

  protected def visitContradictionLeaf(
    leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(
    leaf: UnitLeaf, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) {
      one
    } else if (leaf.positive) {
      val weight = weights.posW
      val answer = weight.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$weight ^ $nbGroundings = $answer (positive leaf)")
      answer
    } else {
      val weight = weights.negW
      val answer = weight.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$weight ^ $nbGroundings = $answer (negative leaf)")
      answer
    }
  }

  protected def visitGroundingNode(
    leaf: GroundingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weightedCNF = WeightedCNF(leaf.cnf, domainSizes, predicateWeights)
    val logWmc = weightedCNF.logPropWmc
    assume(!logWmc.isNaN)
    logWmc
  }

  protected def visitFalse(params: (DomainSizes, PredicateWeights,
                                    ParameterMap)): SignLogDouble = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights,
                                   ParameterMap)): SignLogDouble = one

}


protected class CachingSignLogDoubleWmc extends SignLogDoubleWmc {

  import edu.ucla.cs.starai.forclift.util.SignLogDouble._
  import NnfVisitorCache._

  val cache = new SoftMemCache[Key, SignLogDouble]

  override def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
                   predicateWeights: PredicateWeights): SignLogDouble = {
    cache.clear()
    super.wmc(nnfs, domainSizes, predicateWeights)
  }

  // only decomposition nodes can reduce the number of relevant domains!

  override protected def visitAndNode(
    and: And, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val llwmcc = retrieveWmc(and.l.get, params)
    if (llwmcc.isZero) zero
    else {
      val rlwmcc = retrieveWmc(and.r.get, params)
      val answer = llwmcc * rlwmcc
      if (WmcVisitor.Verbose)
        println(s"$llwmcc * $rlwmcc = $answer (and)")
      answer
    }
  }

  override protected def visitForallNode(
    forall: IndependentPartialGroundingNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val childlwmc = retrieveWmc(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      val answer = childlwmc.pow(nbGroundings)
      if (WmcVisitor.Verbose)
        println(s"$childlwmc ^ $nbGroundings = $answer (forall / independent partial grounding)")
      answer
    }
  }

  @inline private def retrieveWmc(
    node: NNFNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    if(node.evalOrder == 0) {
      // there is no point in caching if the computation is O(1)
      return visit(node, params)
    } else{
	    val domains: IndexedSeq[Domain] = node.orderedDomains
	    val (domainSizes, predicateWeights, parameterMap) = params
	    val key = new Key(node, domains.map(domainSizes(_).size))
	    val result = cache.get(key)
	    if (result.nonEmpty) {
	      addHit()
	      result.get
	    }else {
	      addMiss()
	      val childWeight = visit(node, params)
	      cache.update(key, childWeight)
	      childWeight
	    }
    }
  }

}

class SafeSignLogDoubleWmc extends SignLogDoubleWmc {

  import SignLogDouble._

  override protected def visitForallNode(
    forall: IndependentPartialGroundingNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (domainSizes.contains(forall.d)) {
      super.visitForallNode(forall, params)
    } else NaN
  }

  override protected def visitExists(
    exists: CountingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (domainSizes.contains(exists.domain)) {
      super.visitExists(exists, params)
    } else NaN
  }

  override protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode, params: (DomainSizes, PredicateWeights,
                                        ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (domainSizes.contains(cr.domain)) {
      super.visitConstraintRemovalNode(cr, params)
    } else NaN
  }

  override protected def visitDomainRecursion(
    dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)) = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (domainSizes.contains(dr.domain)) {
      super.visitDomainRecursion(dr, params)
    } else NaN
  }

  override protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                               ParameterMap)) = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (domainSizes.contains(idr.domain)) {
      super.visitImprovedDomainRecursion(idr, params)
    } else NaN
  }

  override protected def visitSmoothingNode(
    leaf: SmoothingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitSmoothingNode(leaf, params)
    } else NaN
  }

  override protected def visitUnitLeaf(
    leaf: UnitLeaf, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitUnitLeaf(leaf, params)
    } else NaN
  }

  override protected def visitContradictionLeaf(
    leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (leaf.clause.constrs.domains.forall { domainSizes.contains(_) }) {
      super.visitContradictionLeaf(leaf, params)
    } else NaN
  }

  override protected def visitGroundingNode(
    leaf: GroundingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val (domainSizes, predicateWeights, parameterMap) = params
    if (leaf.cnf.domains.forall { domainSizes.contains(_) }) {
      super.visitGroundingNode(leaf, params)
    } else NaN
  }

}

/**
 * Compute WMC while also verifying the counts of every intermediate step using C2D
 */

object VerifyWmcVisitor {

  class VerificationFailedException extends Exception

  def verify(nnfs: List[NNFNode], domainSizes: DomainSizes,
             predicateWeights: PredicateWeights) {
    require(nnfs.size == 1)
    (new VerifyWmcVisitor()).visit(nnfs.head, (domainSizes, predicateWeights,
                                               WmcVisitor.ParameterMap.empty))
  }

}

protected class VerifyWmcVisitor extends SignLogDoubleWmc {

  val nonVerifyingWmcVisitor = new SignLogDoubleWmc

  override def visit(
    nnfNode: NNFNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : SignLogDouble = {
    val wmc = super.visit(nnfNode, params)
    verifyLocal(nnfNode, params)
    wmc
  }

  protected def verifyLocal(
    nnfNode: NNFNode, params: (DomainSizes, PredicateWeights, ParameterMap)) {
    val (domainSizes, predicateWeights, parameterMap) = params
    try {
      val cnf = nnfNode.cnf
      val weightedCNF = WeightedCNF(cnf, domainSizes, predicateWeights)
      // ground truth (pun intended)
      val groundCNF = cnf.ground(domainSizes)
      // smooth with everything appearing in the ground cnf, not with every predicate grounding!!
      val atomsInPropWmc = groundCNF.atoms.map { new PositiveUnitClause(_, Constraints.empty) }
      val thisSmooth = nnfNode.smooth
      // not used -- debugging purposes only
      val onceSmoothedLiftedLogWmc = nonVerifyingWmcVisitor.visit(thisSmooth, params)
      // must also ground before subtracting, otherwise constants will unify with empty domain variables
      val atomsInLiftedWmc = nnfNode.variablesForSmoothing.flatMap {
        _.ground(domainSizes).map { _.toPositiveUnitClause } }
      val atomsMissingFromLiftedWmc = atomsInPropWmc.flatMap { _.minus(atomsInLiftedWmc) }
      val atomsMissingFromPropWmc = atomsInLiftedWmc.flatMap { _.minus(atomsInPropWmc) }

      val thisTwiceSmoothed = thisSmooth.smoothWith(atomsMissingFromLiftedWmc)
      val twiceSmoothedLiftedLogWmc = nonVerifyingWmcVisitor.visit(thisTwiceSmoothed, params)

      val weightsMissingFromPropWmc = atomsMissingFromPropWmc.toList.map { clause =>
        nonVerifyingWmcVisitor.visit((new SmoothingNode(clause)), params)
      }
      val propLogWmc = weightedCNF.logPropWmc
      val twiceSmoothedPropLogWmc = {
        weightsMissingFromPropWmc.foldLeft(propLogWmc) { _ * _ }
      }

      val correct = ((twiceSmoothedPropLogWmc == twiceSmoothedLiftedLogWmc)
        || twiceSmoothedPropLogWmc.isZero && twiceSmoothedLiftedLogWmc.isZero
        || (twiceSmoothedPropLogWmc.logToDouble - twiceSmoothedLiftedLogWmc.logToDouble).abs < 0.0000001
        || (twiceSmoothedPropLogWmc - twiceSmoothedLiftedLogWmc).abs.logToDouble < 0.0000001)
      if (!correct) {
        println("WMC:")
        println(weightedCNF)
        println("Ground CNF:")
        println(weightedCNF.groundCnf)
        println("correct wmc = " + twiceSmoothedPropLogWmc.exp)
        println("our wmc = " + twiceSmoothedLiftedLogWmc.exp)
        println("test wmc = " + onceSmoothedLiftedLogWmc.exp)
        println("debug")
        thisTwiceSmoothed.showPDF(domainSizes, predicateWeights, false, file = "bug.nnf")
        throw new VerifyWmcVisitor.VerificationFailedException
      }

    } catch {
      case c2d: C2DError => {
        // C2D refuses: can't verify this node.
      }
    }
  }

}

/**
 * Goes out of memory
 */
protected class BigIntWmc(val decimalPrecision: Int = 100)
    extends NnfVisitor[(DomainSizes, PredicateWeights,
                        WmcVisitor.ParameterMap), BigInt]
    with WmcVisitor {

  val zero: BigInt = 0
  val one: BigInt = 1

  def wmc(nnfs: List[NNFNode], domainSizes: DomainSizes,
          predicateWeights: PredicateWeights): SignLogDouble = {
    val normalization: LogDouble = LogDouble.doubleToLogDouble(decimalPrecision
    ).pow(numRandVars(domainSizes,predicateWeights))
    bigInt2SignLogDouble(
      visit(nnfs.head, (domainSizes, predicateWeights,
                        WmcVisitor.ParameterMap.empty)))/normalization
  }

  def numRandVars(domainSizes: DomainSizes, predicateWeights: PredicateWeights): Int = {
    predicateWeights.predicates.map { _.toAtom.nbGroundings(domainSizes) }.sum
  }

  val LOG2 = Math.log(2.0);

  // see http://stackoverflow.com/questions/6827516/logarithm-for-BigInt
  def bigInt2SignLogDouble(bigint: BigInt): SignLogDouble = {
      var v = bigint
      val blex = v.bitLength - 1022; // any value in 60..1023 is ok
      if (blex > 0)
          v = v >> blex;
      val res = Math.log(v.doubleValue());
      return SignLogDouble.fromLog(if(blex > 0) res + blex * LOG2 else res);
  }

  protected def visitDomainRecursion(
    dr: DomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = dr.domain.size(domainSizes, dr.ineqs)
    if (maxSize < 1) one
    else {
      val groundChildWmc = visit(dr.groundChild.get, params)
      // old inference, linear
      var logWeight = groundChildWmc.pow(maxSize)
      val childchildWmc = visit(dr.mixedChild.get.child.get, params)
      groundChildWmc.pow(maxSize) *
        childchildWmc.pow((maxSize * (maxSize - 1)) / 2)
    }
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, params: (DomainSizes, PredicateWeights,
                                               ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize = idr.domain.size(domainSizes, idr.ineqs)
    if (maxSize < 1) {
      if (WmcVisitor.Verbose)
        println("1 (improved domain recursion, base case)")
      one
    }
    else {
      val childchildWmc = visit(idr.mixedChild.get, params)
      if (WmcVisitor.Verbose)
        println(s"$childchildWmc (improved domain recursion)")
      childchildWmc
    }
  }

  protected def visitExists(
    exists: CountingNode, params: (DomainSizes, PredicateWeights,
                                   ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val maxSize: Int = exists.domain.size(domainSizes, exists.excludedConstants)
    var logWeight = zero;
    for (nbTrue <- 0 to maxSize) {
      val newDomainSizes = (domainSizes
                              + (exists.subdomain, nbTrue)
                              + (exists.subdomain.complement,
                                 (maxSize - nbTrue)));
      val newParams = (newDomainSizes, predicateWeights,
                       parameterMap + (exists -> (nbTrue, maxSize - nbTrue)))
      val childWeight = visit(exists.child.get, newParams)
      val binomialCoeff = coeff(maxSize, nbTrue)
      logWeight += binomialCoeff * childWeight
    }
    logWeight
  }

  protected def visitConstraintRemovalNode(
    cr: ConstraintRemovalNode, params: (DomainSizes, PredicateWeights,
                                        ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val domainSize = cr.domain.size(domainSizes, Set())
    if (domainSize <= 0) 0
    else {
      val newDomainSizes = domainSizes + (cr.subdomain, domainSize - 1) +
        (cr.subdomain.complement, 1)
      val child = visit(cr.child.get,
                        (newDomainSizes, predicateWeights,
                         parameterMap + (cr -> (domainSize - 1, 1))))
      if (WmcVisitor.Verbose)
        println(s"$child (constraint removal)")
      child
    }
  }

  // special cache for bigint factorials (cf. Binomial class)
  private[this] val factorialCache = new collection.mutable.ArrayBuffer[BigInt] ++ List(one, one)

  def factorial(n: Int): BigInt = {
    if (n < factorialCache.length) factorialCache(n)
    else {
      for (i <- factorialCache.length to n) {
        factorialCache += (factorialCache(i - 1) * i)
      }
      factorialCache.last
    }
  }

  def coeff(n: Int, k: Int): BigInt = factorial(n) / factorial(k) / factorial(n - k)


  protected def visitForallNode(
    forall: IndependentPartialGroundingNode,
    params: (DomainSizes, PredicateWeights, ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val childlwmc = visit(forall.child.get, params)
    val nbGroundings = forall.d.size(domainSizes, forall.ineqs)
    if (nbGroundings == 0) {
      one
    } else {
      childlwmc.pow(nbGroundings)
    }
  }

  protected def visitInclusionExclusionNode(
    ie: InclusionExclusion, params: (DomainSizes, PredicateWeights,
                                     ParameterMap)): BigInt = {
    val plus1lwmc = visit(ie.plus1.get, params)
    val plus2lwmc = visit(ie.plus2.get, params)
    val minlwmc = visit(ie.min.get, params)
    plus1lwmc + plus2lwmc - minlwmc
  }

  protected def visitOrNode(
    or: Or, params: (DomainSizes, PredicateWeights, ParameterMap)): BigInt = {
    val llwmcc = visit(or.l.get, params)
    val rlwmcc = visit(or.r.get, params)
    llwmcc + rlwmcc
  }

  protected def visitAndNode(
    and: And, params: (DomainSizes, PredicateWeights, ParameterMap)): BigInt = {
    val llwmcc = visit(and.l.get, params)
    if (llwmcc == zero) zero
    else {
      val rlwmcc = visit(and.r.get, params)
      llwmcc * rlwmcc
    }
  }

  protected def visitRefNode(
    ref: Ref, params: (DomainSizes, PredicateWeights, ParameterMap))
      : BigInt = try {
    val (domainSizes, predicateWeights, parameterMap) = params
    val newDomainSizes = domainSizes.shrink(ref.domainMap, parameterMap)
    val answer = visit(ref.nnfNode.get, (newDomainSizes, predicateWeights, parameterMap))
    if (WmcVisitor.Verbose)
      println(s"$answer (ref)")
    answer
  } catch {
    case e: DomainSize.CantShrinkDomainException => {
      if (WmcVisitor.Verbose)
        println("1 (ref, base case)")
      1
    }
  }

  protected def visitSmoothingNode(
    leaf: SmoothingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    BigInt((decimalPrecision*weights.negWPlusPosWDouble).toInt).pow(nbGroundings)
  }

  protected def visitContradictionLeaf(
    leaf: ContradictionLeaf, params: (DomainSizes, PredicateWeights,
                                      ParameterMap)): BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val hasSolution = leaf.clause.hasConstraintSolution(domainSizes)
    //if the clause has no groundings, it resolves to true
    if (hasSolution) zero else one
  }

  protected def visitUnitLeaf(
    leaf: UnitLeaf, params: (DomainSizes, PredicateWeights, ParameterMap))
      : BigInt = {
    val (domainSizes, predicateWeights, parameterMap) = params
    val weights = predicateWeights(leaf.clause.atom.predicate)
    val nbGroundings = leaf.clause.nbGroundings(domainSizes)
    //if the unit clause has no groundings, it resolves to true
    if (nbGroundings == 0) one
    else if (leaf.positive) BigInt((decimalPrecision*weights.posWDouble).toInt).pow(nbGroundings)
    else BigInt((decimalPrecision*weights.negWDouble).toInt).pow(nbGroundings)
  }

  protected def visitGroundingNode(
    leaf: GroundingNode, params: (DomainSizes, PredicateWeights, ParameterMap))
      : BigInt = {
    throw new UnsupportedOperationException
  }


  protected def visitFalse(params: (DomainSizes, PredicateWeights,
                                    ParameterMap)): BigInt = zero
  protected def visitTrue(params: (DomainSizes, PredicateWeights,
                                   ParameterMap)): BigInt = one

}
