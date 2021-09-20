package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

import edu.ucla.cs.starai.forclift.nnf._

class PostOrderVisitor extends NnfVisitor[Unit, Unit] {

  var visited = Set[NNFNode]()
  var nodeOrder = new ListBuffer[NNFNode]()

  def visit(node: NNFNode): Unit = if (!visited.contains(node)) {
    visited += node
    super.visit(node, ())
    nodeOrder += node
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit): Unit = {
    visit(dr.mixedChild.get)
    visit(dr.groundChild.get)
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, u: Unit): Unit = visit(idr.mixedChild.get)

  protected def visitExists(exists: CountingNode, u: Unit): Unit =
    visit(exists.child.get)

  protected def visitConstraintRemovalNode(cr: ConstraintRemovalNode, u: Unit)
      : Unit = visit(cr.child.get)

  protected def visitForallNode(forall: IndependentPartialGroundingNode,
                                u: Unit): Unit = visit(forall.child.get)

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, u: Unit):
      Unit = {
    visit(ie.plus1.get)
    visit(ie.plus2.get)
    visit(ie.min.get)
  }

  protected def visitOrNode(or: Or, u: Unit): Unit = {
    visit(or.l.get)
    visit(or.r.get)
  }

  protected def visitAndNode(and: And, u: Unit): Unit = {
    visit(and.l.get)
    visit(and.r.get)
  }

  protected def visitRefNode(ref: Ref, u: Unit): Unit = visit(ref.nnfNode.get)

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Unit = ()

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit)
      : Unit = ()

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Unit = ()

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Unit = ()

  protected def visitFalse(u: Unit): Unit = ()

  protected def visitTrue(u: Unit): Unit = ()

}

class DomainsVisitor(val nodes: ListBuffer[NNFNode])
    extends NnfVisitor[Unit, Boolean] {

  def updateDomains: Unit = {
    var changesMade = true
    while (changesMade) {
      changesMade = false
      nodes.foreach { changesMade ^= visit(_, ()) }
    }
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit)
      : Boolean = {
    val newDomains = (dr.mixedChild.get.domains union
                        dr.groundChild.get.domains + dr.domain)
    val returnValue = dr.domains != newDomains
    dr.domains = newDomains
    returnValue
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, u: Unit): Boolean = {
    val newDomains = idr.mixedChild.get.domains + idr.domain
    val returnValue = idr.domains != newDomains
    idr.domains = newDomains
    returnValue
  }

  protected def visitExists(exists: CountingNode, u: Unit): Boolean = {
    val newDomains = (exists.child.get.domains - exists.subdomain -
                        exists.subdomain.complement) + exists.domain
    val returnValue = exists.domains != newDomains
    exists.domains = newDomains
    returnValue
  }

  protected def visitConstraintRemovalNode(cr: ConstraintRemovalNode, u: Unit)
      : Boolean = {
    val newDomains = cr.child.get.domains + cr.domain
    val returnValue = cr.domains != newDomains
    cr.domains = newDomains
    returnValue
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode,
                                u: Unit): Boolean = {
    val newDomains = forall.child.get.domains + forall.d
    val returnValue = forall.domains != newDomains
    forall.domains = forall.child.get.domains + forall.d
    returnValue
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, u: Unit):
      Boolean = {
    val newDomains = (ie.plus1.get.domains union ie.plus2.get.domains union
                        ie.min.get.domains)
    val returnValue = ie.domains != newDomains
    ie.domains = newDomains
    returnValue
  }

  protected def visitOrNode(or: Or, u: Unit): Boolean = {
    val newDomains = or.l.get.domains union or.r.get.domains
    val returnValue = or.domains != newDomains
    or.domains = newDomains
    returnValue
  }

  protected def visitAndNode(and: And, u: Unit): Boolean = {
    val newDomains = and.l.get.domains union and.r.get.domains
    val returnValue = and.domains != newDomains
    and.domains = newDomains
    returnValue
  }

  protected def visitRefNode(ref: Ref, u: Unit): Boolean = {
    val returnValue = ref.domains != ref.nnfNode.get.domains
    ref.domains = ref.nnfNode.get.domains
    returnValue
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Boolean =
    false

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit)
      : Boolean = false

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Boolean = false

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Boolean =
    false

  protected def visitFalse(u: Unit): Boolean = false

  protected def visitTrue(u: Unit): Boolean = false

}

class SmoothingVariablesVisitor(val nodes: ListBuffer[NNFNode]) extends
    NnfVisitor[Unit, Boolean] {

  def updateVariables: Unit = {
    var changesMade = true
    println("Smoothing started")
    while (changesMade) {
      changesMade = false
      nodes.foreach { changesMade ^= visit(_, ()) }
    }
    println("Smoothing finished")
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit)
      : Boolean = {
    val ungroundedMixedChildvars = dr.mixedChild.get.variablesForSmoothing.map
    { _.inverseSubstitution(dr.c, dr.ineqs, dr.domain) }
    val ungroundedGroundChildVars = dr.groundChild.get.variablesForSmoothing.
      map { _.inverseSubstitution(dr.c, dr.ineqs, dr.domain) }
    val allVars = ungroundedMixedChildvars ++ ungroundedGroundChildVars
    val returnValue = dr.variablesForSmoothing != allVars
    dr.variablesForSmoothing = allVars
    println("domain recursion: " + returnValue)
    returnValue
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, u: Unit): Boolean = {
    val allVars = idr.mixedChild.get.variablesForSmoothing.map {
      _.inverseSubstitution(idr.c, idr.ineqs, idr.domain) }
    val returnValue = idr.variablesForSmoothing != allVars
    if (returnValue)
      println("visitImprovedDomainRecursion: replacing " +
                idr.variablesForSmoothing + " with " + allVars)
    idr.variablesForSmoothing = allVars
    println("improved domain recursion: " + returnValue)
    returnValue
  }

  protected def visitExists(exists: CountingNode, u: Unit): Boolean = {
    val countedSubdomainParents = NNFNode.removeSubsumed(
      exists.child.get.variablesForSmoothing.map {
        _.reverseDomainSplitting(exists.domain, exists.subdomain) })
    val returnValue = exists.variablesForSmoothing != countedSubdomainParents
    exists.variablesForSmoothing = countedSubdomainParents
    println("exists/counting: " + returnValue)
    returnValue
  }

  protected def visitConstraintRemovalNode(cr: ConstraintRemovalNode, u: Unit)
      : Boolean = {
    val countedSubdomainParents = NNFNode.removeSubsumed(
      cr.child.get.variablesForSmoothing.map {
        _.reverseDomainSplitting(cr.domain, cr.subdomain) })
    val returnValue = cr.variablesForSmoothing != countedSubdomainParents
    cr.variablesForSmoothing = countedSubdomainParents
    println("constraint removal: " + returnValue)
    returnValue
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode,
                                u: Unit): Boolean = {
    val ungroundedChildVars = forall.child.get.variablesForSmoothing.map {
      _.inverseSubstitution(forall.c, forall.ineqs, forall.d) }

    val returnValue = forall.variablesForSmoothing != ungroundedChildVars
    forall.variablesForSmoothing = ungroundedChildVars
    println("forall / independent partial grounding: " + returnValue)
    returnValue
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, u: Unit):
      Boolean = {
    val plus1Missing = NNFNode.removeSubsumed(
      ie.plus2.get.variablesForSmoothing union
        ie.min.get.variablesForSmoothing).flatMap {
      _.minus(ie.plus1.get.variablesForSmoothing) }
    val plus2Missing = NNFNode.removeSubsumed(
      ie.plus1.get.variablesForSmoothing union
        ie.min.get.variablesForSmoothing).flatMap {
      _.minus(ie.plus2.get.variablesForSmoothing) }
    val minMissing = NNFNode.removeSubsumed(
      ie.plus1.get.variablesForSmoothing union
        ie.plus2.get.variablesForSmoothing).flatMap {
      _.minus(ie.min.get.variablesForSmoothing) }
    val plus1VarsAll = ie.plus1.get.variablesForSmoothing union plus1Missing
    val plus2VarsAll = ie.plus2.get.variablesForSmoothing union plus2Missing
    val minVarsAll = ie.min.get.variablesForSmoothing union minMissing
    val bestOf2 = if (plus1VarsAll.size < plus2VarsAll.size) plus1VarsAll
                  else plus2VarsAll
    val bestOf3 = if (minVarsAll.size < bestOf2.size) minVarsAll else bestOf2
    val returnValue = ie.variablesForSmoothing != bestOf3
    ie.variablesForSmoothing = bestOf3
    println("inclusion-exclusion: " + returnValue)
    returnValue
  }

  protected def visitOrNode(or: Or, u: Unit): Boolean = {
    val lMissing = or.r.get.variablesForSmoothing.flatMap {
      _.minus(or.l.get.variablesForSmoothing) }
    val rMissing = or.l.get.variablesForSmoothing.flatMap {
      _.minus(or.r.get.variablesForSmoothing) }
    val lVarsAll = or.l.get.variablesForSmoothing union lMissing
    val rVarsAll = or.r.get.variablesForSmoothing union rMissing
    val thisVars = if (lVarsAll.size < rVarsAll.size) lVarsAll else rVarsAll
    val returnValue = or.variablesForSmoothing != thisVars
    or.variablesForSmoothing = thisVars
    println("or: " + returnValue)
    returnValue
  }

  protected def visitAndNode(and: And, u: Unit): Boolean = {
    val thisVars = (and.l.get.variablesForSmoothing union
                      and.r.get.variablesForSmoothing)
    val returnValue = and.variablesForSmoothing != thisVars
    and.variablesForSmoothing = thisVars
    println("and: " + returnValue + ". before: " + and.variablesForSmoothing + ", after: " + thisVars)
    returnValue
  }

  protected def visitRefNode(ref: Ref, u: Unit): Boolean = {
    val returnValue = ref.variablesForSmoothing !=
      ref.nnfNode.get.variablesForSmoothing
    ref.variablesForSmoothing = ref.nnfNode.get.variablesForSmoothing
    println("ref: " + returnValue)
    returnValue
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Boolean =
    false

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit)
      : Boolean = false

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Boolean = false

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Boolean =
    false

  protected def visitFalse(u: Unit): Boolean = false

  protected def visitTrue(u: Unit): Boolean = false

}
