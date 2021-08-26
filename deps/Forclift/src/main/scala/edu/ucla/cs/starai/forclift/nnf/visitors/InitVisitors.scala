package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

import edu.ucla.cs.starai.forclift.nnf._

class PostOrderVisitor extends NnfVisitor[Unit, Unit] {

  var visited = Set[NNFNode]()
  var nodeOrder = new ListBuffer[NNFNode]()

  def visit(node: NNFNode): Unit = {
    if (!visited.contains(node)) {
      visited += node
      super.visit(node, ())
      nodeOrder += node
    }
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit): Unit =
    (dr.mixedChild, dr.groundChild) match {
    case (Some(mixedChild), Some(groundChild)) => {
      visit(mixedChild)
      visit(groundChild)
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, u: Unit): Unit = idr.mixedChild match {
    case Some(mixedChild) => visit(mixedChild)
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitExists(exists: CountingNode, u: Unit): Unit = exists.child match {
    case Some(child) => visit(child)
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode, u: Unit): Unit = forall.child match {
    case Some(child) => visit(child)
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, u: Unit):
      Unit = (ie.plus1, ie.plus2, ie.min) match {
    case (Some(plus1), Some(plus2), Some(min)) => {
      visit(plus1)
      visit(plus2)
      visit(min)
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitOrNode(or: Or, u: Unit): Unit = (or.l, or.r) match {
    case (Some(l), Some(r)) => {
      visit(l)
      visit(r)
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitAndNode(and: And, u: Unit): Unit = (and.l, and.r) match {
    case (Some(l), Some(r)) => {
      visit(l)
      visit(r)
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitRefNode(ref: Ref, u: Unit): Unit = ref.nnfNode match {
    case Some(nnfNode) => visit(nnfNode)
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Unit = ()

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit): Unit = ()

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Unit = ()

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Unit = ()

  protected def visitFalse(u: Unit): Unit = ()

  protected def visitTrue(u: Unit): Unit = ()

}

class DomainsVisitor(val nodes: ListBuffer[NNFNode]) extends NnfVisitor[Unit, Boolean] {

  def updateDomains: Unit = {
    var changesMade = true
    while (changesMade) {
      changesMade = false
      nodes.foreach { changesMade ^= visit(_, ()) }
    }
  }

  protected def visitDomainRecursion(dr: DomainRecursionNode, u: Unit): Boolean =
    (dr.mixedChild, dr.groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => {
        val newDomains = mixedChild.domains union groundChild.domains + dr.domain
        val returnValue = dr.domains != newDomains
        dr.domains = newDomains
        returnValue
      }
      case _ => throw new Exception("you forgot to call update()")
    }

  protected def visitImprovedDomainRecursion(
    idr: ImprovedDomainRecursionNode, u: Unit): Boolean = idr.mixedChild match {
    case Some(mixedChild) => {
      val newDomains = mixedChild.domains + idr.domain
      val returnValue = idr.domains != newDomains
      idr.domains = newDomains
      returnValue
    }
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitExists(exists: CountingNode, u: Unit): Boolean = exists.child match {
    case Some(child) => {
      val newDomains = (child.domains - exists.subdomain - exists.subdomain.complement) + exists.domain
      val returnValue = exists.domains != newDomains
      exists.domains = newDomains
      returnValue
    }
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitForallNode(forall: IndependentPartialGroundingNode, u: Unit): Boolean = forall.child match {
    case Some(child) => {
      val newDomains = child.domains + forall.d
      val returnValue = forall.domains != newDomains
      forall.domains = child.domains + forall.d
      returnValue
    }
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitInclusionExclusionNode(ie: InclusionExclusion, u: Unit):
      Boolean = (ie.plus1, ie.plus2, ie.min) match {
    case (Some(plus1), Some(plus2), Some(min)) => {
      val newDomains = plus1.domains union plus2.domains union min.domains
      val returnValue = ie.domains != newDomains
      ie.domains = newDomains
      returnValue
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitOrNode(or: Or, u: Unit): Boolean = (or.l, or.r) match {
    case (Some(l), Some(r)) => {
      val newDomains = l.domains union r.domains
      val returnValue = or.domains != newDomains
      or.domains = newDomains
      returnValue
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitAndNode(and: And, u: Unit): Boolean = (and.l, and.r) match {
    case (Some(l), Some(r)) => {
      val newDomains = l.domains union r.domains
      val returnValue = and.domains != newDomains
      and.domains = newDomains
      returnValue
    }
    case _ => throw new Exception("you forgot to call update()")
  }

  protected def visitRefNode(ref: Ref, u: Unit): Boolean = ref.nnfNode match {
    case Some(nnfNode) => {
      val returnValue = ref.domains != nnfNode.domains
      ref.domains = nnfNode.domains
      returnValue
    }
    case None => throw new Exception("you forgot to call update()")
  }

  protected def visitSmoothingNode(leaf: SmoothingNode, u: Unit): Boolean = false

  protected def visitContradictionLeaf(leaf: ContradictionLeaf, u: Unit): Boolean = false

  protected def visitUnitLeaf(leaf: UnitLeaf, u: Unit): Boolean = false

  protected def visitGroundingNode(leaf: GroundingNode, u: Unit): Boolean = false

  protected def visitFalse(u: Unit): Boolean = false

  protected def visitTrue(u: Unit): Boolean = false

}
