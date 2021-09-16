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

package edu.ucla.cs.starai.forclift.nnf

import scala.collection._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.nnf.visitors.SafeSignLogDoubleWmc
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries

class IndependentPartialGroundingNode(val cnf: CNF, var child: Option[NNFNode],
                                      val c: Constant,
                                      val ineqs: Set[Constant], val d: Domain,
                                      val explanation: String = "")
    extends NNFNode {

  override def update(children : List[NNFNode]) = {
    require(children.length == 1)
    child = Some(children.head)
  }

  lazy val smooth =
    if (NNFNode.smoothingCache.contains(this)) {
      NNFNode.smoothingCache(this)
    } else {
      val newNode = new IndependentPartialGroundingNode(cnf, None, c, ineqs,
                                                        d, explanation)
      NNFNode.smoothingCache(this) = newNode
      newNode.update(List(child.get.smooth))
      newNode
    }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new IndependentPartialGroundingNode(
      cnf, Some(child.get.condition(pos, neg)), c, ineqs, d, explanation)
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  def size = child.get.size + 1

  var domains = Set(d)

  lazy val evalOrder = child.get.evalOrder

  override def toDotNode(domainSizes: DomainSizes,
                         predicateWeights: PredicateWeights,
                         nameSpace: NameSpace[NNFNode, String],
                         compact: Boolean = false, depth: Int,
                         maxDepth: Int = Integer.MAX_VALUE): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.get.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val subscript = ((c.toString + """ \in """ + d) :: ineqs.map { c.toString + """ \neq """ + _.toString }.toList).mkString(""" \land """)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "exp" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(nameSpace) + ";\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val childlwmc = wmcVisitor.visit(child.get.smooth, (domainSizes, predicateWeights))
        "  " + getName(nameSpace) + " -> " + "exp" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "exp" + getName(nameSpace) + " -> " + child.get.getName(nameSpace) + """ [""" + edgeLabel(" $ " + childlwmc.exp + " $ ") + """];""" + "\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
       getName(nameSpace) + " = " + child.get.getName(nameSpace) + " ^ |" + d + "|" + ineqs.map { """X \neq """ + _.toString }.mkString(" , ") + "\n" +
       "\n" +
       child.get.toString(nameSpace))

}

class CountingNode(val cnf: CNF, var child: Option[NNFNode],
  val domain: Domain, val subdomain: SubDomain,
  val explanation: String = "") extends NNFNode {

  override def update(children : List[NNFNode]) = {
    require(children.length == 1)
    child = Some(children.head)
  }

  def size = child.get.size + 1

  def excludedConstants = subdomain.excludedConstants

  var domains = Set(domain)

  lazy val evalOrder = child.get.evalOrder + 1

  def smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new CountingNode(cnf, None, domain, subdomain, explanation)
    NNFNode.smoothingCache(this) = newNode
    // this is fine, but does not mean the result will be non-overlapping
    // two catoms might overlap but not one subsumes the other
    val countedSubdomainParents = NNFNode.removeSubsumed(
      child.get.variablesForSmoothing.map {
        _.reverseDomainSplitting(domain, subdomain) })
    val disjCounted = makeDisjoint(countedSubdomainParents.toList)
    val childMissing = disjCounted.flatMap {
      _.minus(child.get.variablesForSmoothing) }
    val childSmoothAll = child.get.smooth.smoothWith(childMissing.toSet)
    newNode.update(List(childSmoothAll))
    newNode
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new CountingNode(cnf, Some(child.get.condition(pos, neg)),
                                       domain, subdomain, explanation)
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  def minDomainSizeString = "|" + subdomain + "| = 0"
  def maxDomainSizeString = "|" + domain + "|"

  override def toDotNode(domainSizes: DomainSizes,
                         predicateWeights: PredicateWeights,
                         nameSpace: NameSpace[NNFNode, String],
                         compact: Boolean = false, depth: Int,
                         maxDepth: Int = Integer.MAX_VALUE): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (nl, el) = child.get.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "count" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + child.get.getName(nameSpace) + ";\n"

      } else {
        "  " + getName(nameSpace) + " -> " + "count" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "count" + getName(nameSpace) + " -> " + child.get.getName(nameSpace) + ";\n"
      }
      val nodes = (myNodes + nl)
      val edges = (myEdges + el)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
       getName(nameSpace) + " = count " + subdomain + " from " + domain + " " + child.get.getName(nameSpace) + "\n" +
       "\n" +
       child.get.toString(nameSpace))

}

class DomainRecursionNode(
  val cnf: CNF, var mixedChild: Option[IndependentPartialGroundingNode],
  var groundChild: Option[NNFNode], val c: Constant, val ineqs: Set[Constant],
  val domain: Domain, val explanation: String = "") extends NNFNode {

  override def update(children : List[NNFNode]) = {
    require(children.length == 2)
    mixedChild = Some(children.head.asInstanceOf[IndependentPartialGroundingNode])
    groundChild = Some(children.tail.head)
  }

  def size = mixedChild.get.size + groundChild.get.size + 1

  // assumptions to speed up inference
  // require(mixedChild.child.cnf.isGround)

  var domains = Set(domain)

  lazy val evalOrder = mixedChild.get.evalOrder // assume constant eval

  def smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new DomainRecursionNode(cnf, None, None, c, ineqs,
                                          domain, explanation)
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(mixedChild.get.smooth, groundChild.get.smooth))
    newNode
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new DomainRecursionNode(
      cnf,
      Some(mixedChild.get.condition(pos, neg).
             asInstanceOf[IndependentPartialGroundingNode]),
      Some(groundChild.get.condition(pos, neg)),
      c, ineqs, domain, explanation)
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  override def toDotNode(domainSizes: DomainSizes,
                         predicateWeights: PredicateWeights,
                         nameSpace: NameSpace[NNFNode, String],
                         compact: Boolean = false, depth: Int,
                         maxDepth: Int = Integer.MAX_VALUE): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (n1, e1) = mixedChild.get.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val (n2, e2) = groundChild.get.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + mixedChild.get.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + groundChild.get.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val groundChildWmc = wmcVisitor.visit(groundChild.get.smooth,(domainSizes, predicateWeights))
        val mixedChildWmc = wmcVisitor.visit(mixedChild.get.smooth,(domainSizes - domain, predicateWeights))
        "  " + getName(nameSpace) + " -> " + "domainrec" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.get.getName(nameSpace) + """ [""" + edgeLabel(" $ " + mixedChildWmc.exp + " $ ") + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + groundChild.get.getName(nameSpace) + """ [""" + edgeLabel(" $ " + groundChildWmc.exp + " $ ") + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      }
      val nodes = (myNodes + n1 + n2)
      val edges = (myEdges + e1 + e2)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) +
       getName(nameSpace) + " = domainrec " + c + " from " + domain + " " + mixedChild.get.getName(nameSpace) + " " + groundChild.get.getName(nameSpace) + "\n" +
       "\n" +      "\n" +
       mixedChild.get.toString(nameSpace) +
       "\n" +
       groundChild.get.toString(nameSpace))

}

class ImprovedDomainRecursionNode(val cnf: CNF, var mixedChild: Option[NNFNode],
                                  val c: Constant, val ineqs: Set[Constant],
                                  val domain: Domain,
                                  val explanation: String = "") extends NNFNode {

  override def update(children : List[NNFNode]) = {
    require(children.length == 1)
    mixedChild = Some(children.head)
  }

  def size = mixedChild.size + 1

  var domains = Set(domain)

  lazy val evalOrder = mixedChild.get.evalOrder // assume constant eval

  lazy val smooth = if (NNFNode.smoothingCache.contains(this)) {
    NNFNode.smoothingCache(this)
  } else {
    val newNode = new ImprovedDomainRecursionNode(cnf, None, c, ineqs,
                                                  domain, explanation)
    NNFNode.smoothingCache(this) = newNode
    newNode.update(List(mixedChild.get.smooth))
    newNode
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    val returnValue = new ImprovedDomainRecursionNode(
      cnf, Some(mixedChild.get.condition(pos, neg)), c, ineqs, domain,
      explanation)
    NNFNode.conditionCache((this, pos, neg)) = returnValue
    returnValue
  }

  override def toDotNode(domainSizes: DomainSizes,
                         predicateWeights: PredicateWeights,
                         nameSpace: NameSpace[NNFNode, String],
                         compact: Boolean = false, depth: Int,
                         maxDepth: Int = Integer.MAX_VALUE): (String, String) =
    if (depth >= maxDepth) cutoff(nameSpace, compact)
    else {
      val (n1, e1) = mixedChild.get.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
      val myNodes = if (compact) {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      } else {
        "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
      }
      val myEdges = if (compact) {
        "  " + getName(nameSpace) + " -> " + mixedChild.get.getName(nameSpace) + ";\n" +
          "  " + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      } else {
        val wmcVisitor = new SafeSignLogDoubleWmc
        val mixedChildWmc = wmcVisitor.visit(mixedChild.get.smooth,(domainSizes - domain, predicateWeights))
        "  " + getName(nameSpace) + " -> " + "domainrec" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.get.getName(nameSpace) + """ [""" + edgeLabel(" $ " + mixedChildWmc.exp + " $ ") + """];""" + "\n" +
          "  " + "domainrec" + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
      }
      val nodes = (myNodes + n1)
      val edges = (myEdges + e1)
      (nodes, edges)
    }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String =
    (super.toString(nameSpace) + getName(nameSpace) + " = domainrec " + c +
       " from " + domain + " " + mixedChild.get.getName(nameSpace) + "\n" +
       "\n" + mixedChild.get.toString(nameSpace))

}
