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

  lazy val smooth = {
    child match {
      case Some(child) => {
        val (childSmoothed, childvars) = child.smooth
        val ungroundedChildVars = childvars.map { _.inverseSubstitution(c, ineqs, d) }
        (new IndependentPartialGroundingNode(cnf, Some(childSmoothed), c,
                                             ineqs, d, explanation),
         ungroundedChildVars)
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    child match {
      case Some(child) => {
        new IndependentPartialGroundingNode(
          cnf, Some(child.condition(pos, neg)), c, ineqs, d, explanation)
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def size = {
    child match {
      case Some(child) => child.size + 1
      case None => throw new Exception("you forgot to call update()")
    }
  }

  lazy val domains = {
    child match {
      case Some(child) => child.domains + d
      case None => throw new Exception("you forgot to call update()")
    }
  }

  lazy val evalOrder = {
    child match {
      case Some(child) => child.evalOrder
      case None => throw new Exception("you forgot to call update()")
    }
  }

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false,
    depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    child match {
      case Some(child) => {
        if (depth >= maxDepth) cutoff(nameSpace, compact)
        else {
          val (nl, el) = child.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
          val subscript = ((c.toString + """ \in """ + d) :: ineqs.map { c.toString + """ \neq """ + _.toString }.toList).mkString(""" \land """)
          val myNodes = if (compact) {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
          } else {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
              "  " + "exp" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigforall{""" + c + """}{""" + subscript + """}$", shape=circle];""" + "\n"
          }
          val myEdges = if (compact) {
            "  " + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"
          } else {
            val wmcVisitor = new SafeSignLogDoubleWmc
            val childlwmc = wmcVisitor.visit(child.smooth._1, (domainSizes, predicateWeights))
            "  " + getName(nameSpace) + " -> " + "exp" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
              "  " + "exp" + getName(nameSpace) + " -> " + child.getName(nameSpace) + """ [""" + edgeLabel(" $ " + childlwmc.exp + " $ ") + """];""" + "\n"
          }
          val nodes = (myNodes + nl)
          val edges = (myEdges + el)
          (nodes, edges)
        }
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    child match {
      case Some(child) => {
        (super.toString(nameSpace) +
           getName(nameSpace) + " = " + child.getName(nameSpace) + " ^ |" + d + "|" + ineqs.map { """X \neq """ + _.toString }.mkString(" , ") + "\n" +
           "\n" +
           child.toString(nameSpace))
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

}

class CountingNode(val cnf: CNF, var child: Option[NNFNode],
  val domain: Domain, val subdomain: SubDomain,
  val explanation: String = "") extends NNFNode {

  override def update(children : List[NNFNode]) = {
    require(children.length == 1)
    child = Some(children.head)
  }

  def size = {
    child match {
      case Some(child) => child.size + 1
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def excludedConstants = subdomain.excludedConstants

  lazy val domains = {
    child match {
      case Some(child) => (child.domains - subdomain - subdomain.complement) + domain
      case None => throw new Exception("you forgot to call update()")
    }
  }

  lazy val evalOrder = {
    child match {
      case Some(child) => child.evalOrder + 1
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def smooth = {
    child match {
      case Some(child) => {
        val (childSmooth, childVars) = child.smooth
        // this is fine, but does not mean the result will be non-overlapping
        // two catoms might overlap but not one subsumes the other
        val countedSubdomainParents = removeSubsumed(childVars.map { _.reverseDomainSplitting(domain, subdomain) })
        val disjCounted = makeDisjoint(countedSubdomainParents.toList)
        val childMissing = disjCounted.flatMap { _.minus(childVars) }
        val childSmoothAll = childSmooth.smoothWith(childMissing.toSet)
        val thisSmoothed = new CountingNode(cnf, Some(childSmoothAll), domain, subdomain, explanation)
        (thisSmoothed, countedSubdomainParents)
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    child match {
      case Some(child) => new CountingNode(cnf, Some(child.condition(pos, neg)), domain, subdomain, explanation)
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def minDomainSizeString = "|" + subdomain + "| = 0"
  def maxDomainSizeString = "|" + domain + "|"

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    child match {
      case Some(child) => {
        if (depth >= maxDepth) cutoff(nameSpace, compact)
        else {
          val (nl, el) = child.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
          val myNodes = if (compact) {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
          } else {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
              "  " + "count" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\displaystyle\bigexists{""" + subdomain + """}{ """ + subdomain + """ \subseteq """ + domain + """}$", shape=circle];""" + "\n"
          }
          val myEdges = if (compact) {
            "  " + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"

          } else {
            "  " + getName(nameSpace) + " -> " + "count" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
              "  " + "count" + getName(nameSpace) + " -> " + child.getName(nameSpace) + ";\n"
          }
          val nodes = (myNodes + nl)
          val edges = (myEdges + el)
          (nodes, edges)
        }
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    child match {
      case Some(child) => {
        (super.toString(nameSpace) +
           getName(nameSpace) + " = count " + subdomain + " from " + domain + " " + child.getName(nameSpace) + "\n" +
           "\n" +
           child.toString(nameSpace))
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

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

  def size = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => mixedChild.size + groundChild.size + 1
      case _ => throw new Exception("you forgot to call update()")
    }
  }

  // assumptions to speed up inference
  // require(mixedChild.child.cnf.isGround)

  lazy val domains = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => mixedChild.domains union groundChild.domains + domain
      case _ => throw new Exception("you forgot to call update()")
    }
  }

  lazy val evalOrder = {
    mixedChild match {
      case Some(mixedChild) => mixedChild.evalOrder // assume constant eval
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def smooth = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => {
        val (mixedChildSmoothed, mixedChildVars) = mixedChild.smooth
        val (groundChildSmoothed, groundChildVars) = groundChild.smooth
        val ungroundedMixedChildvars = mixedChildVars.map { _.inverseSubstitution(c, ineqs, domain) }
        val ungroundedGroundChildVars = groundChildVars.map { _.inverseSubstitution(c, ineqs, domain) }
        val allVars = ungroundedMixedChildvars ++ ungroundedGroundChildVars
        (new DomainRecursionNode(cnf, Some(mixedChildSmoothed), Some(groundChildSmoothed),
                                 c, ineqs, domain, explanation), allVars)
      }
      case _ => throw new Exception("you forgot to call update()")
    }
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => {
        new DomainRecursionNode(
          cnf,
          Some(mixedChild.condition(pos, neg).asInstanceOf[IndependentPartialGroundingNode]),
          Some(groundChild.condition(pos, neg)),
          c, ineqs, domain, explanation)
      }
      case _ => throw new Exception("you forgot to call update()")
    }
  }

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => {
        if (depth >= maxDepth) cutoff(nameSpace, compact)
        else {
          val (n1, e1) = mixedChild.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
          val (n2, e2) = groundChild.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
          val myNodes = if (compact) {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
          } else {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
          }
          val myEdges = if (compact) {
            "  " + getName(nameSpace) + " -> " + mixedChild.getName(nameSpace) + ";\n" +
              "  " + getName(nameSpace) + " -> " + groundChild.getName(nameSpace) + ";\n" +
              "  " + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
          } else {
            val wmcVisitor = new SafeSignLogDoubleWmc
            val groundChildWmc = wmcVisitor.visit(groundChild.smooth._1,(domainSizes, predicateWeights))
            val mixedChildWmc = wmcVisitor.visit(mixedChild.smooth._1,(domainSizes - domain, predicateWeights))
            "  " + getName(nameSpace) + " -> " + "domainrec" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + " -> " + mixedChild.getName(nameSpace) + """ [""" + edgeLabel(" $ " + mixedChildWmc.exp + " $ ") + """];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + " -> " + groundChild.getName(nameSpace) + """ [""" + edgeLabel(" $ " + groundChildWmc.exp + " $ ") + """];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
          }
          val nodes = (myNodes + n1 + n2)
          val edges = (myEdges + e1 + e2)
          (nodes, edges)
        }
      }
      case _ => throw new Exception("you forgot to call update()")
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (mixedChild, groundChild) match {
      case (Some(mixedChild), Some(groundChild)) => {
        (super.toString(nameSpace) +
           getName(nameSpace) + " = domainrec " + c + " from " + domain + " " + mixedChild.getName(nameSpace) + " " + groundChild.getName(nameSpace) + "\n" +
           "\n" +      "\n" +
           mixedChild.toString(nameSpace) +
           "\n" +
           groundChild.toString(nameSpace))
      }
      case _ => throw new Exception("you forgot to call update()")
    }
  }

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

  lazy val domains = mixedChild match {
    case Some(mc) => mc.domains + domain
    case None => Set(domain)
  }

  lazy val evalOrder = mixedChild match {
    case Some(c) => c.evalOrder // assume constant eval
    case None => throw new Exception("you forgot to call update()")
  }

  lazy val smooth = {
    mixedChild match {
      case Some(mc) => {
        val (mixedChildSmoothed, mixedChildVars) = mc.smooth
        val ungroundedMixedChildvars = mixedChildVars.map { _.inverseSubstitution(c, ineqs, domain) }
        val allVars = ungroundedMixedChildvars
        (new ImprovedDomainRecursionNode(cnf, Some(mixedChildSmoothed), c, ineqs, domain, explanation), allVars)
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  def condition(pos: Set[Atom], neg: Set[Atom]) =
    mixedChild match {
      case Some(mc) => new ImprovedDomainRecursionNode(cnf, Some(mc.condition(pos, neg)), c, ineqs, domain, explanation)
      case None => throw new Exception("you forgot to call update()")
    }

  override def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
                         nameSpace: NameSpace[NNFNode, String], compact: Boolean = false, depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String) = {
    mixedChild match {
      case Some(mc) => {
        if (depth >= maxDepth) cutoff(nameSpace, compact)
        else {
          val (n1, e1) = mc.toDotNode(domainSizes, predicateWeights, nameSpace, compact, depth + 1, maxDepth)
          val myNodes = if (compact) {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
          } else {
            "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\land$", shape=circle];""" + "\n"
          }
          val myEdges = if (compact) {
            "  " + getName(nameSpace) + " -> " + mc.getName(nameSpace) + ";\n" +
              "  " + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
          } else {
            val wmcVisitor = new SafeSignLogDoubleWmc
            val mixedChildWmc = wmcVisitor.visit(mc.smooth._1,(domainSizes - domain, predicateWeights))
            "  " + getName(nameSpace) + " -> " + "domainrec" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + " -> " + mc.getName(nameSpace) + """ [""" + edgeLabel(" $ " + mixedChildWmc.exp + " $ ") + """];""" + "\n" +
              "  " + "domainrec" + getName(nameSpace) + " -> " + getName(nameSpace) + """ [""" + edgeLabel("$" + domain + """ \leftarrow """ + domain + """ \setminus \{""" + c + """\}$""") + """];""" + "\n"
          }
          val nodes = (myNodes + n1)
          val edges = (myEdges + e1)
          (nodes, edges)
        }
      }
      case None => throw new Exception("you forgot to call update()")
    }
  }

  override def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    mixedChild match {
      case Some(mc) => (super.toString(nameSpace) + getName(nameSpace) + " = domainrec " + c + " from " + domain + " " + mc.getName(nameSpace) + "\n" + "\n" + mc.toString(nameSpace))
      case None => throw new Exception("you forgot to call update()")
    }
  }

}
