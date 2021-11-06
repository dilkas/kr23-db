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

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf.visitors._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.util.Binomial._
import edu.ucla.cs.starai.forclift.util.ExternalBinaries
import scala.sys.process._
import System._
import collection.mutable.ListBuffer
import constraints._
import java.io._
import breeze.math._

class NNFNameSpace extends NameSpace[NNFNode, String] {

  var lastUsedVar = -1;

  protected def createName(node: NNFNode) = {
    lastUsedVar += 1
    "n" + lastUsedVar //("x"*lastUsedVar)
  }
}

object Cachestats {

  var hits = 0

  var miss = 0

  type Key = (NNFNode, DomainSizes)
  type WMCCache = collection.mutable.Map[Key, Double]

  def cacheTried(key: Key, cache: WMCCache) {
    if (cache.contains(key)) hits += 1
    else miss += 1
    if ((hits + miss) % 1000 == 0) {
      println("NNF eval cache hit rate = " + (hits * 100.0 / (hits + miss)) + "%")
    }
  }

}

object NNFNode {

  def removeSubsumed(clauses: Set[PositiveUnitClause]) = {
    val clauses2 = clauses.toList.filter { clause1 =>
      !clauses.exists { clause2 =>
        (clause1 ne clause2) && clause2.subsumes(clause1)
      }
    }
    def removeDoubles(clauses: List[PositiveUnitClause]): List[PositiveUnitClause] = clauses match {
      case clause :: rest => clause :: removeDoubles(rest.filter { clause independent _ })
      case Nil => Nil
    }
    removeDoubles(clauses2).toSet
  }

  val conditionCache = new mutable.HashMap[(NNFNode, Set[Atom], Set[Atom]),
                                           NNFNode]
  val smoothingCache = new mutable.HashMap[NNFNode, NNFNode]
  val updateCache = new mutable.HashMap[NNFNode, NNFNode]

}

abstract class NNFNode(var variablesForSmoothing: Set[PositiveUnitClause] =
                         Set[PositiveUnitClause]()) {

  def useCache(function: NNFNode => NNFNode,
               cache: mutable.HashMap[NNFNode, NNFNode],
               constructor: => NNFNode): NNFNode =
    if (cache.contains(this)) {
      cache(this)
    } else {
      val newNode: NNFNode = constructor
      cache(this) = newNode
      newNode.update(directSuccessors.map(_.map(function)))
      newNode
    }

  def simpleClone: NNFNode

  def myClone: NNFNode = useCache(_.myClone, NNFNode.updateCache, simpleClone)

  def cnf: CNF

  def directSuccessors: List[Option[NNFNode]] = List[Option[NNFNode]]()

  def explanation: String

  /** Set the outedges to point to children. The length of the list must match
    the outdegree. */
  def update(children: List[Option[NNFNode]]) = ()

  def updateFirst(child: NNFNode): Boolean = false

  /** Modify the circuit to insert newNode */
  def addNode(newNode: NNFNode): Boolean = {
    // first try to add the new node as a direct successor of this node
    if (updateFirst(newNode)) {
      println("addNode: trying to add " + newNode.getClass.getSimpleName +
                " as a successor of " + getClass.getSimpleName + ": true")
      true
    } else {
      println("addNode: trying to add " + newNode.getClass.getSimpleName +
                " as a successor of " + getClass.getSimpleName + ": false")
      directSuccessors.foreach {
        case Some(node) => {
          println("addNode: recursing from " + this.getClass.getSimpleName
                    + " to " + node.getClass.getSimpleName)
          if (node.addNode(newNode)) return true
        }
      }
      false
    }
  }

  def size: Int

  def makeDisjoint(catoms: List[PositiveUnitClause]): List[PositiveUnitClause] = catoms match {
    case Nil => Nil
    case catom :: rest => catom :: makeDisjoint(rest.flatMap { _.minus(catom) })
  }

  def smoothWithPredicates(predicates: Set[Predicate],
                           excluded: Set[PositiveUnitClause] = Set.empty): NNFNode = {
    // initialise variables for smoothing
    val postOrderVisitor = new PostOrderVisitor
    postOrderVisitor.visit(this)
    val smoothingVariablesVisitor = new SmoothingVariablesVisitor(
      postOrderVisitor.nodeOrder)
    smoothingVariablesVisitor.updateVariables

    val thisSmoothed = smooth
    val allVars = predicates.map { _.toAtom }
    val missing = allVars.flatMap { _.minus(variablesForSmoothing union excluded) }
    thisSmoothed.smoothWith(makeDisjoint(missing.toList).toSet)
  }

  def smooth: NNFNode

  // assumes atoms are disjoint
  def smoothWith(atoms: Set[PositiveUnitClause]): NNFNode = atoms.foldLeft(this) { (branch, clause) =>
    assume(atoms.forall { atom1 => atoms.forall { atom2 => (atom1 eq atom2) || atom1.independent(atom2) } })
    new And(branch.cnf, Some(new SmoothingNode(clause)), Some(branch), "Smoothing of $" + clause.toLatex() + "$.")
  }

  def condition(pos: Set[Atom], neg: Set[Atom]): NNFNode

  def domains: Set[Domain]

  // Keeping this as 'lazy var' should still work because by the time
  // orderedDomains is called, domains will have stabilized to the final value
  lazy val orderedDomains: IndexedSeq[Domain] = domains.toIndexedSeq

  def evalOrder: Int

  override def toString = toString(new NNFNameSpace)

  def getName(nameSpace: NameSpace[NNFNode, String]) = nameSpace.getName(this)

  def toString(nameSpace: NameSpace[NNFNode, String]): String = {
    (getName(nameSpace) + ": \n" +
      cnf + "\n" +
      (if (explanation.nonEmpty) explanation + "\n" else ""))
  }

  val fontsize = "" //"\tiny"

  /**
   * @todo    This is rather non-portable.
   */
  def showPDF(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
      compact: Boolean = false, maxDepth: Int = Integer.MAX_VALUE,
      dir: String = "nnfs", file: String = "liftedinference.nnf",
      verbose: Boolean = false) = {


    ExternalBinaries.checkPdfLatexAvailable
    ExternalBinaries.checkDotAvailable
    ExternalBinaries.checkDot2TexAvailable
    ExternalBinaries.checkDot2TexiAvailable

    if (!(new File(dir)).exists) (new File(dir)).mkdir
    val out = new FileWriter(dir + "/" + file + ".tex")
    out.write(toLatex(domainSizes, predicateWeights, compact, maxDepth))
    out.close

    val pdfFile = new java.io.File(dir + "/" + file + ".pdf")
    pdfFile.delete()

    println("# Running pdflatex on ./" + dir + "/" + file + ".tex")
    if (!verbose) println("# Switch --verbose on to see latex output")
    val pb1 = Process(ExternalBinaries.pdflatexCmd + " --shell-escape --interaction=nonstopmode " + file + ".tex", Some(new java.io.File(dir)));
    val output1 = new ListBuffer[String]()
    pb1 ! ExternalBinaries.stringLogger(output1, output1)
    output1.foreach(l => if (verbose) println(l))

    var pdfreaderbin = "evince";
    if (getenv("JAVA_ARCH") == "x86_64" ||
        System.getProperty("os.name").toLowerCase().startsWith("mac os x")) {
      // It's a Mac
      pdfreaderbin = "open";
    }

    println("# Trying to open ./" + dir + "/" + file + ".pdf")
    if ((new File("./" + dir + "/" + file + ".pdf")).exists()) {
      val output2 = new ListBuffer[String]()
      val pb2 = Process(pdfreaderbin + " " + file + ".pdf", Some(new java.io.File(dir)));
      pb2 ! ExternalBinaries.stringLogger(output2, output2)
      output2.foreach(l => if (verbose) println(l))
    } else {
      println("# ./" + dir + "/" + file + ".pdf not found")
    }
  }

  def toLatex(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    compact: Boolean = false, maxDepth: Int = Integer.MAX_VALUE) = (
    """\documentclass{article}

\""" + """usepackage{dot2texi}
\""" + """usepackage{tikz}
\""" + """usetikzlibrary{shapes,arrows}
\""" + """usepackage{amsmath}
\""" + """usepackage[active,pdftex,tightpage]{preview}

\newcommand{\bigforall}[2]{{{\raisebox{1pt}{\mbox{\Large$\forall$}$#1$}}\atop{\scriptstyle #2}}}
\newcommand{\bigexists}[2]{{{\raisebox{1pt}{\mbox{\Large$\exists$}$#1$}}\atop{\scriptstyle #2}}}

\PreviewEnvironment[{[]}]{tikzpicture}

\begin{document}

\begin{dot2tex}[options=-t raw --autosize]
""" + toDot(domainSizes, predicateWeights, compact, maxDepth) + """
\end{dot2tex}

\end{document}""")

  def toDot(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    compact: Boolean = false, maxDepth: Int = Integer.MAX_VALUE) = {
    val (nodes, edges) = toDotNode(domainSizes, predicateWeights, new NNFNameSpace, compact, 0, maxDepth)
    (
      """digraph G {
  node [shape=rectangle];
""" + nodes + edges + """
}""")
  }

  def edgeLabel(lbl: String) = {
    """label=" ", texlbl="""" + fontsize + """ """ + lbl + """""""
  }

  def cutoff(nameSpace: NameSpace[NNFNode, String], compact: Boolean = false): (String, String) = {
    val nodes = if (compact) {
      "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\vdots$", shape=triangle];""" + "\n"
    } else {
      "  " + getName(nameSpace) + """ [texlbl="""" + fontsize + """ """ + cnf.toLatex() + """"];""" + "\n" +
        "  " + "cutoff" + getName(nameSpace) + """ [texlbl="""" + fontsize + """ $\vdots$", shape=triangle];""" + "\n"
    }
    val edges = if (compact) {
      ""
    } else {
      "  " + getName(nameSpace) + " -> " + "cutoff" + getName(nameSpace) + """ [""" + edgeLabel(explanation) + """];""" + "\n"
    }
    (nodes, edges)
  }

  def toDotNode(domainSizes: DomainSizes, predicateWeights: PredicateWeights,
    nameSpace: NameSpace[NNFNode, String], compact: Boolean = false,
    depth: Int, maxDepth: Int = Integer.MAX_VALUE): (String, String)

}

abstract class ParametrisedNode() extends NNFNode {}
