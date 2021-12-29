package edu.ucla.cs.starai.forclift.compiler

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.nnf.visitors.DomainsVisitor
import edu.ucla.cs.starai.forclift.nnf.visitors.PostOrderVisitor

object CompilerWrapper {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new CompilerWrapper(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new CompilerWrapper(sizeHint, true)

}

class CompilerWrapper(sizeHint: Compiler.SizeHints =
                        Compiler.SizeHints.unknown(_),
                      grounding: Boolean = false) extends Compiler {

  val greedy: Boolean = false

  lazy val compiler = if (greedy) {
    new GreedyCompiler(sizeHint, grounding)
  } else {
    new BreadthCompiler(sizeHint, grounding)
  }

  override def compile(cnf: CNF): List[NNFNode] = {
    val nnfs = compiler.compile(cnf)
    nnfs.foreach {
      nnf => {
        val postOrderVisitor = new PostOrderVisitor
        postOrderVisitor.visit(nnf)
        val domainsVisitor = new DomainsVisitor(postOrderVisitor.nodeOrder)
        domainsVisitor.updateDomains
      }
    }
    nnfs
  }

}
