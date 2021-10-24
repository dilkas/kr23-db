package edu.ucla.cs.starai.forclift.compiler

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._

object GreedyCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new GreedyCompiler(sizeHint, false)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new GreedyCompiler(sizeHint, true)

}

class GreedyCompiler(sizeHint: Compiler.SizeHints =
                       Compiler.SizeHints.unknown(_),
                     grounding: Boolean = false) extends Compiler {

  lazy val compiler = if (grounding) {
    new MyGroundingCompiler(sizeHint)
  } else {
    new MyLiftedCompiler(sizeHint)
  }

  override def compile(cnf: CNF): NNFNode = {
    Compiler.checkCnfInput(cnf)
    var rules = compiler.inferenceRules
    var nnf: NNFNode = null
    while (nnf == null && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) {
        val (node, successors) = tryRule.get
        if (node.isEmpty) {
          require(successors.size == 1)
          nnf = compile(successors.head)
        } else {
          nnf = node.get
          // the important bit
          compiler.updateCache(cnf, nnf)
          nnf.update(successors.map { successor => Some(compile(successor)) } )
        }
      }
      else rules = rules.tail
    }
    if (nnf == null) {
      nnf = compiler.cannotCompile(cnf)
    }
    nnf
  }

}
