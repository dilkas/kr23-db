package edu.ucla.cs.starai.forclift.compiler

import collection._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object MyCompiler {
  val builder: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new MyCompiler(sizeHint) with LiftedCompiler
  val builderWithGrounding: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new MyCompiler(sizeHint) with GroundingCompiler
}

abstract class MyCompiler(sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_)) extends V1_1Compiler(sizeHint) {
  def tryImprovedDomainRecursion(cnf: CNF) = {
    println("improved domain recursion")
    println(cnf.toString)
    val domain = cnf.clauses.head.constrs.domainFor(cnf.clauses.head.literalVariables.head)
    val ineqs = cnf.clauses.head.constrs.ineqConstrs(cnf.clauses.head.literalVariables.head).collect { case c: Constant => c }
    val constant = groundingConstantFor(cnf, domain)
    val mixedClauses = cnf.clauses.flatMap {
      clause => {
        // only consider the subset of variables that come from the same domain
        val vars = clause.literalVariables.filter { clause.constrs.domainFor(_).equals(domain) }
        vars.subsets.flatMap { equalVariables =>
          if (equalVariables.isEmpty || equalVariables.size == clause.literalVariables.size) List()
          else {
            val substitutedClause = clause.substituteOption((variable: Var) =>
              if (equalVariables.contains(variable)) constant else variable)
            substitutedClause match {
              case Some(s) => {
                val ineqVars = vars -- equalVariables
                List(ineqVars.foldLeft(s) { _.addInequality(_, constant) })
              }
              case None => List()
            }
          }
        }
      }
    }
    val mixedCNF = new CNF(mixedClauses)
    val headVar1 = cnf.clauses.head.literalVariables.head
    val headVar2 = (cnf.clauses.head.literalVariables - headVar1).head
    val msg = "Domain recursion on $" + domain + "$"
    println("mixed CNF:")
    println(mixedCNF)
    Some(new ImprovedDomainRecursionNode(cnf, compile(mixedCNF), constant, ineqs, domain, msg))
  }

  override def inferenceRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryTautologyClauseElimination, // added wrt NIPS11
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding, // O(log(n))
    tryCounting, // O(n)
    tryImprovedDomainRecursion, // new
    tryDomainRecursion // is O(log(n)) now! But assumes no unary predicates
  )
}
