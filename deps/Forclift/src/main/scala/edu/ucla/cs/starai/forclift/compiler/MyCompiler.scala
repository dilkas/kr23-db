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
    // TODO: only consider the subset of variables that come from the same domain
    val mixedClauses = cnf.clauses.flatMap { clause =>
      clause.literalVariables.subsets.flatMap { equalVariables =>
        if (equalVariables.isEmpty || equalVariables.size == clause.literalVariables.size) List()
        else {
          val substitutedClause = clause.substitute((variable: Var) =>
            if (equalVariables.contains(variable)) constant else variable)
          val ineqVars = clause.literalVariables -- equalVariables
          List(ineqVars.foldLeft(substitutedClause) { _.addInequality(_, constant) })
        }
      }
    }
    val mixedCNF = new CNF(mixedClauses)
    val headVar1 = cnf.clauses.head.literalVariables.head
    val headVar2 = (cnf.clauses.head.literalVariables - headVar1).head
    val groundClauses = cnf.clauses.map { _.substitute { v => constant } }
    val groundCNF = new CNF(groundClauses)
    val msg = "Domain recursion on $" + domain + "$"
    val mixedNnf = tryIndependentPartialGrounding(mixedCNF)
    println("mixed CNF:")
    println(mixedCNF)
    println("mixed NNF:")
    println(mixedNnf.toString)
    println("ground CNF:")
    println(groundCNF.toString)
    Some(new ImprovedDomainRecursionNode(cnf, compile(mixedCNF), compile(groundCNF), constant, ineqs, domain, msg))
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
