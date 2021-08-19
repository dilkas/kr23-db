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
    // only consider the subset of variables that come from the same domain
    val mixedClauses = cnf.clauses.flatMap { clause =>
      clause.literalVariables.filter {
        clause.constrs.domainFor(_).equals(domain) }.subsets.flatMap { equalVariables =>
        if (equalVariables.isEmpty || equalVariables.size == clause.literalVariables.size) List()
        else {
          val substitutedClause = clause.substituteOption((variable: Var) =>
            if (equalVariables.contains(variable)) constant else variable)
          substitutedClause match {
            case Some(s) => {
              val ineqVars = clause.literalVariables -- equalVariables
              List(ineqVars.foldLeft(s) { _.addInequality(_, constant) })
            }
            case None => List()
          }
        }
      }
    }
    val mixedCNF = new CNF(mixedClauses)
    val headVar1 = cnf.clauses.head.literalVariables.head
    val headVar2 = (cnf.clauses.head.literalVariables - headVar1).head
    // same here: limit what variables are being considered
    val groundClauses = cnf.clauses.map {
      clause => clause.substituteOption {
        v => if (clause.constrs.domainFor(v).equals(domain)) constant
             else v } }.flatten
    val groundCNF = new CNF(groundClauses)
    val msg = "Domain recursion on $" + domain + "$"
    println("mixed CNF:")
    println(mixedCNF)
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
