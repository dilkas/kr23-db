package edu.ucla.cs.starai.forclift.compiler

import collection._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object MyCompiler {
  val builder: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new MyCompiler(sizeHint) with LiftedCompiler
  val builderWithGrounding: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new MyCompiler(sizeHint) with GroundingCompiler
}

abstract class MyCompiler(sizeHint: Compiler.SizeHints =
                            Compiler.SizeHints.unknown(_))
    extends V1_1Compiler(sizeHint) {

  def tryImprovedDomainRecursion(cnf: CNF) = {
    val domain = cnf.clauses.head.constrs.domainFor(cnf.clauses.head.literalVariables.head)
    val ineqs = cnf.clauses.head.constrs.ineqConstrs(cnf.clauses.head.literalVariables.head).collect { case c: Constant => c }
    val constant = groundingConstantFor(cnf, domain)
    println("tryImprovedDomainRecursion: selected constant " + constant)
    val mixedClauses = cnf.clauses.flatMap {
      clause => {
        // only consider the subset of variables that come from the same domain
        val vars = clause.literalVariables.filter { clause.constrs.domainFor(_).equals(domain) }
        println("tryImprovedDomainRecursion: identified " + vars.size + " viable variables")
        vars.subsets.flatMap {
          equalVariables => {
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
    println("\nimproved domain recursion")
    println(cnf + "\n")
    val node = new ImprovedDomainRecursionNode(cnf, None, constant, ineqs, domain, msg)
    updateCache(cnf, node)
    node.update(List(compile(mixedCNF)))
    Some(node)
  }

  def tryConstraintRemoval(cnf: CNF): Option[NNFNode] = {
    for (originalClause <- cnf) {
      for ((variable, terms) <- originalClause.constrs.ineqConstrs) {
        val originalDomain = originalClause.constrs.domainFor(variable)
        for (term <- terms) {
          term match {
            case constant: Constant => {
              // We have a "v != c" constraint. Does it apply to all variables
              // from the same domain across all clauses? And does c occur in atoms?
              // I.e., for each clause, for each variable, either domain is
              // different or there is the same inequality constraint.
              if (cnf.forall { clause => clause.atoms.forall { atom: Atom =>
                                !atom.constants.contains(constant) } } &&
                    cnf.forall { clause => clause.allVariables.forall {
                                  variable: Var =>
                                  clause.constrs.domainFor(variable) !=
                                    originalDomain ||
                                    clause.constrs.ineqConstrs(variable).
                                    contains(constant) } }) {
                val newDomain = originalDomain.subdomain(excludedConstants =
                                                           Set(constant))
                val newCnf = CNF(cnf.map { clause =>
                                   clause.removeConstraints(originalDomain,
                                                            constant).
                                     replaceDomains(originalDomain,
                                                    newDomain) }.toList: _*)
                println("\nconstraint removal")
                println(cnf + "\n")
                return Some(new ConstraintRemovalNode(
                              cnf, Some(compile(newCnf)), originalDomain,
                              newDomain))
              }
            }
            case _ => {}
          }
        }
      }
    }
    None
  }

  override def inferenceRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryConstraintRemoval, // new
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
    tryDomainRecursion, // is O(log(n)) now! But assumes no unary predicates
    tryImprovedDomainRecursion // new
  )

}
