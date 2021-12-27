package edu.ucla.cs.starai.forclift.compiler

import collection._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object MyCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new MyLiftedCompiler(sizeHint)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new MyGroundingCompiler(sizeHint)

}

abstract class MyCompiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] =
    new mutable.HashMap[Int, List[(CNF, NNFNode)]])
    extends V1_1Compiler(sizeHint, nnfCache) {

  def tryImprovedDomainRecursion(cnf: CNF) = {
    cnf.clauses.find { !_.literalVariables.isEmpty } match {
      case None => None
      case Some(suitableClause) => {
        val domain = suitableClause.constrs.domainFor(
          suitableClause.literalVariables.head)
        val ineqs = suitableClause.constrs.ineqConstrs(
          suitableClause.literalVariables.head).collect {
          case c: Constant => c
        }
        val constant = groundingConstantFor(cnf, domain)
        val mixedClauses = cnf.clauses.flatMap {
          clause => {
            // only consider the subset of variables that come from the same
            // domain
            val vars = clause.literalVariables.filter {
              clause.constrs.domainFor(_).equals(domain)
            }
            vars.subsets.flatMap {
              equalVariables => {
                val substitutedClause = clause.substituteOption(
                  (variable: Var) => if (equalVariables.contains(variable))
                                       constant else variable)
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
        val msg = "Improved domain recursion on $" + domain + "$"
        println(msg + ". Before:")
        println(mixedCNF)
        println("After:")
        println(cnf + "\n")
        val node = new ImprovedDomainRecursionNode(cnf, None, constant, ineqs,
                                                   domain, msg)
        Some((Some(node), List(mixedCNF)))
      }
    }
  }

  def tryConstraintRemoval(cnf: CNF): InferenceResult = {
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
                println("\nConstraint removal. Before:")
                println(cnf)
                println("After:")
                println(newCnf + "\n")
                val node = new ConstraintRemovalNode(cnf, None, originalDomain,
                                                     newDomain)
                newDomain.setCause(node)
                return Some((Some(node), List(newCnf)))
              }
            }
            case _ => {}
          }
        }
      }
    }
    None
  }

  override def sinkRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryTautologyClauseElimination,
    //tryRemoveDoubleClauses, // revival
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryConstraintRemoval)

  override def nonSinkRules: List[InferenceRule] = List(
    tryIndependentSubtheories, // +1
    tryIndependentSubtheoriesAfterShattering, // +1
    tryGroundDecomposition, // +1
    tryInclusionExclusion, // +2
    tryShatter, // 0
    tryIndependentPartialGrounding, // 0
    tryCounting, // 0
    tryImprovedDomainRecursion // 0
  )

  override def inferenceRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    //tryRemoveDoubleClauses, // revival
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryTautologyClauseElimination,
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding, // O(log(n))
    tryCounting, // O(n)
    tryConstraintRemoval, // new
    tryImprovedDomainRecursion // new
  )

}

class MyLiftedCompiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] =
    new mutable.HashMap[Int, List[(CNF, NNFNode)]])
    extends MyCompiler(sizeHint, nnfCache) with LiftedCompiler {

  def myClone: MyLiftedCompiler =
    new MyLiftedCompiler(sizeHint, cloneCache)

}

class MyGroundingCompiler(
  sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
  nnfCache: mutable.HashMap[Int, List[(CNF, NNFNode)]] =
    new mutable.HashMap[Int, List[(CNF, NNFNode)]])
    extends MyCompiler(sizeHint, nnfCache) with GroundingCompiler {

  def myClone: MyGroundingCompiler =
    new MyGroundingCompiler(sizeHint, cloneCache)

}
