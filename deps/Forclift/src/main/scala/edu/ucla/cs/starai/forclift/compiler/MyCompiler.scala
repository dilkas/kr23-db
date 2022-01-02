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
        println("Improved domain recursion")
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
        println("Before:")
        println(mixedCNF)
        val msg = "Improved domain recursion on $" + domain + "$"
        val node = new ImprovedDomainRecursionNode(cnf, None, constant, ineqs,
                                                   domain, msg)
        println("After:")
        println(cnf + "\n")
        Some((Some(node), List(mixedCNF)))
      }
    }
  }

  def tryConstraintRemoval(cnf: CNF): InferenceResult = {
    for (originalClause <- cnf) {
      for ((variable, terms) <- originalClause.constrs.ineqConstrs) {
        val originalDomain = originalClause.constrs.domainFor(variable)
        // Recursion that subtracts more than 1 from a domain size gets a bit
        // complicated with multiple base cases, so we don't currently support
        // it.
        if (!originalDomain.isCausedByConstraintRemoval) {
          for (term <- terms) {
            term match {
              case constant: Constant => {
                // We have a "v != c" constraint. Does it apply to all
                // variables from the same domain across all clauses? And does
                // c occur in atoms? I.e., for each clause, for each variable,
                // either domain is different or there is the same inequality
                // constraint.
                if (cnf.forall { clause => clause.atoms.forall { atom: Atom =>
                                  !atom.constants.contains(constant) } } &&
                      cnf.forall { clause => clause.allVariables.forall {
                                    variable: Var =>
                                    clause.constrs.domainFor(variable) !=
                                      originalDomain ||
                                      clause.constrs.ineqConstrs(variable).
                                      contains(constant) } }) {
                  val newIndex = (originalDomain.nbSplits + 1).toString
                  val newDomain = originalDomain.subdomain(
                    newIndex, newIndex, excludedConstants = Set(constant))
                  val newCnf = CNF(cnf.map { clause =>
                                     clause.removeConstraints(originalDomain,
                                                              constant).
                                       replaceDomains(originalDomain,
                                                      newDomain) }.toList: _*)
                  val node = new ConstraintRemovalNode(
                    cnf, None, originalDomain, newDomain)
                  newDomain.setCause(node)

                  println("\nConstraint removal. Before:")
                  println(cnf)
                  println("Constraint removal. After:")
                  println(newCnf + "\n")

                  return Some((Some(node), List(newCnf)))
                }
              }
              case _ => {}
            }
          }
        }
      }
    }
    None
  }

  def tryContradictionFilter(cnf: CNF) = {
    val contradictionClauseOption = cnf.clauses.find {
      c => c.isConditionalContradiction && c.isUnconditional
    }
    if (contradictionClauseOption.nonEmpty) {
      Some((None, List(new CNF(List(contradictionClauseOption.get)))))
    } else {
      None
    }
  }

  override def sinkRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryContradictionFilter,
    tryTautologyClauseElimination,
    tryRemoveDoubleClauses,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryConstraintRemoval,
    tryIndependentSubtheories, // +1
    tryIndependentSubtheoriesAfterShattering
)

  override def nonSinkRules: List[InferenceRule] = List(
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
    tryContradictionFilter, // new
    tryTautologyClauseElimination,
    tryRemoveDoubleClauses, // revival
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryConstraintRemoval, // new
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding, // O(log(n))
    tryCounting, // O(n)
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
