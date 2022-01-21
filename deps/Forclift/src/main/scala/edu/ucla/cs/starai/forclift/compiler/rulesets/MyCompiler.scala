package edu.ucla.cs.starai.forclift.compiler.rulesets

import collection._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object MyCompiler {

  val builder: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new MyLiftedCompiler(sizeHint)

  val builderWithGrounding: Compiler.Builder =
    (sizeHint: Compiler.SizeHints) => new MyGroundingCompiler(sizeHint)

}

/** The latest version of the compiler.
  *
  * Adds three more rules:
  * 1) an improved version of domain recursion capable of creating cycles,
  * 2) constraint removal that removes a bunch of 'X != x' constraints by
  *    creating a new domain without domain value x,
  * 3) contradiction filter that finds a completely empty clause and removes
  *    all other clauses so that the formula would be immediately identified as
  *    unsatisfiable by another rule.
  *
  * @param nnfCache see the AbstractCompiler class
  */
abstract class MyCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends V1_1Compiler(sizeHint, nnfCache) {

  def tryImprovedDomainRecursion(cnf: CNF) = {
    cnf.clauses.find { !_.literalVariables.isEmpty } match {
      case None => None
      case Some(suitableClause) => {
        log("Improved domain recursion")
        val domain =
          suitableClause.constrs.domainFor(suitableClause.literalVariables.head)
        val constant = groundingConstantFor(cnf, domain)
        val mixedClauses = cnf.clauses.flatMap { clause =>
          {
            // only consider the subset of variables that come from the same
            // domain
            val vars = clause.literalVariables.filter {
              clause.constrs.domainFor(_).equals(domain)
            }
            vars.subsets.flatMap { equalVariables =>
              {
                val substitutedClause =
                  clause.substituteOption((variable: Var) =>
                    if (equalVariables.contains(variable))
                      constant
                    else variable
                  )
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
        log("Before:")
        log(mixedCNF.toString)
        val msg = "Improved domain recursion on $" + domain + "$"
        val node = new ImprovedDomainRecursionNode(
          cnf,
          None,
          constant,
          domain,
          msg
        )
        log("After:")
        log(cnf + "\n")
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
                if (
                  cnf.forall { clause =>
                    clause.atoms.forall { atom: Atom =>
                      !atom.constants.contains(constant)
                    }
                  } &&
                  cnf.forall { clause =>
                    clause.allVariables.forall { variable: Var =>
                      clause.constrs.domainFor(variable) !=
                        originalDomain ||
                      clause.constrs.ineqConstrs(variable).contains(constant)
                    }
                  }
                ) {
                  val newIndex = (originalDomain.nbSplits + 1).toString
                  val newDomain = originalDomain.subdomain(
                    newIndex,
                    newIndex,
                    excludedConstants = Set(constant)
                  )
                  val newCnf = CNF(cnf.map { clause =>
                    clause
                      .removeConstraints(constant)
                      .replaceDomains(originalDomain, newDomain)
                  }.toList: _*)
                  val node = new ConstraintRemovalNode(
                    cnf,
                    None,
                    originalDomain,
                    newDomain
                  )
                  newDomain.setCause(node)

                  log("\nConstraint removal. Before:")
                  log(cnf.toString)
                  log("Constraint removal. After:")
                  log(newCnf + "\n")

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
    val contradictionClauseOption = cnf.clauses.find { c =>
      c.isConditionalContradiction && c.isUnconditional
    }
    if (contradictionClauseOption.nonEmpty) {
      Some((None, List(new CNF(List(contradictionClauseOption.get)))))
    } else {
      None
    }
  }

  override def greedyRules: List[InferenceRule] =
    List(
      tryCache,
      tryTautology,
      tryContradictionClause,
      tryPositiveUnitClause,
      tryNegativeUnitClause,
      tryContradictionFilter, // new
      tryTautologyClauseElimination,
      tryRemoveDoubleClauses, // revived
      tryPositiveUnitPropagation,
      tryNegativeUnitPropagation,
      tryConstraintRemoval, // new
      tryIndependentSubtheories, // +1
      tryIndependentSubtheoriesAfterShattering
    )

  override def nonGreedyRules: List[InferenceRule] =
    List(
      tryGroundDecomposition, // +1
      // tryInclusionExclusion, // +2
      tryShatter, // 0
      tryIndependentPartialGrounding, // 0
      tryCounting, // 0
      tryImprovedDomainRecursion // 0, new
    )

}

class MyLiftedCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends MyCompiler(sizeHint, nnfCache)
    with LiftedCompiler {

  def myClone(): MyLiftedCompiler =
    new MyLiftedCompiler(sizeHint, cloneCache())

}

class MyGroundingCompiler(
    sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_),
    nnfCache: Compiler.Buckets = new Compiler.Buckets
) extends MyCompiler(sizeHint, nnfCache)
    with GroundingCompiler {

  def myClone(): MyGroundingCompiler =
    new MyGroundingCompiler(sizeHint, cloneCache())

}
