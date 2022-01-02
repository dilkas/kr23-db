package edu.ucla.cs.starai.forclift.compiler

import scala.collection.immutable.Queue

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._

class PartialCircuit(val compiler: AbstractCompiler,
                     val circuit: Option[NNFNode], val formulas: List[CNF]) {

  def myClone: PartialCircuit = {
    // println("circuit is non-empty: " + circuit.isDefined)
    val newCircuit = circuit.map(_.myClone)
    val newCompiler = compiler.myClone
    new PartialCircuit(newCompiler, newCircuit, formulas)
  }

  // Make copies of the partial circuit as needed, add the resulting
  // subcircuits to them, and return full partial circuits
  def applyAllRules: Stream[PartialCircuit] = {
    println("applyAllRules: started")
    val cnf = formulas.head
    Compiler.checkCnfInput(cnf)
    val circuits = (0 until compiler.nonSinkRules.size).toStream.flatMap {
      ruleIndex => {
        val circuitCopy = myClone
        circuitCopy.compiler.applyIthRule(ruleIndex, cnf) match {
          case None => None // the rule is not applicable
          case Some((node: Option[NNFNode], successors: List[CNF])) => {
            node match {
              case None => {
                // rerun on the updated theory
                require(successors.size == 1)
                new PartialCircuit(circuitCopy.compiler, circuitCopy.circuit,
                                   successors ++ circuitCopy.formulas.tail)
                  .applyAllRules
              }
              case Some(node) => {
                circuitCopy.compiler.updateCache(cnf, node)
                val newSuccessors = circuitCopy.compiler.
                  applySinkRulesToAllFormulas(node, successors)
                println("applyAllRules is calling addNode to add " +
                          node.getClass.getSimpleName)
                require(circuitCopy.circuit.get.addNode(node))
                Some(new PartialCircuit(circuitCopy.compiler,
                                        circuitCopy.circuit, newSuccessors ++
                                          circuitCopy.formulas.tail))
              }
            }
          }
        }
      }
    }
    println("applyAllRules: finished")
    circuits
  }

  def nextCircuits(compiler: Compiler): Queue[PartialCircuit] = {
    val newPartialCircuits = applyAllRules.flatMap { newPartialCircuit =>
      if (newPartialCircuit.formulas.isEmpty) {
        compiler.foundSolution(newPartialCircuit.circuit.get)
        None
      } else {
        Some(newPartialCircuit)
      }
    }
    Queue(newPartialCircuits: _*)
  }

}
