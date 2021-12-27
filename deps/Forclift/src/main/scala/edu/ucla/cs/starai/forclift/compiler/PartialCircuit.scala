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

  def applySinkRulesToAllFormulas(additionalFormulas: List[CNF])
      : PartialCircuit = {
    // println("applySinkRules: The original circuit has " + formulas.size +
    //           " loose ends")
    val newFormulas = formulas.map { compiler.applySinkRules(_) } .flatMap {
      partialCircuit => {
        if (partialCircuit.circuit.isDefined) {
          require(circuit.get.addNode(partialCircuit.circuit.get))
          // println("applySinkRules: adding " +
          //           partialCircuit.circuit.get.getClass.getSimpleName)
        }
        partialCircuit.formulas
      }
    }
    // println("applySinkRules: The new circuit has " + newFormulas.size + " + " +
    //           additionalFormulas.size + " loose ends")
    new PartialCircuit(compiler, circuit, newFormulas ++ additionalFormulas)
  }

  // Make copies of the partial circuit as needed, add the resulting
  // subcircuits to them, and return full partial circuits
  def applyAllRules: Stream[PartialCircuit] = {
    val cnf = formulas.head
    Compiler.checkCnfInput(cnf)
    (0 until compiler.nonSinkRules.size).toStream.flatMap {
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
                require(circuitCopy.circuit.get.addNode(node))
                // println("applyAllRules: After applying non-sink rule " +
                //           ruleIndex +
                //           ", the number of loose ends changed from " +
                //           circuitCopy.formulas.size + " to " +
                //           circuitCopy.formulas.tail.size + " + " +
                //           successors.size)
                val newCircuit = new PartialCircuit(circuitCopy.compiler,
                                                    circuitCopy.circuit,
                                                    successors).
                  applySinkRulesToAllFormulas(circuitCopy.formulas.tail)
                Some(newCircuit)
              }
            }
          }
        }
      }
    }
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
