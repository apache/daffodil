package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer

object CreatePaddingDFA {

  /**
   * Constructs a DFADelimiter object that specifically
   * looks for padChar.
   */
  def apply(padChar: Char, r: Registers): DFADelimiter = {
    // TODO: In the future we will need to change this because the padChar isn't necessarily a char. 
    // One can use it to specify a numeric byte to be used to pad as well.

    val allStates: ArrayBuffer[State] = ArrayBuffer.empty

    val startState = new StartStatePadding(allStates, r, padChar)

    allStates.insert(0, startState)

    new DFADelimiterImpl(allStates.toArray, padChar.toString())
  }
}