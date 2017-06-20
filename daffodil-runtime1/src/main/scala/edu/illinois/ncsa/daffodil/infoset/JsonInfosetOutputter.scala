/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */
package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.util.MStackOfBoolean
import edu.illinois.ncsa.daffodil.util.Indentable
import com.fasterxml.jackson.core.io.JsonStringEncoder
import edu.illinois.ncsa.daffodil.dpath.NodeInfo

class JsonInfosetOutputter(writer: java.io.Writer, pretty: Boolean = true)
  extends InfosetOutputter with Indentable {

  // Keeps track of if the next element we see is the first child or not of a
  // document/array/complex type. The top of the stack is true if the an
  // element is the first child in this scope. It's false otherwise. This is
  // used to determine if a comma should be written. We write commas before
  // writing start content if it is not the first child (i.e. top of the stack
  // is false).
  private val isFirstChildStack = MStackOfBoolean()

  private val stringEncoder = JsonStringEncoder.getInstance() // thread-safe instance of a string encoder

  override def reset(): Unit = {
    isFirstChildStack.clear()
    resetIndentation()
  }

  // handles skipping the comma for the first child in a complex/array,
  // starting a newline, and adding indenting for whatever ends up coming after
  // it
  private def startNode(): Unit = {
    if (isFirstChildStack.top == true) {
      // the first child does not need a comma before it, but all following children will
      isFirstChildStack.pop()
      isFirstChildStack.push(false)
    } else {
      writer.write(',')
    }
    if (pretty) writer.write(System.lineSeparator())
    if (pretty) outputIndentation(writer)
  }

  // handles logic for printing the name of a simple or complex element
  private def startElement(element: DIElement): Unit = {
    if (!element.erd.isArray) {
      // Only write the name if this is not an array of simple/complex types.
      // If it is an array, the name is written in startArray
      writer.write('"')
      writer.write(element.erd.name)
      writer.write("\": ")
    }
  }

  // modifies the state so that things with children (i.e.
  // document/complex/array) are at the right indentation level and commas are
  // created as necessary
  private def prepareForChildren(): Unit = {
    incrementIndentation()
    // the next child will be the first
    isFirstChildStack.push(true)
  }

  // undoes what was done prepareForChildren and closes the
  // complex/array/document at the right indentation level
  private def endNodeWithChildren(): Unit = {
    isFirstChildStack.pop()
    if (pretty) writer.write(System.lineSeparator())
    decrementIndentation()
    if (pretty) outputIndentation(writer)
  }



  override def startSimple(simple: DISimple): Boolean = {
    startNode()
    startElement(simple)
    if (!simple.isNilled) {
      val text =
        if (simple.erd.optPrimType.get.isInstanceOf[NodeInfo.String.Kind]) {
          new String(stringEncoder.quoteAsString(simple.dataValueAsString)) // escapes according to Json spec
        } else {
          simple.dataValueAsString
        }
      writer.write('"')
      writer.write(text)
      writer.write('"')
    } else {
      writer.write("null")
    }
    true
  }

  override def endSimple(simple: DISimple): Boolean = {
    // nothing to do
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    startNode()
    startElement(complex)
    if (!complex.isNilled) {
      writer.write('{')
      prepareForChildren()
    } else {
      writer.write("null")
    }
    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    if (!complex.isNilled) {
      endNodeWithChildren()
      writer.write('}')
    } else {
      // do nothing
    }
    true
  }

  override def startArray(array: DIArray): Boolean = {
    startNode()
    writer.write('"')
    writer.write(array.erd.name)
    writer.write("\": [")
    prepareForChildren()
    true
  }

  override def endArray(array: DIArray): Boolean = {
    endNodeWithChildren()
    writer.write(']')
    true
  }

  override def startDocument(): Boolean = {
    // does not use startNode() because the stack is empty and we also do
    // not want to output the newline that it outputs or do any indentation
    writer.write('{')
    prepareForChildren()
    true
  }

  override def endDocument(): Boolean = {
    endNodeWithChildren()
    writer.write('}')
    if (pretty) writer.write(System.lineSeparator())
    writer.flush()
    true
  }
}
