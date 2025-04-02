/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.infoset

import java.nio.charset.StandardCharsets

import org.apache.daffodil.lib.util.Indentable
import org.apache.daffodil.lib.util.MStackOfBoolean
import org.apache.daffodil.runtime1.iapi.DFDLPrimType
import org.apache.daffodil.runtime1.iapi.InfosetArray
import org.apache.daffodil.runtime1.iapi.InfosetComplexElement
import org.apache.daffodil.runtime1.iapi.InfosetElement
import org.apache.daffodil.runtime1.iapi.InfosetSimpleElement

import com.fasterxml.jackson.core.io.JsonStringEncoder

class JsonInfosetOutputter private (writer: java.io.BufferedWriter, pretty: Boolean)
  extends InfosetOutputter
  with Indentable {

  def this(os: java.io.OutputStream, pretty: Boolean) = {
    // using a BufferedWriter provides significant performance improvements
    this(
      new java.io.BufferedWriter(new java.io.OutputStreamWriter(os, StandardCharsets.UTF_8)),
      pretty
    )
  }

  // Keeps track of if the next element we see is the first child or not of a
  // document/array/complex type. The top of the stack is true if the an
  // element is the first child in this scope. It's false otherwise. This is
  // used to determine if a comma should be written. We write commas before
  // writing start content if it is not the first child (i.e. top of the stack
  // is false).
  private val isFirstChildStack = MStackOfBoolean()

  private val stringEncoder = JsonStringEncoder.getInstance()

  override def reset(): Unit = {
    isFirstChildStack.clear()
    resetIndentation()
  }

  // handles skipping the comma for the first child in a complex/array,
  // starting a newline, and adding indenting for whatever ends up coming after
  // it
  private def startNode(): Unit = {
    if (isFirstChildStack.top) {
      // the first child does not need a comma before it, but all following children will
      isFirstChildStack.pop()
      isFirstChildStack.push(false)
    } else {
      writer.write(',')
    }
    if (pretty) writer.newLine()
    if (pretty) outputIndentation(writer)
  }

  // handles logic for printing the name of a simple or complex element
  private def startElement(element: InfosetElement): Unit = {
    if (!element.metadata.isArray) {
      // Only write the name if this is not an array of simple/complex types.
      // If it is an array, the name is written in startArray
      writer.write('"')
      writer.write(element.metadata.name)
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
    if (pretty) writer.newLine()
    decrementIndentation()
    if (pretty) outputIndentation(writer)
  }

  override def startSimple(simple: InfosetSimpleElement): Unit = {
    startNode()
    startElement(simple)
    if (!simple.isNilled) {
      val text =
        if (simple.metadata.dfdlType == DFDLPrimType.String) {
          new String(
            stringEncoder.quoteAsString(simple.getText)
          ) // escapes according to Json spec
        } else {
          simple.getText
        }
      if (needsQuote(simple)) {
        writer.write('"')
        writer.write(text)
        writer.write('"')
      } else {
        writer.write(text)
      }
    } else {
      writer.write("null")
    }
  }

  private def needsQuote(simple: InfosetSimpleElement): Boolean = {
    simple.metadata.dfdlType match {
      case DFDLPrimType.String => true
      case DFDLPrimType.HexBinary => true
      case DFDLPrimType.AnyURI => true
      case DFDLPrimType.DateTime => true
      case DFDLPrimType.Date => true
      case DFDLPrimType.Time => true

      // json does not support inf/nan double/float so they must be quoted
      case DFDLPrimType.Double => {
        val d = simple.getDouble
        d.isInfinite || d.isNaN
      }
      case DFDLPrimType.Float => {
        val f = simple.getFloat
        f.isInfinite || f.isNaN
      }
      case _ => false
    }
  }

  override def endSimple(se: InfosetSimpleElement): Unit = {
    // nothing to do
  }

  override def startComplex(complex: InfosetComplexElement): Unit = {
    startNode()
    startElement(complex)
    if (!complex.isNilled) {
      writer.write('{')
      prepareForChildren()
    } else {
      writer.write("null")
    }
  }

  override def endComplex(complex: InfosetComplexElement): Unit = {
    if (!complex.isNilled) {
      endNodeWithChildren()
      writer.write('}')
    } else {
      // do nothing
    }
  }

  override def startArray(array: InfosetArray): Unit = {
    startNode()
    writer.write('"')
    writer.write(array.metadata.name)
    writer.write("\": [")
    prepareForChildren()
  }

  override def endArray(array: InfosetArray): Unit = {
    endNodeWithChildren()
    writer.write(']')
  }

  override def startDocument(): Unit = {
    // does not use startNode() because the stack is empty and we also do
    // not want to output the newline that it outputs or do any indentation
    writer.write('{')
    prepareForChildren()
  }

  override def endDocument(): Unit = {
    endNodeWithChildren()
    writer.write('}')
    if (pretty) writer.newLine()
    writer.flush()
  }
}
