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

package org.apache.daffodil.infoset

import java.nio.charset.StandardCharsets

import com.fasterxml.jackson.core.io.JsonStringEncoder

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.util.Indentable
import org.apache.daffodil.util.MStackOfBoolean

class JsonInfosetOutputter private (writer: java.io.Writer, pretty: Boolean)
  extends InfosetOutputter with Indentable {

  def this(os: java.io.OutputStream, pretty: Boolean) = {
    this(new java.io.OutputStreamWriter(os, StandardCharsets.UTF_8), pretty)
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


  override def startSimple(simple: DISimple): Unit = {
    startNode()
    startElement(simple)
    if (!isNilled(simple) && simple.hasValue) {
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
  }

  override def endSimple(simple: DISimple): Unit = {
    // nothing to do
  }

  override def startComplex(complex: DIComplex): Unit = {
    startNode()
    startElement(complex)
    if (!isNilled(complex)) {
      writer.write('{')
      prepareForChildren()
    } else {
      writer.write("null")
    }
  }

  override def endComplex(complex: DIComplex): Unit = {
    if (!isNilled(complex)) {
      endNodeWithChildren()
      writer.write('}')
    } else {
      // do nothing
    }
  }

  override def startArray(array: DIArray): Unit = {
    startNode()
    writer.write('"')
    writer.write(array.erd.name)
    writer.write("\": [")
    prepareForChildren()
  }

  override def endArray(array: DIArray): Unit = {
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
    if (pretty) writer.write(System.lineSeparator())
    writer.flush()
  }
}
