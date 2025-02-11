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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.util._

object Processor {

  /**
   * This initialize routine is really part of compiling the DFDL schema so should be called from there.
   * This insures that it is not, for example, being multi-threaded. If called from the runtime module, compilation might
   * actually end up happening at run time.
   */
  def initialize(proc: Processor): Unit = {
    ensureCompiled(proc)
  }

  private def ensureCompiled(proc: Processor): Unit = {
    if (!proc.isInitialized) {
      proc.isInitialized = true
      proc.runtimeDependencies.foreach { ensureCompiled }
      proc.childProcessors.foreach { ensureCompiled }
    }
  }

  private def ensureCompiled(ev: Evaluatable[AnyRef]): Unit = {
    ev.ensureCompiled()
    ev.runtimeDependencies.foreach { ensureCompiled }
  }
}

/**
 * Captures common members for any processor, parser or unparser.
 */
trait Processor extends ToBriefXMLImpl with Serializable {
  // things common to both unparser and parser go here.
  def context: RuntimeData
  override def childProcessors: Vector[Processor]
  def runtimeDependencies: Vector[Evaluatable[AnyRef]]

  var isInitialized: Boolean = false

  /**
   * True if alignment, bit/byte order, and other aspects of real data on the data stream
   * are relevant.
   *
   * True for primitive processors that actually touch the data stream, false
   * for NoData, and for combinators.
   *
   * This enables an optimization in the runtime that doesn't evaluate
   * expensive expressions for values or checking of encoding, byteOrder, bitOrder
   * and so forth except for processors where it matters because they actually
   * interact with the data stream.
   */
  def isPrimitive: Boolean

  def isEmpty: Boolean
}

/**
 * A Prim or Primitive processor does not orchestrate the operation of
 * other processors, it actually does the work of parsing/unparsing.
 *
 * Most PrimProcessor actually manipulate data to/from the data stream.
 * Some (NoData) do not.
 */
trait PrimProcessor extends Processor {
  override def childProcessors: Vector[Processor] = Vector()

  /**
   * True if alignment, bit/byte order, and other aspects of real data on the data stream
   * are relevant.
   *
   * True for primitive processors that actually touch the data stream, false
   * otherwise.
   */
  override def isPrimitive = true
}

/**
 * A PrimProcessor which does other work than manipulating the data, such
 * as evaluating DPath expressions.
 */
trait PrimProcessorNoData extends Processor {
  override def childProcessors: Vector[Processor] = Vector()

  /**
   * False because NoData processors don't touch the data stream.
   */
  override def isPrimitive = false
}

/**
 * A combinator is a processor that orchestrates the operation of
 * other processors.
 *
 * Combinators must be pure. They cannot both touch the data stream directly and
 * also orchestrate other processors.
 */
trait CombinatorProcessor extends Processor {

  /**
   * False because combinators don't touch the data stream themselves. The
   * processors they call do that work.
   */
  override final def isPrimitive = false
}

/** must mixin to all processors that deal with text */
trait TextProcessor

/**
 * BriefXML is XML-style output, but intended for specific purposes. It is NOT
 * an XML serialization of the data structure. It's an XML-style string, suitable to
 * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
 * then get it all indented nicely, use a structure editor to expand/collapse subregions,
 * but it is NOT intended to capture all of the state of the object.
 */
trait ToBriefXMLImpl {

  private lazy val nom_ : String = Misc.getNameFromClass(this)
  def nom = nom_

  protected def briefXMLAttributes: String = ""

  def childProcessors: Seq[Processor]

  // TODO: make this create a DOM tree, not a single string (because of size limits)
  def toBriefXML(depthLimit: Int = -1): String = {
    val eltStartText =
      nom + (if (briefXMLAttributes == "") "" else " " + briefXMLAttributes + " ")
    if (depthLimit == 0) "..."
    else if (childProcessors.length == 0) "<" + eltStartText + "/>"
    else {
      val lessDepth = depthLimit - 1
      val sb = new StringBuilder
      childProcessors.foreach { cp =>
        val s = cp.toBriefXML(lessDepth)
        if (sb.size < 3000) sb.append(s) // hack!
        else sb.append("...")
      }
      "<" + eltStartText + ">" + sb + "</" + nom + ">"
    }
  }

  override def toString = toBriefXML() // pParser.toString + " ~ " + qParser.toString
}
