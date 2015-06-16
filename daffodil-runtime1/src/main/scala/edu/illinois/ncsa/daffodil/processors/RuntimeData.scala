/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionError
import edu.illinois.ncsa.daffodil.dsom.SchemaDefinitionWarning
import edu.illinois.ncsa.daffodil.api.Diagnostic
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.dsom.DPathCompileInfo
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.HasSchemaFileLocation
import edu.illinois.ncsa.daffodil.dpath._
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

trait RuntimeData
  extends ImplementsThrowsSDE
  with HasSchemaFileLocation {
  val schemaFileLocation: SchemaFileLocation
  val prettyName: String
  val path: String
  val namespaces: NamespaceBinding

  def immediateEnclosingRuntimeData: Option[RuntimeData]
  def variableMap: VariableMap
  override def toString = prettyName

}

abstract class TermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @transient immedEnclosingRD: => Option[RuntimeData],
  final val encodingInfo: EncodingRuntimeData,
  val dpathCompileInfo: DPathCompileInfo,
  val isRepresented: Boolean,
  val couldHaveText: Boolean,
  val alignmentValueInBits: Int,
  val hasNoSkipRegions: Boolean,
  val fillByteValue: Int,
  val defaultBitOrder: BitOrder)
  extends RuntimeData
  with Serializable
  with PreSerialization {

  lazy val immediateEnclosingRuntimeData = immedEnclosingRD

  override def preSerialization: Unit = {
    super.preSerialization
    immediateEnclosingRuntimeData
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  def fillByte(ustate: UState, encodingInfo: EncodingRuntimeData): Int = {
    Assert.invariant(encodingInfo.isKnownEncoding) // FIXME: implement runtime determination of encoding
    fillByteValue
  }
}

class NonTermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @transient variableMapArg: => VariableMap,
  override val schemaFileLocation: SchemaFileLocation,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  override val immediateEnclosingRuntimeData: Option[RuntimeData])
  extends RuntimeData
  with PreSerialization {

  override lazy val variableMap = variableMapArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

class ModelGroupRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @transient variableMapArg: => VariableMap,
  encInfo: EncodingRuntimeData,
  override val schemaFileLocation: SchemaFileLocation,
  ci: DPathCompileInfo,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  defaultBitOrder: BitOrder,
  val groupMembers: Seq[TermRuntimeData],
  val erd: ElementRuntimeData,
  isRepresented: Boolean,
  couldHaveText: Boolean,
  alignmentValueInBits: Int,
  hasNoSkipRegions: Boolean,
  fillByteValue: Int)
  extends TermRuntimeData(Some(erd), encInfo, ci, isRepresented, couldHaveText, alignmentValueInBits, hasNoSkipRegions, fillByteValue, defaultBitOrder) {

  override lazy val variableMap = variableMapArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

class VariableRuntimeData(
  sfl: SchemaFileLocation,
  override val prettyName: String,
  override val path: String,
  override val namespaces: NamespaceBinding,
  val external: Boolean,
  val defaultValue: Option[String],
  val typeRef: RefQName,
  val globalQName: GlobalQName,
  val primType: NodeInfo.PrimType)
  extends NonTermRuntimeData(
    null, // no variable map
    sfl,
    prettyName,
    path,
    namespaces,
    None)
  with Serializable {

}