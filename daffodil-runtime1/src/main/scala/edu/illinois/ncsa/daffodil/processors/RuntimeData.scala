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

import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dsom.DPathCompileInfo
import edu.illinois.ncsa.daffodil.dsom.ImplementsThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.ConstantExpression
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.HasSchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.YesNo
import edu.illinois.ncsa.daffodil.util.PreSerialization
import edu.illinois.ncsa.daffodil.util.TransientParam
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.RefQName
import scala.collection.mutable

trait RuntimeData
  extends ImplementsThrowsSDE
  with HasSchemaFileLocation {
  val schemaFileLocation: SchemaFileLocation
  val prettyName: String
  val path: String
  val namespaces: NamespaceBinding

  def immediateEnclosingElementRuntimeData: Option[ElementRuntimeData]
  def immediateEnclosingTermRuntimeData: Maybe[TermRuntimeData]
  def variableMap: VariableMap
  override def toString = prettyName

}

object TermRuntimeData {

  private var nextID = 0

  def generateTermID: Int = synchronized {
    val n = nextID
    nextID += 1
    n
  }

}

abstract class TermRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam immediateEnclosingElementRuntimeDataArg: => Option[ElementRuntimeData],
  @TransientParam immedEnclosingTermRuntimeDataArg: => Maybe[TermRuntimeData],
  @TransientParam encodingInfoArg: => EncodingRuntimeData,
  @TransientParam dpathCompileInfoArg: => DPathCompileInfo,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam fillByteValueArg: => Int,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends RuntimeData
  with Serializable
  with PreSerialization {

  private val termID = TermRuntimeData.generateTermID

  final override def hashCode(): Int = termID

  final override def equals(other: Any) = other match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  lazy val immediateEnclosingElementRuntimeData = immediateEnclosingElementRuntimeDataArg
  lazy val immediateEnclosingTermRuntimeData = immedEnclosingTermRuntimeDataArg
  lazy val encodingInfo = encodingInfoArg
  lazy val dpathCompileInfo = dpathCompileInfoArg
  lazy val isRepresented = isRepresentedArg
  lazy val couldHaveText = couldHaveTextArg
  lazy val alignmentValueInBits = alignmentValueInBitsArg
  lazy val hasNoSkipRegions = hasNoSkipRegionsArg
  lazy val fillByteValue = fillByteValueArg
  lazy val defaultBitOrder = defaultBitOrderArg
  lazy val optIgnoreCase = optIgnoreCaseArg

  override def preSerialization: Unit = {
    super.preSerialization
    immediateEnclosingElementRuntimeData
    immediateEnclosingTermRuntimeData
    encodingInfo
    dpathCompileInfo
    isRepresented
    couldHaveText
    alignmentValueInBits
    hasNoSkipRegions
    fillByteValue
    defaultBitOrder
    optIgnoreCase
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
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam immediateEnclosingElementRuntimeDataArg: => Option[ElementRuntimeData],
  @TransientParam immedEnclosingTermRuntimeDataArg: => Maybe[TermRuntimeData])
  extends RuntimeData
  with PreSerialization {

  override lazy val variableMap = variableMapArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val prettyName = prettyNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val immediateEnclosingElementRuntimeData = immediateEnclosingElementRuntimeDataArg
  lazy val immediateEnclosingTermRuntimeData = immedEnclosingTermRuntimeDataArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    schemaFileLocation
    prettyName
    path
    namespaces
    immediateEnclosingElementRuntimeData
    immediateEnclosingTermRuntimeData
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

}

sealed abstract class ModelGroupRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam fillByteValueArg: => Int,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends TermRuntimeData(Some(erdArg),
    Maybe(trdArg),
    encInfoArg, ciArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, fillByteValueArg,
    defaultBitOrderArg, optIgnoreCaseArg) {

  lazy val variableMap = variableMapArg
  lazy val encInfo = encInfoArg
  lazy val schemaFileLocation = schemaFileLocationArg
  lazy val ci = ciArg
  lazy val prettyName = prettyNameArg
  lazy val path = pathArg
  lazy val namespaces = namespacesArg
  lazy val groupMembers = groupMembersArg
  lazy val erd = erdArg
  lazy val trd = trdArg

  override def preSerialization: Unit = {
    super.preSerialization
    variableMap
    encInfo
    schemaFileLocation
    ci
    prettyName
    path
    namespaces
    groupMembers
    erd
    trd
  }
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

class SequenceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam fillByteValueArg: => Int,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, prettyNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    erdArg, trdArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, fillByteValueArg, optIgnoreCaseArg)

class ChoiceRuntimeData(
  /**
   * These transient by-name args are part of how we
   * hook these objects into a parent-child tree without
   * having to use an assignment to a var.
   */
  @TransientParam variableMapArg: => VariableMap,
  @TransientParam encInfoArg: => EncodingRuntimeData,
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam ciArg: => DPathCompileInfo,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam defaultBitOrderArg: => BitOrder,
  @TransientParam groupMembersArg: => Seq[TermRuntimeData],
  @TransientParam erdArg: ElementRuntimeData,
  @TransientParam trdArg: => TermRuntimeData,
  @TransientParam isRepresentedArg: => Boolean,
  @TransientParam couldHaveTextArg: => Boolean,
  @TransientParam alignmentValueInBitsArg: => Int,
  @TransientParam hasNoSkipRegionsArg: => Boolean,
  @TransientParam fillByteValueArg: => Int,
  @TransientParam optIgnoreCaseArg: => Option[YesNo])
  extends ModelGroupRuntimeData(variableMapArg, encInfoArg, schemaFileLocationArg, ciArg, prettyNameArg, pathArg, namespacesArg, defaultBitOrderArg, groupMembersArg,
    erdArg, trdArg, isRepresentedArg, couldHaveTextArg, alignmentValueInBitsArg, hasNoSkipRegionsArg, fillByteValueArg, optIgnoreCaseArg)

class VariableRuntimeData(
  @TransientParam schemaFileLocationArg: => SchemaFileLocation,
  @TransientParam prettyNameArg: => String,
  @TransientParam pathArg: => String,
  @TransientParam namespacesArg: => NamespaceBinding,
  @TransientParam externalArg: => Boolean,
  @TransientParam maybeDefaultValueExprArg: => Maybe[CompiledExpression[AnyRef]],
  @TransientParam typeRefArg: => RefQName,
  @TransientParam globalQNameArg: => GlobalQName,
  @TransientParam primTypeArg: => NodeInfo.PrimType)
  extends NonTermRuntimeData(
    null, // no variable map
    schemaFileLocationArg,
    prettyNameArg,
    pathArg,
    namespacesArg,
    None,
    None)
  with Serializable {

  lazy val external = externalArg
  lazy val maybeDefaultValueExpr = maybeDefaultValueExprArg
  lazy val typeRef = typeRefArg
  lazy val globalQName = globalQNameArg
  lazy val primType = primTypeArg

  override def preSerialization: Unit = {
    super.preSerialization
    external
    maybeDefaultValueExpr
    typeRef
    globalQName
    primType
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  private val state =
    if (!maybeDefaultValueExpr.isDefined) VariableUndefined
    else VariableDefined

  private val maybeValue: Maybe[AnyRef] =
    if (maybeDefaultValueExpr.isEmpty) Nope
    else {
      val defaultValueExpr = maybeDefaultValueExpr.get
      defaultValueExpr match {
        case constExpr: ConstantExpression[_] => One(constExpr.constant.asInstanceOf[AnyRef])
        case _ => Nope
      }
    }

  def newVariableInstance: Variable = Variable(state, maybeValue, this, maybeDefaultValueExpr)

}
