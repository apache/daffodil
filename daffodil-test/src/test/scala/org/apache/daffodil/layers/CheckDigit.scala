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

package org.apache.daffodil.layers

import org.apache.commons.io.IOUtils
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthKind
import org.apache.daffodil.schema.annotation.props.gen.LayerLengthUnits
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.processors.LayerLengthEv
import org.apache.daffodil.processors.LayerBoundaryMarkEv
import org.apache.daffodil.processors.LayerCharsetEv
import org.apache.daffodil.processors.parsers.PState
import org.apache.daffodil.processors.unparsers.UState

import java.io._
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.CharsetEvBase
import org.apache.daffodil.processors.SequenceRuntimeData
import org.apache.daffodil.processors.SuspendableOperation
import org.apache.daffodil.processors.charset.BitsCharsetJava
import org.apache.daffodil.util.ByteBufferOutputStream
import org.apache.daffodil.util.Logger
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.RefQName

import java.nio.ByteBuffer
import java.nio.charset.Charset


final class CheckDigitExplicit(
  srd: SequenceRuntimeData,
  charset: Charset,
  layerLengthEv: LayerLengthEv)
extends LayerTransformer() {
  Assert.invariant(charset.newDecoder().maxCharsPerByte() == 1) // is a SBCS charset

  private val checkDigitVRD = {
    val varNamespace = NS("urn:org.apache.daffodil.layers.checkDigit")
    val varQName = RefQName(Some("cd"), "checkDigit", varNamespace).toGlobalQName
    val vrd = srd.variableMap.getVariableRuntimeData(varQName).getOrElse {
      srd.SDE("Variable '%s' is not defined.", varQName.toExtendedSyntax)
    }
    srd.schemaDefinitionUnless(vrd.primType == PrimType.Int,
      "Variable '%s' is not of type 'xs:int'.", varQName.toExtendedSyntax)
    vrd
  }

  private val checkDigitParamsVRD = {
    val varNamespace = NS("urn:org.apache.daffodil.layers.checkDigit")
    val varQName = RefQName(Some("cd"), "checkDigitParams", varNamespace).toGlobalQName
    val vrd = srd.variableMap.getVariableRuntimeData(varQName).getOrElse {
      srd.SDE("Variable '%s' is not defined.", varQName.toExtendedSyntax)
    }
    srd.schemaDefinitionUnless(vrd.primType == PrimType.String,
      "Variable '%s' is not of type 'xs:string'.", varQName.toExtendedSyntax)
    vrd
  }

  private var limitingOutputStream : ByteBufferOutputStream = _
  private var originalOutputStream: OutputStream = _

  //
  // Our example here takes a parameter which is a flag indicating if it is to
  // issue warning messages to help with debugging.
  //
  // But really this is just to show that a layer transform can read variables
  // as well as write them.
  //
  case class Params(isVerbose: Boolean)

  private def parseParams(paramString: String) = {
    if (paramString.toLowerCase.contains("verbose")) Params(isVerbose = true)
    else Params(isVerbose = false)
  }

  protected def wrapLayerDecoder(jis: InputStream) = jis

  override def wrapLimitingStream(jis: java.io.InputStream, state: PState) = {
    val layerLengthInBytes: Int = layerLengthEv.evaluate(state).toInt
    val ba = new Array[Byte](layerLengthInBytes)
    try
      IOUtils.readFully(jis, ba)
    catch {
      case eof: EOFException =>
        throw LayerNotEnoughDataException(srd, state, eof, ba.length)
    }
    // this stream will be used by the parse when it parses the
    // schema contents of the layer. We have already consumed the bytes
    // from the original input stream, and saved those in the byte array
    // to enable the computation of the checkDigit.
    val layerStream = new ByteArrayInputStream(ba)
    val str = new String(ba, charset) // always does replacement on decode errors.
    val params = state.getVariable(checkDigitParamsVRD, srd).getString
    val isVerbose = parseParams(params).isVerbose
    if (isVerbose)
      Logger.log.info(s"CheckDigit layer: computing check digit for '$str.")

    val checkDigit = computeCheckDigit(str)
    state.setVariable(checkDigitVRD, checkDigit, srd) // assign to result variable
    layerStream
  }

  protected def wrapLayerEncoder(jos: OutputStream) = jos

  protected def wrapLimitingStream(jos: OutputStream, state: UState) = {
    originalOutputStream = jos
    val layerLengthInBytes: Int = layerLengthEv.evaluate(state).toInt
    val ba = new Array[Byte](layerLengthInBytes)
    val byteBuf = ByteBuffer.wrap(ba) // we don't need byteOrder in this example, but FYI: bigEndian byte order by default
    limitingOutputStream = new ByteBufferOutputStream(byteBuf)
    limitingOutputStream
  }

  /**
   * Shared by both parsing and unparsing.
   *
   * Ignores any non-digit character in the argument.
   *
   * The checkDigit is the total of all digits, viewed as a string, the last digit of that total.
   *
   * Returns a long, because the type of the variable this gets assigned to, is unsignedInt, which
   * uses a Long as its representation.
   */
  private def computeCheckDigit(s: String): Int = {
    val digits: Seq[Int] = s.filter{ _.isDigit }.map{ _.asDigit }
    val num = digits.sum
    val checkDigit = num.toString.last.asDigit
    checkDigit
  }

  final class SuspendableCheckDigitLayerOperation()
    extends SuspendableOperation {
    override def rd = srd

    /**
     * Test succeeds if all the data required has been written to the layer
     */
    protected def test(ustate: UState) = {
      //
      // Note that there is no such thing when unparsing as "not enough data". Rather, the unparser
      // will just deadlock because this test will never be satisfied.
      //
      limitingOutputStream.size() == limitingOutputStream.byteBuffer.capacity()
    }

    /**
     * Computes checkdigit, assigns the output variable, then writes the layer data out,
     */
    protected def continuation(ustate: UState): Unit = {
      val ba = limitingOutputStream.byteBuffer.array()
      val str = new String(ba, charset)
      val checkDigit = computeCheckDigit(str)
      ustate.setVariable(checkDigitVRD, checkDigit, srd) // assign to the result variable.
      //
      // write out the layer data
      originalOutputStream.write(ba)
      originalOutputStream.close()
    }
  }

  private lazy val suspendableOperation = new SuspendableCheckDigitLayerOperation()

  override def endLayerForUnparse(s: UState): Unit = {
    suspendableOperation.run(s)
  }

}

object CheckDigit
  extends LayerTransformerFactory("checkDigit") {

  override def newInstance(
    maybeLayerCharsetEv: Maybe[LayerCharsetEv], // if not supplied, taken from the srd.
    maybeLayerLengthKind: Maybe[LayerLengthKind], // explicit or implicit
    maybeLayerLengthEv: Maybe[LayerLengthEv], // ignored (with warning ideally)
    maybeLayerLengthUnits: Maybe[LayerLengthUnits], // ignored (with warning ideally)
    maybeLayerBoundaryMarkEv: Maybe[LayerBoundaryMarkEv], // ignored (for now, this is a future possibility)
    srd: SequenceRuntimeData) = {


    val layerLengthKind = {
      if (maybeLayerLengthKind.isEmpty) {
        srd.SDE("The 'dfdlx:layerLengthKind' property must be defined.")
      } else {
        val llk = maybeLayerLengthKind.get
        llk match {
          case LayerLengthKind.Explicit => {
            srd.schemaDefinitionUnless(maybeLayerLengthEv.isDefined, "Property dfdlx:layerLength must be defined.")
            maybeLayerLengthUnits.toScalaOption match {
              case Some(LayerLengthUnits.Bytes) => // ok
              case None => // ok
              case _ => srd.SDE("Property dfdlx:layerLengthUnits must be 'bytes'.")
            }
          }
          case other => srd.SDE("The dfdlx:layerLengthKind '%s' is not supported.", other)
        }
        llk
      }
    }

    // To simplify, insist the charset is a constant, 8-bit aligned, SBCS encoding, e.g., ascii or 8859-1.

    val layerCharsetEv: CharsetEvBase =
      if (maybeLayerCharsetEv.isDefined) maybeLayerCharsetEv.get
      else srd.encodingInfo.charsetEv

    val bitsCharset = layerCharsetEv.optConstant.getOrElse{
      srd.SDE("Must have a character set encoding defined.")
    }
    val javaCharset: Charset = bitsCharset match {
      case jbc: BitsCharsetJava if jbc.bitWidthOfACodeUnit == 8 => jbc.javaCharset
      case _ => srd.SDE(
        "Charset encoding must be for a byte-aligned single-byte character set, but was encoding '%s', which has width %s bits, and alignment %s bits.",
        bitsCharset.name, bitsCharset.bitWidthOfACodeUnit, bitsCharset.mandatoryBitAlignment)
    }

    //
    // ToDo: We can't issue SDW because we don't have a context object
    // for that sort of compilation time warning. We *should* have that.
    // That requires a layer factory method called at schema compile time with the
    // Sequence term (not just term runtime data) as an argument.
    //
    val xformer = new CheckDigitExplicit(srd, javaCharset, maybeLayerLengthEv.get)
    xformer
  }
}
