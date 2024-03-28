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

package org.apache.daffodil.runtime1.layers

// different subpackage on purpose to test package private methods

import java.io.InputStream
import java.io.OutputStream

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.schema.annotation.props.Enum
import org.apache.daffodil.runtime1.layers.api.Layer

/**
 * This layer has bombWhere and bombHow variables to enable
 * us to tell it where and how to error out of every situation.
 */
final class STL_BombOutLayer() extends Layer("stlBombOutLayer", "urn:STL") {

  private lazy val context: ThrowsSDE = this.getLayerRuntime.layerRuntimeData.context

  sealed trait Loc extends Loc.Value
  object Loc
    extends Enum[Loc] // with ThrowsSDE
    {
    final case object Nowhere extends Loc
    final case object Setter extends Loc
    final case object Getter extends Loc
    final case object WrapInput extends Loc
    final case object WrapOutput extends Loc
    final case object Read extends Loc
    final case object CloseInput extends Loc
    final case object Write extends Loc
    final case object CloseOutput extends Loc
    override lazy val values: Array[Loc] =
      Array(
        Nowhere,
        Setter,
        Getter,
        WrapInput,
        WrapOutput,
        Read,
        CloseInput,
        Write,
        CloseOutput,
      )

    def apply(name: String): Loc = apply(name, context)
    override def apply(name: String, context: ThrowsSDE): Loc =
      stringToEnum("Loc", name, context)
  }
  import Loc._

  sealed trait Kind extends Kind.Value
  object Kind extends Enum[Kind] {
    final case object ThrowRE extends Kind
    final case object ThrowEX extends Kind
    final case object ProcErr extends Kind
    final case object ProcErrWithCause extends Kind
    final case object RSDE extends Kind
    final case object NoBomb extends Kind
    final case object Abort extends Kind
    override lazy val values: Array[Kind] =
      Array(ThrowRE, ThrowEX, ProcErr, ProcErrWithCause, RSDE, NoBomb, Abort)

    def apply(name: String): Kind = apply(name, context)
    override def apply(name: String, context: ThrowsSDE): Kind =
      stringToEnum("Kind", name, context)
  }
  import Kind._

  private var intVar: Int = 0
  private var stringVar: String = _

  private var bombWhere: Loc = _
  private var bombHow: Kind = _

  setProcessingErrorException(classOf[Exception])

  private def bomb(loc: Loc): Unit = {
    loc match {
      case Nowhere => // ok
      case _ => {
        if (loc == bombWhere) {
          val msg = s"Bombed out at $loc."
          bombHow match {
            case ThrowRE =>
              throw new RuntimeException(msg)
            case ThrowEX =>
              throw new Exception(msg)
            case ProcErr =>
              processingError(msg)
            case ProcErrWithCause =>
              processingError(new Exception(msg))
            case RSDE =>
              runtimeSchemaDefinitionError(msg)
            case NoBomb =>
            // ok
            case Abort =>
              Assert.abort(msg)
            case _ =>
              Assert.invariantFailed(s"Not recognized bombHow value: $bombHow")
          }
        }
      }
    }
  }

  private[layers] def setLayerVariableParameters(
    intVar: Int,
    bombWhere: String,
    bombHow: String,
  ): Unit = {
    this.bombWhere = Loc(bombWhere)
    this.bombHow = Kind(bombHow)
    this.intVar = intVar
    this.stringVar = "forty two"
    bomb(Setter)
  }

  def getLayerVariableResult_stringVar: String = {
    bomb(Getter)
    stringVar + " " + stringVar
  }

  override def wrapLayerInput(jis: InputStream): InputStream = {
    bomb(WrapInput)
    new STL_InputStream(jis)
  }

  override def wrapLayerOutput(jos: OutputStream): OutputStream = {
    bomb(WrapOutput)
    new STL_OutputStream(jos)
  }

  class STL_InputStream(is: InputStream) extends InputStream {

    override def read(): Int = {
      bomb(Read)
      is.read
    }

    override def close(): Unit = {
      bomb(CloseInput)
      is.close()
    }
  }

  class STL_OutputStream(os: OutputStream) extends OutputStream {

    override def write(b: Int): Unit = {
      os.write(b)
      bomb(Write)
    }

    override def close(): Unit = {
      os.close()
      bomb(CloseOutput)
    }
  }
}
