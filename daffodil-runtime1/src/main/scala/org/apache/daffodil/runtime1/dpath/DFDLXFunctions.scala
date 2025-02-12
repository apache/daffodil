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

package org.apache.daffodil.runtime1.dpath

import java.lang.{ Double => JDouble }
import java.math.{ BigInteger => JBigInt }

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType.PrimNumeric
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueDouble
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueLong
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.processors.parsers.PState
import org.apache.daffodil.runtime1.processors.parsers.ParseError
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

import passera.unsigned.{ UByte, UInt, ULong, UShort }

/**
 * This is the "logical" shift left.
 * The left-most bits shifted out are discarded. Zeros are shifted in for the right-most bits.
 *
 * @param recipes
 * @param argType
 */
case class DFDLXLeftShift(recipes: List[CompiledDPath], argType: NodeInfo.Kind)
  extends FNTwoArgs(recipes) {
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {

    val shiftLong = arg2.getLong
    val shift = shiftLong.toInt
    val width = argType.asInstanceOf[PrimNumeric].maxWidth.get
    Assert.invariant(shift >= 0)
    if (shift >= width)
      dstate.SDE(
        "dfdlx:leftShift not supported for shift greater or equal to %s for %s. Shift was %s",
        width,
        argType.globalQName,
        shift
      )

    argType match {
      case NodeInfo.Long => arg1.getLong << shift
      case NodeInfo.Int => arg1.getInt << shift
      case NodeInfo.Short => (arg1.getShort << shift).toShort
      case NodeInfo.Byte => (arg1.getByte << shift).toByte
      case NodeInfo.UnsignedLong => (ULong(arg1.getBigInt.longValue()) << shift).toBigInt
      case NodeInfo.UnsignedInt => ULong(arg1.getLong << shift).toUInt.toLong
      case NodeInfo.UnsignedShort => UInt(arg1.getInt << shift).toUShort.toInt
      case NodeInfo.UnsignedByte => UInt(arg1.getShort << shift).toUByte.toShort
      // $COVERAGE-OFF$
      case _ =>
        Assert.invariantFailed(
          s"Should not have gotten ${argType.globalQName} for left shift argument type"
        )
      // $COVERAGE-ON$
    }
  }
}

/**
 * This is the "arithmetic" shift right. The right-most bits shifted out are discarded.
 * The sign bit is shifted in for the left-most bits. This means for signed primitives the values are sign-extended.
 * If "logical" (zero-filling) shift right is needed, you must use unsigned primitives.
 *
 *
 * @param recipes
 * @param argType
 */
case class DFDLXRightShift(recipes: List[CompiledDPath], argType: NodeInfo.Kind)
  extends FNTwoArgs(recipes) {
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {
    val shiftLong = arg2.getLong
    val shift = shiftLong.toInt
    val width = argType.asInstanceOf[PrimNumeric].maxWidth.get
    Assert.invariant(shift >= 0)
    if (shift >= width)
      dstate.SDE(
        "dfdlx:rightShift not supported for shift greater or equal to %s for %s. Shift was %s",
        width,
        argType.globalQName,
        shift
      )
    argType match {
      case NodeInfo.Long => arg1.getLong >> shift
      case NodeInfo.Int => arg1.getInt >> shift
      case NodeInfo.Short => (arg1.getShort >> shift).toShort
      case NodeInfo.Byte => (arg1.getByte >> shift).toByte
      case NodeInfo.UnsignedLong => (ULong(arg1.getBigInt.longValue()) >> shift).toBigInt
      case NodeInfo.UnsignedInt => ULong(arg1.getLong.toInt >> shift).toUInt.toLong
      case NodeInfo.UnsignedShort => UInt(arg1.getInt.toShort >> shift).toUShort.toInt
      case NodeInfo.UnsignedByte => UInt(arg1.getShort.toByte >> shift).toUByte.toShort
      // $COVERAGE-OFF$
      case _ =>
        Assert.invariantFailed(
          s"Should not have gotten ${argType.globalQName} for right shift argument type"
        )
      // $COVERAGE-ON$
    }
  }
}
case class DFDLXBitAnd(recipes: List[CompiledDPath], argType: NodeInfo.Kind)
  extends FNTwoArgs(recipes) {
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {
    argType match {
      case NodeInfo.Long => arg1.getLong & arg2.getLong
      case NodeInfo.Int => arg1.getInt & arg2.getInt
      case NodeInfo.Short => (arg1.getShort & arg2.getShort).toShort
      case NodeInfo.Byte => (arg1.getByte & arg2.getByte).toByte
      case NodeInfo.UnsignedLong => arg1.getBigInt.and(arg2.getBigInt)
      case NodeInfo.UnsignedInt => arg1.getLong & arg2.getLong
      case NodeInfo.UnsignedShort => arg1.getInt & arg2.getInt
      case NodeInfo.UnsignedByte => (arg1.getShort & arg2.getShort).toShort
      // $COVERAGE-OFF$
      case _ => Assert.invariantFailed(s"dfdlx:bitAnd not supported")
      // $COVERAGE-ON$
    }
  }
}

case class DFDLXBitOr(recipes: List[CompiledDPath], argType: NodeInfo.Kind)
  extends FNTwoArgs(recipes) {
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {
    argType match {
      case NodeInfo.Long => arg1.getLong | arg2.getLong
      case NodeInfo.Int => arg1.getInt | arg2.getInt
      case NodeInfo.Short => (arg1.getShort | arg2.getShort).toShort
      case NodeInfo.Byte => (arg1.getByte | arg2.getByte).toByte
      case NodeInfo.UnsignedLong => arg1.getBigInt.or(arg2.getBigInt)
      case NodeInfo.UnsignedInt => arg1.getLong | arg2.getLong
      case NodeInfo.UnsignedShort => arg1.getInt | arg2.getInt
      case NodeInfo.UnsignedByte => (arg1.getShort | arg2.getShort).toShort
      // $COVERAGE-OFF$
      case _ => Assert.invariantFailed(s"dfdlx:bitOr not supported")
      // $COVERAGE-ON$
    }
  }
}

case class DFDLXBitNot(recipes: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipes, argType) {
  override def computeValue(arg1: DataValuePrimitive, dstate: DState): DataValuePrimitive = {
    argType match {
      case NodeInfo.Long => ~arg1.getLong
      case NodeInfo.Int => ~arg1.getInt
      case NodeInfo.Short => (~arg1.getShort).toShort
      case NodeInfo.Byte => (~arg1.getByte).toByte
      case NodeInfo.UnsignedLong => ULong((~arg1.getBigInt.longValue)).toBigInt
      case NodeInfo.UnsignedInt => UInt((~arg1.getLong).toInt).toLong
      case NodeInfo.UnsignedShort => UShort((~arg1.getInt).toShort).toInt
      case NodeInfo.UnsignedByte => UByte((~arg1.getShort).toByte).toShort
      // $COVERAGE-OFF$
      case _ => Assert.invariantFailed(s"dfdlx:bitNot not supported")
      // $COVERAGE-ON$
    }
  }
}
case class DFDLXBitXor(recipes: List[CompiledDPath], argType: NodeInfo.Kind)
  extends FNTwoArgs(recipes) {
  override def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {
    argType match {
      case NodeInfo.Long => arg1.getLong ^ arg2.getLong
      case NodeInfo.Int => arg1.getInt ^ arg2.getInt
      case NodeInfo.Short => (arg1.getShort ^ arg2.getShort).toShort
      case NodeInfo.Byte => (arg1.getByte ^ arg2.getByte).toByte
      case NodeInfo.UnsignedLong => arg1.getBigInt.xor(arg2.getBigInt)
      case NodeInfo.UnsignedInt => arg1.getLong ^ arg2.getLong
      case NodeInfo.UnsignedShort => arg1.getInt ^ arg2.getInt
      case NodeInfo.UnsignedByte => (arg1.getShort ^ arg2.getShort).toShort
      // $COVERAGE-OFF$
      case _ => Assert.invariantFailed(s"dfdlx:bitXor not supported")
      // $COVERAGE-ON$
    }
  }
}

case class DFDLXTrace(recipe: CompiledDPath, msg: String)
  extends RecipeOpWithSubRecipes(recipe) {

  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    val nodeString: String = dstate.currentNode match {
      case _: DISimple | null => {
        // if there is no current node (null case) then there must be a
        // current value.
        val v = dstate.currentValue
        dstate.setCurrentValue(v)
        v.toString()
      }
      case other: DINode => other.namedQName.toString
    }
    Logger.log.info(s"dfdlx:trace ${msg} : ${nodeString}")
  }

  // This is toXML for the case class object, not the infoset node it is
  // dealing with.
  override def toXML = toXMLVarargs(recipe.toXML)

}

case object DAFError extends RecipeOp {

  override def run(dstate: DState): Unit = {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope
    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, "The error function was called.")
      case _: ParserMode => {
        val fe = new FNErrorFunctionException(
          maybeSFL,
          dstate.contextLocation,
          "The error function was called."
        )
        throw fe
      }
    }
  }
}

case class DFDLXLookAhead(recipes: List[CompiledDPath]) extends FNTwoArgs(recipes) {

  def computeValue(
    arg1: DataValuePrimitive,
    arg2: DataValuePrimitive,
    dstate: DState
  ): DataValuePrimitive = {
    val offset = arg1.getLong
    val lBitSize = arg2.getLong

    /*
     * Since dfdlx:lookAhead is defined to take unsigned arguements, the DPath interperater
     * will error out on the cast if a negative arguement is supplied, so we do not need to SDE here.
     */

    Assert.invariant(offset >= 0)
    Assert.invariant(lBitSize >= 0)

    val totalLookahead = offset + lBitSize
    val maxLookahead = dstate.tunable.maxLookaheadFunctionBits
    if (totalLookahead > maxLookahead) {
      dstate.SDE(
        "Look-ahead distance of %s bits exceeds implementation defined limit of %s bits",
        totalLookahead,
        maxLookahead
      )
    }
    // Safe since we guard on totalLookahead
    val bitSize = lBitSize.toInt

    if (!dstate.parseOrUnparseState.isDefined) {
      Assert.invariant(dstate.isCompile)
      /*
       * This is an expected code path.
       * Throwing an exception is how we indicated that this expression
       * cannot be reduced to a constant at compile time.
       */
      throw new IllegalStateException("No input stream at compile time")
    }
    if (dstate.parseOrUnparseState.get.isInstanceOf[PState]) {
      val pstate = dstate.parseOrUnparseState.get.asInstanceOf[PState]
      val dis = pstate.dataInputStream
      if (!dis.isDefinedForLength(totalLookahead)) {
        val maybeSFL =
          if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
          else Nope
        throw new ParseError(
          maybeSFL,
          dstate.contextLocation,
          Nope,
          One("Insufficient bits available to satisfy dfdlx:lookAhead(%s,%s)."),
          offset,
          bitSize,
          totalLookahead
        )
      }
      val mark = dis.markPos
      dis.skip(offset, pstate)
      val ans: DataValuePrimitive = if (bitSize > 63) {
        dis.getUnsignedBigInt(bitSize, pstate)
      } else if (bitSize == 0) {
        JBigInt.ZERO
      } else {
        JBigInt.valueOf(dis.getUnsignedLong(bitSize, pstate).longValue)
      }
      dis.resetPos(mark)
      ans
    } else {
      Assert.invariant(dstate.parseOrUnparseState.get.isInstanceOf[UState])
      dstate.SDE("Cannot call dfdlx:lookAhead() during unparse")
    }
  }
}

case class DFDLXDoubleFromRawLong(recipes: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipes, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState): DataValueDouble = {
    JDouble.longBitsToDouble(value.getLong)
  }
}

case class DFDLXDoubleToRawLong(recipes: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipes, argType) {
  override def computeValue(value: DataValuePrimitive, dstate: DState): DataValueLong = {
    JDouble.doubleToRawLongBits(value.getDouble)
  }
}
