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

import java.lang.{
  Boolean => JBoolean,
  Byte => JByte,
  Double => JDouble,
  Float => JFloat,
  Integer => JInt,
  Long => JLong,
  Number => JNumber,
  Short => JShort,
  String => JString
}
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }
import java.net.URI

import org.apache.daffodil.lib.calendar.DFDLCalendar
import org.apache.daffodil.lib.calendar.DFDLDate
import org.apache.daffodil.lib.calendar.DFDLDateTime
import org.apache.daffodil.lib.calendar.DFDLTime
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One

/*
 * These traits are used for the phantom type X. When considering the type of a
 * DataValue. See the comment on the DataValue class below.
 */
trait NonNullable extends Nullable;
trait Nullable extends tUseNilForDefault;
trait tUseNilForDefault;
sealed trait DataValuePrimitiveType

/**
 * A class to provide basic type safety around infoset and DPath data values.
 * At runtime, this class goes away, and objects are passed around unboxed as if
 * we were refering to the underlying objects directly as an AnyRef. Note, however,
 * that in some circumstances, Scala may still choice to box these objects, resulting
 * in the same overhead of a normal wrapper class.
 */
/*
 * The type variable X here is a phantom type. That is to say, it is not actually
 * used to describe the type of anything within DataValue. The goal is to provide us
 * with more ability to control the inheritense relationship between various DataValue types.
 * In theory, this would be accomplished by establishing a type hierarchy on the types
 * used for T. Indeed, if Scala had type-classes as a first-class feature that is likely
 * the approach we would have taken. However, since most of the types we use for T
 * are out of our control, we are unable to control what type structure they have.
 * By carefully selecting X in our type aliases, it is possible for to define types
 * that can represent any subset of types which we want. It is further possible to
 * allow for automatic upcasting between these types where appropriate.
 *
 */
final class DataValue[+T <: AnyRef, +X <: AnyRef] private (val v: T)
  extends AnyVal
  with Serializable {
  @inline def isEmpty = DataValue.NoValue.v eq v
  @inline def isDefined = !isEmpty
  @inline def value = v
  @inline override def toString = if (isEmpty) "NoValue" else "DataValue(" + v.toString + ")"

  @inline def getAnyRef = v.asInstanceOf[AnyRef]
  @inline def getBigDecimal = v.asInstanceOf[JBigDecimal]
  @inline def getCalendar = v.asInstanceOf[DFDLCalendar]
  @inline def getDate = v.asInstanceOf[DFDLDate]
  @inline def getTime = v.asInstanceOf[DFDLTime]
  @inline def getDateTime = v.asInstanceOf[DFDLDateTime]
  @inline def getByteArray = v.asInstanceOf[Array[Byte]]
  @inline def getBoolean = v.asInstanceOf[JBoolean]
  @inline def getNumber = v.asInstanceOf[JNumber]
  @inline def getByte = v.asInstanceOf[JByte]
  @inline def getShort = v.asInstanceOf[JShort]
  @inline def getInt = v.asInstanceOf[JInt]
  @inline def getLong = v.asInstanceOf[JLong]
  @inline def getDouble = v.asInstanceOf[JDouble]
  @inline def getFloat = v.asInstanceOf[JFloat]
  @inline def getBigInt = v.asInstanceOf[JBigInt]
  @inline def getString = v.asInstanceOf[JString]
  @inline def getURI = v.asInstanceOf[URI]

  @inline def getNonNullable: DataValue[T, X with NonNullable] = new DataValue(v)
  @inline def getNullablePrimitive: DataValue.DataValuePrimitiveNullable = new DataValue(v)

  @inline def getOptionAnyRef = {
    if (isEmpty) {
      None
    } else {
      Some(getAnyRef)
    }
  }

  @inline def getMaybe[T <: AnyRef]: Maybe[T] = {
    if (isEmpty) {
      Nope
    } else {
      One(getAnyRef.asInstanceOf[T])
    }
  }
}

object DataValue {

  /** All values which are legal for DPath and infoset data values. Note that this incudes
   *  DINodes, which is legal for DPath, but not infoset data values.
   *  Also note that at any given time, the infoset may have no value, which is not directly
   *  representable by this type.
   */
  type DataValuePrimitive = DataValue[AnyRef, NonNullable with DataValuePrimitiveType]

  /** A (set-theoretic) extension of DataValuePrimitive adjoining a NULL element refered to as NoValue.
   * Since this just adjoins NoValue, we can think of it as a nullable varient of DataValuePrimitive.
   * See https://en.wikipedia.org/wiki/Nullable_type
   */
  type DataValuePrimitiveNullable = DataValue[AnyRef, Nullable with DataValuePrimitiveType]

  /** All values of DataValuePrimitiveNullable, plus a sentinal UseNilForDefault value.
   * Used only by the default field of ElementRuntimeData.
   */
  type DataValuePrimitiveOrUseNilForDefaultOrNull = DataValue[AnyRef, tUseNilForDefault]

  type DataValueEmpty = DataValue[Null, Nullable with DataValuePrimitiveType]
  type DataValueBigDecimal = DataValue[JBigDecimal, NonNullable with DataValuePrimitiveType]
  type DataValueCalendar = DataValue[DFDLCalendar, NonNullable with DataValuePrimitiveType]
  type DataValueDateTime = DataValue[DFDLDateTime, NonNullable with DataValuePrimitiveType]
  type DataValueDate = DataValue[DFDLDate, NonNullable with DataValuePrimitiveType]
  type DataValueTime = DataValue[DFDLTime, NonNullable with DataValuePrimitiveType]
  type DataValueByteArray = DataValue[Array[Byte], NonNullable with DataValuePrimitiveType]
  type DataValueBool = DataValue[JBoolean, NonNullable with DataValuePrimitiveType]
  type DataValueNumber = DataValue[JNumber, NonNullable with DataValuePrimitiveType]
  type DataValueLong = DataValue[JLong, NonNullable with DataValuePrimitiveType]
  type DataValueDouble = DataValue[JDouble, NonNullable with DataValuePrimitiveType]
  type DataValueBigInt = DataValue[JBigInt, NonNullable with DataValuePrimitiveType]
  type DataValueString = DataValue[JString, NonNullable with DataValuePrimitiveType]
  type DataValueURI = DataValue[URI, NonNullable with DataValuePrimitiveType]
  type DataValueFloat = DataValue[JFloat, NonNullable with DataValuePrimitiveType]
  type DataValueByte = DataValue[JByte, NonNullable with DataValuePrimitiveType]
  type DataValueInt = DataValue[JInt, NonNullable with DataValuePrimitiveType]
  type DataValueShort = DataValue[JShort, NonNullable with DataValuePrimitiveType]
  type DataValueDINode = DataValue[DINode, NonNullable with DataValuePrimitiveType]
  type DataValueUseNilForDefault = DataValue[UseNilForDefaultObj, NonNullable]

  import scala.language.implicitConversions

  @inline implicit def toDataValue(v: JBigDecimal): DataValueBigDecimal = new DataValue(v)
  @inline implicit def toDataValue(v: DFDLCalendar): DataValueCalendar = new DataValue(v)
  @inline implicit def toDataValue(v: DFDLDateTime): DataValueDateTime = new DataValue(v)
  @inline implicit def toDataValue(v: DFDLDate): DataValueDate = new DataValue(v)
  @inline implicit def toDataValue(v: DFDLTime): DataValueTime = new DataValue(v)
  @inline implicit def toDataValue(v: Array[Byte]): DataValueByteArray = new DataValue(v)
  @inline implicit def toDataValue(v: JBoolean): DataValueBool = new DataValue(v)
  @inline implicit def toDataValue(v: JNumber): DataValueNumber = new DataValue(v)
  @inline implicit def toDataValue(v: JLong): DataValueLong = new DataValue(v)
  @inline implicit def toDataValue(v: JDouble): DataValueDouble = new DataValue(v)
  @inline implicit def toDataValue(v: JBigInt): DataValueBigInt = new DataValue(v)
  @inline implicit def toDataValue(v: JString): DataValueString = new DataValue(v)
  @inline implicit def toDataValue(v: URI): DataValueURI = new DataValue(v)
  @inline implicit def toDataValue(v: JFloat): DataValueFloat = new DataValue(v)
  @inline implicit def toDataValue(v: JByte): DataValueByte = new DataValue(v)
  @inline implicit def toDataValue(v: JInt): DataValueInt = new DataValue(v)
  @inline implicit def toDataValue(v: JShort): DataValueShort = new DataValue(v)
  @inline implicit def toDataValue(v: DINode): DataValueDINode = new DataValue(v)

  @inline implicit def toDataValue(v: Long): DataValueLong = new DataValue(v: JLong)
  @inline implicit def toDataValue(v: Double): DataValueDouble = new DataValue(v: JDouble)
  @inline implicit def toDataValue(v: Boolean): DataValueBool = new DataValue(v: JBoolean)
  @inline implicit def toDataValue(v: Float): DataValueFloat = new DataValue(v: JFloat)
  @inline implicit def toDataValue(v: Byte): DataValueByte = new DataValue(v: JByte)
  @inline implicit def toDataValue(v: Int): DataValueInt = new DataValue(v: JInt)
  @inline implicit def toDataValue(v: Short): DataValueShort = new DataValue(v: JShort)

  @inline def unsafeFromAnyRef(v: AnyRef) = new DataValue(v)
  @inline def unsafeFromMaybeAnyRef(v: Maybe[AnyRef]) = {
    if (v.isDefined) {
      new DataValue(v.get)
    } else {
      NoValue
    }
  }
  @inline def unsafeFromOptionAnyRef(v: Option[AnyRef]) = {
    if (v.isDefined) {
      new DataValue(v.get)
    } else {
      NoValue
    }
  }

  val NoValue: DataValueEmpty = new DataValue(null)

  /** Used as a sentinal value for Element's defaultValue, when said element
   *  is nillable and has dfdl:useNilForDefault set to true,
   */
  val UseNilForDefault: DataValueUseNilForDefault = new DataValue(new UseNilForDefaultObj)

  final protected class UseNilForDefaultObj {
    override def toString = "UseNilForDefault"
  }

  @inline def assertValueIsNotDataValue(v: AnyRef): Unit = {

    /*
     *
     * In our CompileExpressions classes, we use type variables declared as T <: AnyRef
     *
     * Ideally, we would have declared T <: DataValuePrimitive
     * However, we need to be able to refer to something of the form Maybe[T].
     * In theory, it should be possible to take a T <: DataValuePrimitive,
     * and construct a T' <: DataValuePrimitiveNullable, such that T <: T'
     * In practice, it does not appear to be easy to tell Scala's type system what we want to do,
     * so we instead punt on this issue and require the caller to translate to/from AnyRef at the boundary.
     *
     * In theory, if a caller forgets to do so, and instead passes in a DataValue instead of AnyRef,
     * the compiler would issue a type error because DataValue is an AnyVal type, and so does not inherit from AnyRef.
     *
     * In practice, Scala will "helpfully" box the DataValue for us, which causes all sorts of problems. For instance,
     * x.asInstanceOf[String] does not work when x is a DataValueString (even though an unboxed DataValueString is literally just a String at runtime).
     *
     * To make matters worse, the Scala compiler, does not seem to realize the implications of this autoboxing,
     * and so believes that is impossible for an AnyRef to ever be an instance of DataValue.
     * As such, is issues a warning on the naive typecheck since it can "never" fail. We silence this warning,
     * by first casting to Any.
     *
     */
    Assert.invariant(!v.asInstanceOf[Any].isInstanceOf[DataValue[AnyRef, AnyRef]])

    // Ideally, we could compare against our DataValueAny type alias. However, Scala (correctly) points out,
    // that type erasure means that DataValueAny contains information that Scala cannot actually verify at runtime.
    // To silence this warning, we explictly check against the underlying DataValue type.
    //
    // The commented out assertion below shows what we are trying to accomplish.
    // The actual assertion above is equivalent, but does not result in warnings.
    //  Assert.invariant(!v.isInstanceOf[DataValueAny])
  }
}
