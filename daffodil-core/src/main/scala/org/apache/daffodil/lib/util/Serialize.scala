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

package org.apache.daffodil.lib.util

import scala.collection.mutable.HashMap
import scala.jdk.CollectionConverters._

import org.apache.daffodil.lib.exceptions.Assert

trait PreSerialization extends Serializable {

  val me = this.getClass()
  Assert.usage(
    PreSerialization.classHasWriteObjectMethod(me),
    String.format("Class %s does not implement the method writeObject.", me.getName())
  )

  /**
   * If this is overridden by a def, there's a chance we call it
   * multiple times. So we don't use this directly but by way
   * of a lazy val preSerializationOnlyOnce.
   */
  protected def preSerialization: Any = {
    // nothing by default
  }

  /**
   * Use this to insure we pre-serialize only once.
   */
  final lazy val preSerializationOnlyOnce = {
    // println("preSerializing " + this) // good place for a breakpoint
    preSerialization
  }

  // painful, but you must explicitly put a private version of this in each
  // class that you want to have call the preSerialization hook and have
  // be tracable, and have this catch, etc.
  //
  // This is because the serialization algorithm will call this for every
  // class in the inheritance hierarchy of a serializable class.
  // Hence, it is private since it only deals with fields of that one class.
  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  protected final def serializeObject(out: java.io.ObjectOutputStream): Unit = {
    try {
      preSerializationOnlyOnce
      out.defaultWriteObject()
    } catch {
      case ns: java.io.NotSerializableException => {
        Assert.abort(
          "Could not serialize member of class %s, found within class %s".format(
            ns.getMessage(),
            Misc.getNameFromClass(this)
          )
        )
      }
    }
  }
}

private object PreSerialization {
  private val classCache = new HashMap[Class[_], Boolean]

  //
  // This private method ensures that any class (or any super class) that use this trait also implements the writeObject method
  // used by the Java serialization framework.
  //
  private def classHasWriteObjectMethod(cls: Class[_]): Boolean = {

    if (cls == null) return false
    if (classCache contains cls) return classCache(cls)

    val methods = cls.getDeclaredMethods().filter(m => m.getName() == "writeObject")
    val hasIt = methods.length > 0 ||
      classHasWriteObjectMethod(cls.getSuperclass()) ||
      cls.getInterfaces().exists(classHasWriteObjectMethod(_))
    classCache += (cls -> hasIt)
    hasIt
  }
}

/**
 * Scala has a bug where failure to deserialize a Scala HashMap when classpath jars aren't
 * correct can lead to very confusing and unhelpful error messages. This seems to be related to
 * the fact that a HashMap serializes to a HashMap.SerializationProxy, which appears to have a
 * bug when deserialization fails. Note that we cannot just use .asJava since that creates a
 * Scala JavaCollectionWrappers.MapWrapper, which has the same serialization issues.
 *
 * Fortunately, the Java HashMap does not have these serialization issues. So anywhere we have a
 * Scala HashMap that could easily fail deserialization, we should probably use a Java HashMap
 * instead, and do so by create a new Java HashMap instance and copying in the keys/values from
 * the Scala HashMap.
 *
 * To make to make it clear throughout the code where we actually do this, a new type alias is
 * created for a Java Map called "ProperlySerializableMap". Although the compiler cannot enforce
 * alias use instead of directly using a Java Map, as convention we should use this type alias
 * anywhere we are using a Map for correct serialization purposes. Additionally, an implicit
 * class with helper function is added to make it easy to correctly convert a Scala Map to a
 * ProperlySerializableMap. For example:
 *
 *     import org.apache.daffodil.lib.util.ProperlySerializableMap._
 *
 *     val myScalaMap = Map((1,2), (3,4))
 *     val serializableMap = myScalaMap.toProperlySerializableMap
 *
 * Note that the toProperlySerializableMap function creates a LinkedHashMap to guarantee
 * repeatable order.
 *
 * See DAFFODIL-228 for examples of the confusing error messages and details of the issue.
 */
object ProperlySerializableMap {

  type ProperlySerializableMap[K, V] = java.util.Map[K, V]

  implicit class DecoratedWithToProperlySerialableMap[K, V](m: Map[K, V]) {
    def toProperlySerializableMap: ProperlySerializableMap[K, V] = {
      // This LinkedHashMap constructor copies the keys/values from the Scala MapWrapper, so
      // anything using the result of this function will not have any Scala serialization issues
      new java.util.LinkedHashMap[K, V](m.asJava)
    }
  }
}
