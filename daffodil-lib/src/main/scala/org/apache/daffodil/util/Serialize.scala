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

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert
import scala.collection.mutable.HashMap

trait PreSerialization extends Serializable {

  val me = this.getClass()
  Assert.usage(PreSerialization.classHasWriteObjectMethod(me), String.format("Class %s does not implement the method writeObject.", me.getName()))

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
        Assert.abort("Could not serialize member of class %s, found within class %s".format(ns.getMessage(), Misc.getNameFromClass(this)))
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
