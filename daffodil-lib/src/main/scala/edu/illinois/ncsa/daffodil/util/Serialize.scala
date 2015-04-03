/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.mutable.HashMap

trait PreSerialization extends Serializable {

  val me = this.getClass()
  Assert.usage(PreSerialization.classHasWriteObjectMethod(me), String.format("Class %s does not implement the method writeObject.", me.getName()))

  protected def preSerialization: Any = {
    // nothing by default
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

  protected final def serializeObject(out: java.io.ObjectOutputStream) {
    try {
      // println("serializing " + Misc.getNameFromClass(this)) // good place for a breakpoint
      preSerialization
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
