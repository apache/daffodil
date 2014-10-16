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

  final protected def serializeObject(out: java.io.ObjectOutputStream) {
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
