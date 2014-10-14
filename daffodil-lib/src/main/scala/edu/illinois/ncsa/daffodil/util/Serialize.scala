package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.collection.mutable.HashMap

trait PreSerialization {
  this: Serializable =>
  
  val me = this.getClass()
  Assert.usage(PreSerialization.classHasWriteObjectMethod(me), String.format("Class %s does not implement the method writeObject.", me.getName()))
  
  def preSerialization: Any
}


private object PreSerialization {
  private val classCache = new HashMap[Class[_], Boolean]

  //
  // This private method ensures that any class (or any super class) that use this trait also implements the writeObject method
  // used by the Java serialization framework.
  //
  private def classHasWriteObjectMethod (cls: Class[_]): Boolean = {

    if (cls == null) return false
    if (classCache contains cls) return classCache(cls)
    
    val methods = cls.getDeclaredMethods().filter(m => m.getName() == "writeObject")
    val hasIt = methods.length > 0 || classHasWriteObjectMethod(cls.getSuperclass())
    classCache += (cls -> hasIt)
    hasIt
  }
}
