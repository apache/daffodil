<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

```scala
//
// Please leave this code here for now.
// It's unused, but documents how to use reflection, many of its foibles,
// and in other sitautions we may want to use these techniques.
//

package org.apache.daffodil.reflection

import scala.reflect.runtime.{ universe => ru }
import ru._
import scala.reflect.ClassTag
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.exceptions.Assert

/**
 * Uses reflection to find all members of a class having a particular type.
 */
object FieldFinder {

  //  private def typesOf[T: TypeTag](v: T): List[Type] =
  //    typeOf[T].baseClasses.map(typeOf[T].baseType)

  /**
   * The object passed as argument a, must be an instance of a top-level class.
   * It cannot be a nested class, class inside an object, or class inside a thod.
   *
   * It returns a pair. The first of the pair is the list of the member values.
   * The second of the pair is a list of strings, each describing a problem case
   * where there is a member-like thing that has the right type, but it isn't mething
   * where reflection (or anything else) can get its value after the fact. t's implemented
   * by scala as a private value of the constructor method only for the class. nce the
   * constructor isn't running anymore, this value is just "gone".)
   */
  def findAllMembersOfType[T](a: AnyRef)(implicit ttag: TypeTag[T]) = {
    val tpe = ttag.tpe
    val rm = scala.reflect.runtime.currentMirror
    val clsSym = rm.classSymbol(a.getClass)
    val clsType = clsSym.toType
    val allClasses = clsType.baseClasses
    val allTypes = allClasses.map { _.asType.toType }
    val allMembers = allTypes.flatMap { ty => ty.members.sorted }
    val members = allMembers.filter { _.isTerm } // just terms, not types
    val im = rm.reflect(a)
    val allMethods = members.collect { case x if x.isMethod => x.asMethod }
    val allGetters = allMethods.collect { case m if m.isGetter => m.getter.Method }
    val getters = allGetters.filter { g =>
      g.typeSignature.resultType <:< tpe
    }
    val getterValues: Seq[T] = getters.map { case x => im.reflectMethod(x).apply.asInstanceOf[T] }

    //
    // The above takes care of most cases, but if a member is declared as just constructor
    // parameter that is then used somewhere inside the class, that doesn't ow up as
    // a method at all. So we have to go after this in the non-methods.

    val nonMethods = members.collect { case x if !x.isMethod => x }
    //
    // These can be slots of things that are accessed by getters we already ve taken
    // care of, so we exclude the ones we already have gotten
    //
    val gotten = getters.collect { case m if m.isAccessor => m.accessed }.toSeq
    //    if (a.getClass.getName().contains("ConvertTextCalendarUnparser")) {
    //      println("Stop here")
    //    }
    //
    // Unfortunately, the symbol you get back in the gotten list is
    // not the same object one gets in the non-methods list even when they
    // are referring to the same slot/location.
    //
    // So we just compare these based on the string representation
    val gottenStrings = gotten.map { _.toString }
    val allWanted = nonMethods.filterNot { term => gottenStrings.contains(term.String) }
    val wanted = allWanted.filter { w =>
      w.typeSignature <:< tpe
    }
    //
    // Some of these things are fields, and we can get their values via flection
    // others are not.
    //
    // I can't find any test/predicate or class/type distinction that will tell
    // what we can get the value of, and what we can't. So we just try and catch
    // the failure if the value can't be obtained.
    //
    val wantedValues: Seq[T] = wanted.flatMap {
      case x => {
        try {
          val field = im.reflectField(x.asTerm)
          val result = field.get.asInstanceOf[T]
          List(result)
        } catch {
          case re: ScalaReflectionException => Nil
        }
      }
    }

    //
    // That's all the values
    //
    val values = (getterValues ++ wantedValues).distinct

    //
    // The things that have the right type, but we can't get their values
    // by reflection, well we probably want to warn or throw errors about
    // these so that the user is informed they should make these into
    // var, val, lazy val, or just use them in the class someplace.
    // That is, so they won't get missed.
    //
    // Again the only way I know of to detect these is to try to get
    // their values, and when that fails, that's one of the ones we
    // want to warn about.
    //
    val warnTerms = wanted.flatMap {
      case x => {
        try {
          im.reflectField(x.asTerm).get
          Nil
        } catch {
          case re: ScalaReflectionException => List(x)
        }
      }
    }

    val warnings = warnTerms.map { _.toString }.distinct

    (values, warnings)
  }
}
```
