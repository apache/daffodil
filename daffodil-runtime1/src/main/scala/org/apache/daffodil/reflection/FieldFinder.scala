/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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


//
// Please leave this code here for now.
// It's unused, but documents how to use reflection, many of its foibles,
// and in other sitautions we may want to use these techniques.
//

//package org.apache.daffodil.reflection
//
//import scala.reflect.runtime.{ universe => ru }
//import ru._
//import scala.reflect.ClassTag
//import org.apache.daffodil.util.Maybe
//import org.apache.daffodil.exceptions.Assert
//
///**
// * Uses reflection to find all members of a class having a particular type.
// */
//object FieldFinder {
//
//  //  private def typesOf[T: TypeTag](v: T): List[Type] =
//  //    typeOf[T].baseClasses.map(typeOf[T].baseType)
//
//  /**
//   * The object passed as argument a, must be an instance of a top-level class.
//   * It cannot be a nested class, class inside an object, or class inside a method.
//   *
//   * It returns a pair. The first of the pair is the list of the member values.
//   * The second of the pair is a list of strings, each describing a problem case
//   * where there is a member-like thing that has the right type, but it isn't something
//   * where reflection (or anything else) can get its value after the fact. (It's implemented
//   * by scala as a private value of the constructor method only for the class. Since the
//   * constructor isn't running anymore, this value is just "gone".)
//   */
//  def findAllMembersOfType[T](a: AnyRef)(implicit ttag: TypeTag[T]) = {
//    val tpe = ttag.tpe
//    val rm = scala.reflect.runtime.currentMirror
//    val clsSym = rm.classSymbol(a.getClass)
//    val clsType = clsSym.toType
//    val allClasses = clsType.baseClasses
//    val allTypes = allClasses.map { _.asType.toType }
//    val allMembers = allTypes.flatMap { ty => ty.members.sorted }
//    val members = allMembers.filter { _.isTerm } // just terms, not types
//    val im = rm.reflect(a)
//    val allMethods = members.collect { case x if x.isMethod => x.asMethod }
//    val allGetters = allMethods.collect { case m if m.isGetter => m.getter.asMethod }
//    val getters = allGetters.filter { g =>
//      g.typeSignature.resultType <:< tpe
//    }
//    val getterValues: Seq[T] = getters.map { case x => im.reflectMethod(x).apply().asInstanceOf[T] }
//
//    //
//    // The above takes care of most cases, but if a member is declared as just a constructor
//    // parameter that is then used somewhere inside the class, that doesn't show up as
//    // a method at all. So we have to go after this in the non-methods.
//
//    val nonMethods = members.collect { case x if !x.isMethod => x }
//    //
//    // These can be slots of things that are accessed by getters we already have taken
//    // care of, so we exclude the ones we already have gotten
//    //
//    val gotten = getters.collect { case m if m.isAccessor => m.accessed }.toSeq
//    //    if (a.getClass.getName().contains("ConvertTextCalendarUnparser")) {
//    //      println("Stop here")
//    //    }
//    //
//    // Unfortunately, the symbol you get back in the gotten list is
//    // not the same object one gets in the non-methods list even when they
//    // are referring to the same slot/location.
//    //
//    // So we just compare these based on the string representation
//    val gottenStrings = gotten.map { _.toString }
//    val allWanted = nonMethods.filterNot { term => gottenStrings.contains(term.toString) }
//    val wanted = allWanted.filter { w =>
//      w.typeSignature <:< tpe
//    }
//    //
//    // Some of these things are fields, and we can get their values via reflection
//    // others are not.
//    //
//    // I can't find any test/predicate or class/type distinction that will tell us
//    // what we can get the value of, and what we can't. So we just try and catch
//    // the failure if the value can't be obtained.
//    //
//    val wantedValues: Seq[T] = wanted.flatMap {
//      case x => {
//        try {
//          val field = im.reflectField(x.asTerm)
//          val result = field.get.asInstanceOf[T]
//          List(result)
//        } catch {
//          case re: ScalaReflectionException => Nil
//        }
//      }
//    }
//
//    //
//    // That's all the values
//    //
//    val values = (getterValues ++ wantedValues).distinct
//
//    //
//    // The things that have the right type, but we can't get their values
//    // by reflection, well we probably want to warn or throw errors about
//    // these so that the user is informed they should make these into
//    // var, val, lazy val, or just use them in the class someplace.
//    // That is, so they won't get missed.
//    //
//    // Again the only way I know of to detect these is to try to get
//    // their values, and when that fails, that's one of the ones we
//    // want to warn about.
//    //
//    val warnTerms = wanted.flatMap {
//      case x => {
//        try {
//          im.reflectField(x.asTerm).get
//          Nil
//        } catch {
//          case re: ScalaReflectionException => List(x)
//        }
//      }
//    }
//
//    val warnings = warnTerms.map { _.toString }.distinct
//
//    (values, warnings)
//  }
//
//}
