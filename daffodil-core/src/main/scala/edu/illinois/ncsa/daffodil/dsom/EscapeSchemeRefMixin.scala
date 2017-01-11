/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.api.DaffodilTunableParameters

trait EscapeSchemeRefMixin { self: AnnotatedSchemaComponent =>
  /**
   * Changed to use findProperty, and to resolve the namespace properly.
   *
   * We lookup a property like escapeSchemeRef, and that actual property
   * binding can be local, in scope, by way of a format reference, etc.
   *
   * It's value is a QName, and the definition of the prefix is from the
   * location where we found the property, and NOT where we consume the property.
   *
   * Hence, we resolve w.r.t. the location that provided the property.
   *
   * The point of findProperty vs. getProperty is just that the former returns
   * both the value, and the object that contained it. That object is what
   * we resolve QNames with respect to.
   *
   * Note: Same is needed for properties that have expressions as their values.
   * E.g., consider "{ ../foo:bar/.. }". That foo prefix must be resolved relative
   * to the object where this property was written, not where it is evaluated. (JIRA
   * issue DFDL-77)
   */
  final lazy val optionEscapeScheme: Option[DFDLEscapeScheme] = {
    val er = findPropertyOption("escapeSchemeRef")
    er match {
      case _: NotFound => {
        SDW(DaffodilTunableParameters.WarnID.EscapeSchemeRefUndefined,
          "Property escapeSchemeRef was undefined. Please add escapeSchemeRef='' to your schema.")
        None
      }
      case Found("", _, _, _) => None // empty string means no escape scheme
      case Found(qName, loc, _, _) => {
        val qn = loc.resolveQName(qName) // loc is where we resolve the QName prefix.
        val defESFactory = schemaSet.getDefineEscapeScheme(qn)
        //
        // We have escape scheme factories because an escape schema can have
        // expressions (for escapeCharacter and escapeEscapeCharacter), and
        // those need to be compiled for each context where the escape scheme
        // is referenced, not just once.
        //
        defESFactory match {
          case None => SDE("Define Escape Scheme %s Not Found", qName)
          case Some(desf) => Some(desf.forComponent(this).escapeScheme)
        }
      }
    }
  }

}
