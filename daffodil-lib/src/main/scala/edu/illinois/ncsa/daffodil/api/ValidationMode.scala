package edu.illinois.ncsa.daffodil.api

import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.HasIsError
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Enum

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

/**
 * Base trait for all error, warning, info, and other sorts of objects
 * that capture diagnostic information.
 *
 * Allows for lazy message creation, internationalization, etc.
 */
/**
 * Simple logging system evolved from code found on Stack Overflow, on the web.
 * http://stackoverflow.com/questions/2018528/logging-in-scala
 * Mostly based on the contribution of Don Mackenzie.
 */

object ValidationMode extends Enum {
  import edu.illinois.ncsa.daffodil.japi.{ ValidationMode => JValidationMode }
  sealed abstract class Type protected (val jVMode: JValidationMode) extends EnumValueType with Ordered[Type] with Serializable {
    val id = jVMode.id
    def compare(that: ValidationMode.Type) = this.id - that.id
  }
  case object Off extends Type(JValidationMode.Off); Off
  case object Limited extends Type(JValidationMode.Limited); Limited
  case object Full extends Type(JValidationMode.Full); Full

  // 
  // Very annoying, but if this values list is here, the obvious place for it
  // then initialization doesn't work right if the very first thing that happens
  // is someone calls LogLevel.fromJava. 
  //
  // In that case, the values.find returns the enum object, but that
  // enum object hasn't been initialized properly so you get a null pointer exception.
  //
  // private val values: List[Type] = List(Error, Warning, Info, Compile, Debug, OOLAGDebug)
  /**
   * We want scala code to use the typesafe enum idiom which actually
   * uses case objects as above. But that's not reachable from java,
   * so we provide this conversion from the plain Java enum.
   */
  def fromJava(jVMode: JValidationMode): ValidationMode.Type = {
    //
    // Seems like we should hoist this constant list out of this method.
    // Do not. It causes problems with object initialization.
    //
    val values: List[Type] = List(Off, Limited, Full)
    Assert.usage(jVMode != null)
    values.find { _.id == jVMode.id }.getOrElse(Assert.abort("unmapped: java enum has no corresponding scala enum"))
  }

  def forJava(vMode: ValidationMode.Type): JValidationMode = {
    vMode.jVMode
  }
}