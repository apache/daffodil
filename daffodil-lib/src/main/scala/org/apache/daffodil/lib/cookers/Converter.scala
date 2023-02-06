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

package org.apache.daffodil.lib.cookers

import org.apache.daffodil.lib.exceptions.ThrowsSDE

/*
 * Quick tutorial on -Before +After.
 *
 * Use of -Before makes it contravariant.
 *
 * To convince myself, I'm going to walk through the contravariance "thing".....
 *
 * An example. Let's assume JByte <: JShort <: JInt. (reminder <: is the scala "is a subtype of" operator)
 *
 * Now, given a Converter[JShort, String], the convert method takes a JShort and creates a String.
 *
 * If I want to create a derived Converter2[X, String] <: Converter[JShort, String] what can type X be?
 *
 * For one converter to be a subtype of another, the derived one has to be able to substitute for the base anywhere that base occurs.
 *
 * This means that the convert method of the derived must accept at least a JShort (which is what the convert of the base class accepts), but
 * convert of the derived may be more general, and accept more than that.
 *
 * Hence Converter[JInt, String] <: Converter[JShort, String] even though JInt >: JShort
 *
 * That's what contravariance means. To create a subtype it doesn't narrow with the derivation, it broadens with the derivation.
 *
 * So yes I'm convinced Converter should be Converter[-Before, +After]
 */
trait Converter[-Before, +After] extends Serializable {

  /**
   * Override these runtime and constant-specific variants if you need different
   * conversions done if an expression is evaluated at runtime.
   *
   * This is for things like delimiters or escapeCharacter where it can be "" if
   * it is a constant, but it cannot be "" if it is a runtime expression value.
   *
   * The forUnparse flag is for distinctions needed between parse time and unparse time
   * which is very common thing to have.
   *
   * This is not the same concept as Compile time vs. Runtime.
   */
  def convertRuntime(b: Before, context: ThrowsSDE, forUnparse: Boolean): After =
    convert(b, context, forUnparse)

  def convertConstant(b: Before, context: ThrowsSDE, forUnparse: Boolean): After =
    convert(b, context, forUnparse)

  /**
   * Override this if there is just one conversion used for both constants and
   * for runtime expressions.
   */
  protected def convert(b: Before, context: ThrowsSDE, forUnparse: Boolean): After

}
