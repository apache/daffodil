/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.dsom

import org.apache.daffodil.exceptions.Assert

trait NestingMixin {

  /** The lexically enclosing schema component */
  def parent: SchemaComponent

  /**
   * Define this for schema components that have back-references to ref 
   * objects.
   * So group def to group ref, globalelementdecl to element ref,
   * type to element, base type to derived type.
   * 
   * Not for format annotations however. We don't backpoint those to 
   * other format annotations that ref them.
   */
  protected def enclosingComponentDef: Option[SchemaComponent]

  /**
   * The enclosing component, and follows back-references
   *  from types to their elements, from globalElementDef to
   *  elementRefs, from simpleType defs to derived simpletype defs,
   *  from global group defs to group refs
   *
   *  Note: the enclosing component of a global element or global group
   *  referenced from a element ref or group ref, is NOT the ref object, but the
   *  component that contains the ref object
   */
  final lazy val enclosingComponent: Option[SchemaComponent] = {
    val optECD = enclosingComponentDef
    optECD match {
      case Some(eref: ElementRef) => eref.enclosingComponent
      case Some(gref: GroupRef) => gref.enclosingComponent
      case _ => optECD
    }
  }
}

/**
 * Mixin for all schema factories and schema components with no
 * backpointers, just a lexical parent. This means all the non-global
 * schema components.
 */
trait NestingLexicalMixin
  extends NestingMixin {

  override protected def enclosingComponentDef =
    if (parent eq null) None else Some(parent)

}

/**
 * Mixin for all global schema components
 */
trait NestingTraversesToReferenceMixin
  extends NestingMixin {

  def referringComponent: Option[SchemaComponent]

  final override protected def enclosingComponentDef: Option[SchemaComponent] = {
    Assert.invariant(parent.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    referringComponent
  }

}
