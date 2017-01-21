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

package edu.illinois.ncsa.daffodil.infoset

import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.util.UniquenessCache
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc

object ChoiceBranchEvent extends UniquenessCache[NamedQName, (ChoiceBranchStartEvent, ChoiceBranchEndEvent)] {

  override def apply(nqn: NamedQName) = {
    Assert.usage(nqn != null)
    super.apply(nqn)
  }

  protected def valueFromKey(nqn: NamedQName) = {
    Assert.usage(nqn != null)
    (new ChoiceBranchStartEvent(nqn), new ChoiceBranchEndEvent(nqn))
  }

  protected def keyFromValue(pair: (ChoiceBranchStartEvent, ChoiceBranchEndEvent)) = {
    Assert.usage(pair != null)
    Some(pair._1.qname)
  }
}

sealed trait ChoiceBranchEvent extends Serializable {
  val qname: NamedQName

  override def toString = Misc.getNameFromClass(this) + "(" + qname + ")"

  override def hashCode = qname.hashCode

}

class ChoiceBranchStartEvent(val qname: NamedQName) extends ChoiceBranchEvent {
  override def equals(x: Any) = {
    x match {
      case x: ChoiceBranchStartEvent => x.qname == qname
      case _ => false
    }
  }
}
object ChoiceBranchStartEvent {
  def apply(nqn: NamedQName) = ChoiceBranchEvent(nqn)._1
}
class ChoiceBranchEndEvent(val qname: NamedQName) extends ChoiceBranchEvent {
  override def equals(x: Any) = {
    x match {
      case x: ChoiceBranchEndEvent => x.qname == qname
      case _ => false
    }
  }
}
object ChoiceBranchEndEvent {
  def apply(nqn: NamedQName) = ChoiceBranchEvent(nqn)._2
}