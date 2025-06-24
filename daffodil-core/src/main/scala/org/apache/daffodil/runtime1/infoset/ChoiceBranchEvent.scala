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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.UniquenessCache
import org.apache.daffodil.lib.xml.NamedQName

object ChoiceBranchEvent
  extends UniquenessCache[NamedQName, (ChoiceBranchStartEvent, ChoiceBranchEndEvent)] {

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
