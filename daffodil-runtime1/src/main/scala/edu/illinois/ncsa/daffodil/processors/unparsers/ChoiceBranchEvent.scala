package edu.illinois.ncsa.daffodil.processors.unparsers

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