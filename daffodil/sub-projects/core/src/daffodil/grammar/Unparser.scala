package daffodil.grammar
import daffodil.schema.annotation.props.PropertyMixin
import daffodil.exceptions.Assert

trait Unparser {

  def unparse(ustate: UState): UState

}

class UState {
  // TBD
}

case class DummyUnparser(sc: PropertyMixin) extends Unparser {
  def unparse(ustate: UState): UState = Assert.abort("Unparser for " + sc + " is not yet implemented.")
  override def toString = if (sc == null) "Dummy[null]" else "Dummy[" + sc.detailName + "]"
}