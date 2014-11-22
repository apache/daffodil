package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Term

trait BitOrderMixin extends GrammarMixin { self: Term =>

  lazy val defaultBitOrder = {
    if (DaffodilTunableParameters.requireBitOrderProperty) {
      bitOrder
    } else {
      optionBitOrder.getOrElse(BitOrder.MostSignificantBitFirst)
    }
  }

  lazy val enclosingBitOrder = enclosingTerm.map(_.defaultBitOrder)
  lazy val priorSiblingBitOrder = priorSibling.map(_.defaultBitOrder)
  lazy val bitOrderBefore = priorSiblingBitOrder.getOrElse(enclosingBitOrder.getOrElse(defaultBitOrder))

  lazy val bitOrderChange = prod("bitOrderChange", this, bitOrderBefore != defaultBitOrder) { new BitOrderChange(this) }
}

