package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.dpath.SuspendableExpression
import edu.illinois.ncsa.daffodil.processors.LengthEv
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.util.MaybeULong

class ElementOutputValueCalcStaticLengthUnparser(erd: ElementRuntimeData, repUnparser: Unparser, maybeKnownLengthInBits: MaybeULong)
  extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)

  def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //

    repUnparser.runtimeDependencies.foreach {
      _.evaluate(ustate) // these evaluations will force dependencies of the dependencies. So we just do 1 tier, not a tree walk.
    }

    val se = SuspendableExpression(diSimple, expr, ustate, repUnparser, maybeKnownLengthInBits)

    ustate.addSuspendedExpression(se)

  }
}

class ElementOutputValueCalcRuntimeLengthUnparser(erd: ElementRuntimeData, repUnparser: Unparser,
  lengthEv: LengthEv, lengthUnits: LengthUnits)
  extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  Assert.invariant(erd.outputValueCalcExpr.isDefined)
  val expr = erd.outputValueCalcExpr.get

  override lazy val childProcessors = Seq(repUnparser)
  override lazy val runtimeDependencies = Seq(lengthEv)

  def unparse(ustate: UState): Unit = {
    Assert.invariant(erd.outputValueCalcExpr.isDefined)

    val diSimple = ustate.currentInfosetNode.asSimple
    // note. This got attached to infoset in StatementElementOutputValueCalcUnparser

    //
    // Forces the evaluation of runtime-valued things, and this will cause those
    // that actually are runtime-expressions to be cached on the infoset element.
    //
    // Then later when the actual unparse occurs, these will be accessed off the
    // infoset element's cache.
    //

    repUnparser.runtimeDependencies.foreach {
      _.evaluate(ustate) // these evaluations will force dependencies of the dependencies. So we just do 1 tier, not a tree walk.
    }

    val length: Long = lengthEv.evaluate(ustate)

    val knownLengthInBits: Long = lengthUnits match {
      case LengthUnits.Bits => length
      case LengthUnits.Bytes => length * 8
      case LengthUnits.Characters => length * erd.encInfo.knownEncodingWidthInBits
    }

    val se = SuspendableExpression(diSimple, expr, ustate, repUnparser, MaybeULong(knownLengthInBits))

    ustate.addSuspendedExpression(se)

  }

}
