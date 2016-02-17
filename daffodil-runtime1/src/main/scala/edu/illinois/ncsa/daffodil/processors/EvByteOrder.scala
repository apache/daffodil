package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.equality._
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import com.ibm.icu.util.GregorianCalendar
import com.ibm.icu.util.TimeZone
import com.ibm.icu.util.ULocale

/**
 * Runtime valued properties that are enums would all work like ByteOrder here.
 */
class ByteOrderEv(expr: CompiledExpression[String], erd: ElementRuntimeData)
  extends EvaluatableConvertedExpression[String, ByteOrder](
    expr,
    ByteOrder,
    erd) {
  override def runtimeDependencies = Nil

}

/**
 * Singleton Ok is for returning from checks and such which will either throw SDE
 * or succeed. It means "check succeeded"
 */
class Ok private () extends Serializable {
  override def toString = "Ok"
}
object Ok extends Ok()

class CheckByteAndBitOrderEv(t: TermRuntimeData, bitOrder: BitOrder, maybeByteOrder: Maybe[ByteOrderEv])
  extends Evaluatable[Ok](t) { // can't use unit here, not <: AnyRef

  override lazy val runtimeDependencies =
    if (maybeByteOrder.isEmpty) Nil
    else maybeByteOrder.value +: maybeByteOrder.value.runtimeDependencies

  final protected def compute(state: ParseOrUnparseState): Ok = {
    if (maybeByteOrder.isEmpty) return Ok
    val byteOrderEv = maybeByteOrder.get
    val byteOrder = byteOrderEv.evaluate(state)
    bitOrder match {
      case BitOrder.MostSignificantBitFirst => // ok
      case BitOrder.LeastSignificantBitFirst =>
        if (byteOrder =:= ByteOrder.BigEndian) {
          t.schemaDefinitionError("Bit order 'leastSignificantBitFirst' requires byte order 'littleEndian', but byte order was '%s'.", byteOrder)
        }
    }
    Ok
  }
}
