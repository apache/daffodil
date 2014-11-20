package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.DFDLCalendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLTime
import AsIntConverters._

case object ToRoot extends RecipeOp {
  override def run(dstate: DState) {
    var now = dstate.currentElement
    var parent = now.diParent
    while (parent.isDefined) {
      now = parent.get
      parent = now.diParent
    }
    dstate.setCurrentNode(now)
  }
}
case object SelfMove extends RecipeOp {
  override def run(dstate: DState) {
    // do this entirely so it will fail at constant compile time
    // also serves as a sort of assertion check.
    dstate.selfMove()
  }
}

case object UpMove extends RecipeOp {
  override def run(dstate: DState) {
    val now = dstate.currentElement
    val n = now.diParent.getOrElse(
      Assert.invariantFailed(
        "UpMove past root. Should never happen since an expression like that won't typecheck statically."))
    dstate.setCurrentNode(n)
  }
}

/**
 * Down to a non-array element. Can be optional or scalar.
 */
case class DownElement(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    // TODO PE ? if doesn't exist should be a processing error.
    // It will throw and so will be a PE, but may be poor diagnostic.
    dstate.setCurrentNode(now.getChild(info.slotIndexInParent, info.name, info.namedQName.namespace).asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(info.name)
  }

}

/**
 * Move down to an occurrence of an array element.
 */
case class DownArrayOccurrence(info: DPathElementCompileInfo, indexRecipe: CompiledDPath)
  extends RecipeOpWithSubRecipes(indexRecipe) {

  override def run(dstate: DState) {
    val savedCurrentElement = dstate.currentComplex
    indexRecipe.run(dstate)
    val index = dstate.index
    Assert.invariant(dstate.index > 0) // TODO PE?
    val arr = savedCurrentElement.getChildArray(info.slotIndexInParent)
    Assert.invariant(arr.isDefined) // TODO PE?
    val occurrence = arr.get.getOccurrence(index) // will throw on out of bounds
    dstate.setCurrentNode(occurrence.asInstanceOf[DIElement])
  }

  override def toXML = {
    toXML(new scala.xml.Text(info.name) ++ indexRecipe.toXML)
  }

}

/*
 * down to an array object containing all occurrences
 */
case class DownArray(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(info.slotIndexInParent)
    Assert.invariant(arr.isDefined)
    dstate.setCurrentNode(arr.get.asInstanceOf[DIArray])
  }

  override def toXML = {
    toXML(info.name)
  }

}

case class DownArrayExists(info: DPathElementCompileInfo) extends RecipeOp {

  override def run(dstate: DState) {
    val now = dstate.currentComplex
    val arr = now.getChildArray(info.slotIndexInParent)

    if (!arr.isDefined || arr.isEmpty) throw new InfosetNoSuchChildElementException("Array does not exist.")
  }

  override def toXML = {
    toXML(info.name)
  }

}
