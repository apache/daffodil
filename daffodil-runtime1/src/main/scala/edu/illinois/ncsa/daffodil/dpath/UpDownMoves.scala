package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.dsom.DPathElementCompileInfo
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.processors.DIElement
import edu.illinois.ncsa.daffodil.processors.InfosetNoSuchChildElementException

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
    val arr = savedCurrentElement.getChildArray(info.slotIndexInParent, info.name, info.namedQName.namespace)
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
    val arr = now.getChildArray(info.slotIndexInParent, info.name, info.namedQName.namespace)
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
    val arr = now.getChildArray(info.slotIndexInParent, info.name, info.namedQName.namespace)

    if (!arr.isDefined || arr.get.length == 0) throw new InfosetNoSuchChildElementException("Array does not exist.")
  }

  override def toXML = {
    toXML(info.name)
  }

}
