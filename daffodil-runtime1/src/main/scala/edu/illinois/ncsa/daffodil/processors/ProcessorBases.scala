package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util._

object Processor {
  /**
   * This initialize routine is really part of compiling the DFDL schema so should be called from there.
   * This insures that it is not, for example, being multi-threaded. If called from the runtime module, compilation might
   * actually end up happening at run time.
   */
  def initialize(proc: Processor) {
    ensureCompiled(proc)
  }

  private def ensureCompiled(proc: Processor) {
    if (!proc.isInitialized) {
      proc.isInitialized = true
      proc.runtimeDependencies.foreach { ensureCompiled }
      proc.childProcessors.foreach { ensureCompiled }
    }
  }

  private def ensureCompiled(ev: EvaluatableBase[AnyRef]) {
    ev.ensureCompiled
    ev.runtimeDependencies.foreach { ensureCompiled }
  }
}

trait Processor
  extends ToBriefXMLImpl
  with Logging
  with Serializable {
  // things common to both unparser and parser go here.
  def context: RuntimeData
  def childProcessors: Seq[Processor]
  def runtimeDependencies: Seq[Evaluatable[AnyRef]]

  var isInitialized: Boolean = false
}

trait PrimProcessor extends Processor {
  override def childProcessors: Seq[Processor] = Nil
}

trait BinaryParserUnparserRuntimeMixin {

  final protected def setupByteOrder(state: ParseOrUnparseState, erd: ElementRuntimeData, byteOrdEv: ByteOrderEv) {
    if (state.dataStream.isEmpty) return
    val dis = state.dataStream.get
    val byteOrd = byteOrdEv.evaluate(state)
    dis.setByteOrder(byteOrd)
    dis.setBitOrder(erd.defaultBitOrder)
  }
}

/**
 * BriefXML is XML-style output, but intended for specific purposes. It is NOT
 * an XML serialization of the data structure. It's an XML-style string, suitable to
 * manipulate, by people, in XML tooling. E.g., can stick into an XML editor to
 * then get it all indented nicely, use a structure editor to expand/collapse subregions,
 * but it is NOT intended to capture all of the state of the object.
 */
trait ToBriefXMLImpl {

  private lazy val nom_ : String = Misc.getNameFromClass(this)
  def nom = nom_

  protected def briefXMLAttributes: String = ""

  def childProcessors: Seq[Processor]

  // TODO: make this create a DOM tree, not a single string (because of size limits)
  def toBriefXML(depthLimit: Int = -1): String = {
    val eltStartText = nom + (if (briefXMLAttributes == "") "" else " " + briefXMLAttributes + " ")
    if (depthLimit == 0) "..."
    else if (childProcessors.length == 0) "<" + eltStartText + "/>"
    else {
      val lessDepth = depthLimit - 1
      val sb = new StringBuilder
      childProcessors.foreach {
        cp =>
          val s = cp.toBriefXML(lessDepth)
          if (sb.size < 3000) sb.append(s) // hack!
          else sb.append("...")
      }
      "<" + eltStartText + ">" + sb + "</" + nom + ">"
    }
  }

  override def toString = toBriefXML() // pParser.toString + " ~ " + qParser.toString
}
