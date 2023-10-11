package org.apache.daffodil.japi.schema

import org.apache.daffodil.japi.{ DataProcessor => JDataProcessor }
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.processors.{
  ChoiceRuntimeData,
  DataProcessor => SDataProcessor,
  ElementRuntimeData,
  ErrorERD,
  SequenceRuntimeData,
  TermRuntimeData,
}

/**
 * Base class used by japi clients who want to walk the runtime schema information.
 */
abstract class RuntimeSchemaHandler() {

  /**
   * Called for simple type element declarations.
   *
   * @param qnameString
   * @param primTypeName
   * @param isArray only true if more than one occurrence is possible. Disjoint with isOptional
   * @param isOptional true if 0 or 1 occurrence only. Allows for a different representation of optional from a 0..1 array
   */
  def elementSimple(
    qnameString: String,
    primTypeName: String,
    isArray: Boolean,
    isOptional: Boolean,
  ): Unit

  /**
   * Called for complex type element declarations
   *
   * Subsequent calls will be for the model group making up the content
   * of the element.
   *
   * @param qnameString
   */
  def startElementComplex(qnameString: String, isArray: Boolean, isOptional: Boolean): Unit
  def endElementComplex(qnameString: String, isArray: Boolean, isOptional: Boolean): Unit

  def startSequence(): Unit

  def endSequence(): Unit

  def startChoice(): Unit

  def endChoice(): Unit

}

/**
 * Walks the schema, but not the DSOM schema, it walks the RuntimeData objects that
 * represent the DFDL schema at runtime.
 *
 * @param dp
 */
class RuntimeSchemaWalker(private val dp: SDataProcessor) {

  // provided so that people can write various tests from JAPI only.
  // we wouldn't need this otherwise.
  def this(jdp: JDataProcessor) = this(jdp.getUnderlyingDataProcessor)

  private lazy val rootERD = dp.ssrd.elementRuntimeData

  def walk(handler: RuntimeSchemaHandler): Unit = {
    walkTerm(handler, rootERD)
  }

  private def walkTerm(handler: RuntimeSchemaHandler, trd: TermRuntimeData): Unit = {
    trd match {
      case err: ErrorERD => Assert.invariantFailed("should not get ErrorERDs")
      case erd: ElementRuntimeData => walkElement(handler, erd)
      case srd: SequenceRuntimeData => walkSequence(handler, srd)
      case crd: ChoiceRuntimeData => walkChoice(handler, crd)
      case _ => Assert.invariantFailed(s"unrecognized TermRuntimeData subtype: $trd")
    }
  }

  private def walkElement(handler: RuntimeSchemaHandler, erd: ElementRuntimeData): Unit = {
    if (erd.optComplexTypeModelGroupRuntimeData.isDefined)
      walkComplexElement(handler, erd)
    else
      walkSimpleElement(handler, erd)
  }

  private def walkComplexElement(
    handler: RuntimeSchemaHandler,
    erd: ElementRuntimeData,
  ): Unit = {
    val qname = erd.namedQName.toQNameString
    val isArray = erd.isArray
    val isOptional = erd.isOptional
    val mgrd = erd.optComplexTypeModelGroupRuntimeData.getOrElse {
      Assert.invariantFailed("not a complex type element")
    }
    handler.startElementComplex(qname, isArray, isOptional)
    walkTerm(handler, mgrd)
    handler.endElementComplex(qname, isArray, isOptional)
  }

  private def walkSimpleElement(
    handler: RuntimeSchemaHandler,
    erd: ElementRuntimeData,
  ): Unit = {
    val qname = erd.namedQName.toQNameString
    val isArray = erd.isArray
    val isOptional = erd.isOptional
    val primType: PrimType = erd.optPrimType.getOrElse {
      erd.optSimpleTypeRuntimeData.map { _.primType }.get
    }
    handler.elementSimple(qname, primType.toString, isArray, isOptional)
  }

  private def walkSequence(handler: RuntimeSchemaHandler, srd: SequenceRuntimeData): Unit = {
    handler.startSequence()
    srd.groupMembers.map { trd =>
      walkTerm(handler, trd)
    }
    handler.endSequence()
  }

  private def walkChoice(handler: RuntimeSchemaHandler, crd: ChoiceRuntimeData): Unit = {
    handler.startChoice()
    crd.groupMembers.map { trd =>
      walkTerm(handler, trd)
    }
    handler.endChoice()
  }

}
