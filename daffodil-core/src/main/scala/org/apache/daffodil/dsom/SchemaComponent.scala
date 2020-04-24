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

package org.apache.daffodil.dsom

import scala.xml.Node
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.NonTermRuntimeData
import org.apache.daffodil.xml.ResolvesQNames
import org.apache.daffodil.schema.annotation.props.LookupLocation
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.xml.GetAttributesMixin
import org.apache.daffodil.schema.annotation.props.PropTypes
import org.apache.daffodil.util.Misc
import org.apache.daffodil.BasicComponent

abstract class SchemaComponentImpl(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent])
  extends SchemaComponent {

  def this(xml: Node, lexicalParent: SchemaComponent) =
    this(xml, Option(lexicalParent))
}

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
trait SchemaComponent
  extends BasicComponent
  with ImplementsThrowsOrSavesSDE
  with GetAttributesMixin
  with SchemaComponentIncludesAndImportsMixin
  with ResolvesQNames
  with SchemaFileLocatableImpl
  with PropTypes {

  def xml: Node

  override def oolagContextViaArgs = optLexicalParent

  requiredEvaluationsIfActivated(runtimeData)
  requiredEvaluationsIfActivated(runtimeData.preSerializationOnlyOnce)

  override lazy val tunable: DaffodilTunables = optLexicalParent.get.tunable
  final override lazy val unqualifiedPathStepPolicy = tunable.unqualifiedPathStepPolicy

  lazy val dpathCompileInfo: DPathCompileInfo = {
    lazy val parents = enclosingTerms.map { _.dpathCompileInfo }
    new DPathCompileInfo(
      parents,
      variableMap,
      namespaces,
      path,
      schemaFileLocation,
      tunable.unqualifiedPathStepPolicy,
      schemaSet.typeCalcMap,
      runtimeData)
  }

  /**
   * Abbreviation. We use this very often.
   */
  final def ci = dpathCompileInfo

  /**
   * All non-terms get runtimeData from this definition. All Terms
   * which are elements and model-groups) override this.
   *
   * The Term class has a generic termRuntimeData => TermRuntimeData
   * function (useful since all Terms share things like having charset encoding)
   * The Element classes all inherit an elementRuntimeData => ElementRuntimeData
   * and the model groups all have modelGroupRuntimeData => ModelGroupRuntimeData.
   *
   * There is also VariableRuntimeData and SchemaSetRuntimeData.
   */
  lazy val runtimeData: RuntimeData = nonTermRuntimeData // overrides in ModelGroup, ElementBase, SimpleTypes

  final def nonTermRuntimeData = LV('nonTermRuntimeData) {
    new NonTermRuntimeData(
      variableMap,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      tunable.unqualifiedPathStepPolicy)
  }.value

  def variableMap: VariableMap = LV('variableMap) {
    schemaSet.variableMap
  }.value


  lazy val schemaComponent: LookupLocation = this

  /**
   * Elements that enclose this.
   *
   * If this is already an element, this still walks outward to find the
   * next tier out.
   */
  final lazy val enclosingElements: Seq[ElementBase] = LV('enclosingElements) {
    val ets = enclosingTerms
    if (ets.isEmpty) {
      // a root element (in test situations there can be several) has no enclosing term
      val r = this.schemaSet.root
      if (this eq r) Nil
      else {
        // A simpleType that is used by way of an expression like:
        // ```dfdl:inputValueCalc="{ dfdlx:inputTypeCalc('tns:simpleTypeName', ...) }" ```
        // Also has no enclosing terms. (Currently)
        this match {
          case st: GlobalSimpleTypeDef if st.optRepType.isDefined => Nil // no enclosingElements
          case _ => {
            // This does happen for some tests in daffodil-core
            // That use the file example-of-most-dfdl-constructs.dfdl.xml.
            // This may not be valid.
            Seq(r)
          }
        }
      }
    } else {
      val res = ets.flatMap { et =>
        val ee = et match {
          case eb: ElementBase => Seq(eb)
          case sc: SchemaComponent => {
            val scee = sc.enclosingElements
            scee
          }
        }
        ee
      }
      res
    }
  }.value

  /**
   * The terms that can enclose this.
   *
   * Even if this is already a term, this walks outward to find
   * those enclosing this.
   */
  final lazy val enclosingTerms: Seq[Term] = {
    val ec = enclosingComponents.map { _.encloser }
    val res = ec.flatMap { sc =>
      sc match {
        case t: Term => Seq(t)
        case sd: SchemaDocument => Assert.invariantFailed("enclosing component should never be a schema document for " + this)
        case other => other.enclosingComponents.map { _.encloser }.flatMap {
          case t: Term => Seq(t)
          case x => x.enclosingTerms
        }
      }
    }
    res
  }

  /**
   * path is used in diagnostic messages and code debug
   * messages; hence, it is very important that it be
   * very dependable.
   */
  override lazy val path = {
    val list = scPath.filter { isComponentForSSCD(_) }
    val p = list.map { _.diagnosticDebugName }.mkString("::")
    p
  }

  private def isComponentForSSCD(sc: SchemaComponent) = {
    sc match {
      case _: SchemaDocument => false
      case _: XMLSchemaDocument => false
      case _: DFDLSchemaFile => false
      case _: Schema => false
      case _: SchemaSet => false
      case _ => true
    }
  }

  lazy val shortSchemaComponentDesignator: String = {
    val list = scPath.filter { isComponentForSSCD(_) }
    val sscdStrings = list.map { sc =>
      sc match {
        case er: AbstractElementRef => "er" + (if (er.position > 1) er.position else "") + "=" + er.refQName.toQNameString
        case e: ElementBase => "e" + (if (e.position > 1) e.position else "") + "=" +
          e.namedQName.toQNameString
        case ed: GlobalElementDecl => "e=" + ed.namedQName.toQNameString
        case ct: GlobalComplexTypeDef => "ct=" + ct.namedQName.toQNameString
        case ct: ComplexTypeBase => "ct"
        case st: SimpleTypeDefBase => "st=" + st.namedQName.toQNameString
        case st: SimpleTypeBase => "st=" + st.primType.globalQName.toQNameString
        case cgr: ChoiceGroupRef => "cgr" + (if (cgr.isHidden) "h" else "") + (if (cgr.position > 1) cgr.position else "") + "=" + cgr.groupDef.namedQName.toQNameString
        case cgd: GlobalChoiceGroupDef => "cgd=" + cgd.namedQName.toQNameString
        case sgr: SequenceGroupRef => "sgr" + (if (sgr.isHidden) "h" else "") + (if (sgr.position > 1) sgr.position else "") + "=" + sgr.groupDef.namedQName.toQNameString
        case sgd: GlobalSequenceGroupDef => "sgd=" + sgd.namedQName.toQNameString
        case cg: Choice => "c" + (if (cg.position > 1) cg.position else "")
        case sg: Sequence => "s" + (if (sg.position > 1) sg.position else "")
        case unknown => "unk=" + Misc.getNameFromClass(unknown)
      }
    }
    val sscd = sscdStrings.mkString(":")
    sscd
  }

  final def sscd = shortSchemaComponentDesignator

  /**
   * Elements only e.g., /foo/ex:bar
   */
  final lazy val slashPath: String =
    scPath.filter { _.isInstanceOf[ElementBase] }.map { _.diagnosticDebugName }.mkString("/")

  override def toString = diagnosticDebugName

  /**
   * Does not include instances. Ie., walks up lexical nest only.
   *
   * This implies that to fully distinguish context for diagnostic messages
   * we will need to use the node stack in the state of the processor to provide
   * the dynamic nest of elements, as at schema compile time we only have
   * the lexical nest.
   *
   * Used in diagnostic messages and code debug messages.
   */
  private lazy val scPath: Seq[SchemaComponent] = {
    val res = optLexicalParent.map { _.scPath }.getOrElse(Nil) :+ this
    res
  }

  /**
   * the easiest way to get an empty metadata object.
   */
  private val scala.xml.Elem(_, _, emptyXMLMetadata, _, _*) = <foo/>

  /**
   * Used as factory for the XML Node with the right namespace and prefix etc.
   *
   * Given "element" it creates <dfdl:element /> with the namespace definitions
   * based on this schema component's corresponding XSD construct.
   *
   * Makes sure to inherit the scope so we have all the namespace bindings.
   */
  protected final def newDFDLAnnotationXML(label: String) = {
    //
    // This is not a "wired" dfdl prefix.
    // Rather, we are creating a new nested binding for the dfdl prefix to the right DFDL uri.
    // Which applies to this element and what is inside it only.
    // So we're indifferent to what the surrounding context might be using for namespace bindings.
    //
    val dfdlBinding = new scala.xml.NamespaceBinding("dfdl", XMLUtils.DFDL_NAMESPACE.toString, xml.scope)
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, dfdlBinding, true)
  }
}

/**
 * A schema is all the schema documents sharing a single target namespace.
 *
 * That is, one can write several schema documents which all have the
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
final class Schema(val namespace: NS, schemaDocs: Seq[SchemaDocument], schemaSetArg: SchemaSet)
  extends SchemaComponentImpl(<fake/>, Option(schemaSetArg)) {

  requiredEvaluationsAlways(schemaDocuments)

  override def targetNamespace: NS = namespace

  override lazy val schemaDocument: SchemaDocument = Assert.usageError("schemaDocument should not be called on Schema")

  override lazy val schemaSet = schemaSetArg

  lazy val schemaDocuments = schemaDocs

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc) // exactly one is good
      case s => {
        schemaSet.schemaDefinitionError(
          "More than one definition for name: %s. Defined in following locations:\n%s",
          name, s.map { thing =>
            thing match {
              case df: DFDLDefiningAnnotation => df.asAnnotation.locationDescription
              case sc: SchemaComponent => sc.locationDescription
              case _ => Assert.impossibleCase(thing)
            }
          }.mkString("\n"))
      }
    }
  }

  /**
   * Given a name, retrieve the appropriate object.
   *
   * This just scans each schema document in the schema, checking each one.
   */
  def getGlobalElementDecl(name: String) = {
    val sds = schemaDocuments
    val res = sds.flatMap {
      sd =>
        {
          val ged = sd.getGlobalElementDecl(name)
          ged
        }
    }
    noneOrOne(res, name)
  }
  def getGlobalSimpleTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalSimpleTypeDef(name) }, name)
  def getGlobalComplexTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalComplexTypeDef(name) }, name)
  def getGlobalGroupDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalGroupDef(name) }, name)
  def getDefineFormat(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineFormat(name) }, name)
  def getDefineFormats() = schemaDocuments.flatMap { _.defineFormats }
  def getDefineVariable(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineVariable(name) }, name)
  def getDefineEscapeScheme(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineEscapeScheme(name) }, name)
  def getDefaultFormat = schemaDocuments.flatMap { x => Some(x.getDefaultFormat) }

  // used for bulk checking of uniqueness

  lazy val globalElementDecls = schemaDocuments.flatMap(_.globalElementDecls)
  lazy val globalGroupDefs = schemaDocuments.flatMap(_.globalGroupDefs)
  lazy val globalSimpleTypeDefs = schemaDocuments.flatMap(_.globalSimpleTypeDefs)
  lazy val globalComplexTypeDefs = schemaDocuments.flatMap(_.globalComplexTypeDefs)
  lazy val defineFormats = schemaDocuments.flatMap(_.defineFormats)
  lazy val defineEscapeSchemes = schemaDocuments.flatMap(_.defineEscapeSchemes)
  lazy val defineVariables = schemaDocuments.flatMap(_.defineVariables)

}
