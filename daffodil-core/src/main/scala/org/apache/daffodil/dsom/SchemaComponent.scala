/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.processors.NonTermRuntimeData
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.NonTermRuntimeData
import edu.illinois.ncsa.daffodil.xml.ResolvesQNames
import edu.illinois.ncsa.daffodil.schema.annotation.props.LookupLocation
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyLookupResult
import edu.illinois.ncsa.daffodil.schema.annotation.props.FindPropertyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropTypes
import edu.illinois.ncsa.daffodil.schema.annotation.props.NotFound
import edu.illinois.ncsa.daffodil.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.api.DaffodilTunables
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropertyLookupResult
import edu.illinois.ncsa.daffodil.schema.annotation.props.FindPropertyMixin
import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.PropTypes

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(xmlArg: Node, val parent: SchemaComponent)
  extends OOLAGHost(parent)
  with ImplementsThrowsOrSavesSDE
  with GetAttributesMixin
  with SchemaComponentIncludesAndImportsMixin
  with ResolvesQNames
  with FindPropertyMixin
  with SchemaFileLocatableImpl
  with PropTypes {
  
  def tunable: DaffodilTunables = parent.tunable

  val xml = xmlArg
  val aaa_xml = xml // for debugging, so we don't have to scroll down.

  lazy val dpathCompileInfo: DPathCompileInfo =
    new DPathCompileInfo(
      enclosingComponent.map { _.dpathCompileInfo },
      variableMap,
      namespaces,
      path,
      schemaFileLocation,
      tunable)

  val context: SchemaComponent = parent

  /**
   * ALl non-terms get runtimeData from this definition. All Terms
   * which are elements and model-groups) override this.
   *
   * The Term class has a generic termRuntimeData => TermRuntimeData
   * function (useful since all Terms share things like having charset encoding)
   * The Element classes all inherit an elementRuntimeData => ElementRuntimeData
   * and the model groups all have modelGroupRuntimeData => ModelGroupRuntimeData.
   *
   * There is also VariableRuntimeData and SchemaSetRuntimeData.
   */
  lazy val runtimeData: RuntimeData = nonTermRuntimeData // overrides in ModelGroup, ElementBase

  final def nonTermRuntimeData = LV('nonTermRuntimeData) {
    new NonTermRuntimeData(
      variableMap,
      schemaFileLocation,
      diagnosticDebugName,
      path,
      namespaces,
      enclosingElement.map { _.erd },
      Maybe.toMaybe(enclosingTerm.map { _.termRuntimeData }),
      tunable)
  }.value

  def variableMap: VariableMap = LV('variableMap) {
    schemaSet.variableMap
  }.value

  /*
   * Anything non-annotated always returns property not found
   *
   * Override in annotated components
   */
  def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode
    NotFound(Nil, Nil, pname)
  }
  // Q: not sure why non-annotated schema components need to have findProperty
  // on them at all. Who would call it polymorphically, not knowing whether they
  // have an annotated schema component or not?
  // A: DFDLAnnotation - the annotation objects themselves - aren't annotated
  // schema components - they have a relationship to an annotated schema component
  // but clearly they carry properties.

  def isHidden: Boolean = LV('isHidden) {
    enclosingComponent match {
      case None => Assert.invariantFailed("Root global element should be overriding this.")
      case Some(ec) => ec.isHidden
    }
  }.value

  lazy val schemaComponent: LookupLocation = this

  /**
   * All schema components except the root have an enclosing element.
   */
  final lazy val enclosingElement: Option[ElementBase] = LV('enclosingElement) {
    val et = enclosingTerm
    val ee = et match {
      case None => None
      case Some(eb: ElementBase) => Some(eb)
      case Some(sc: SchemaComponent) => sc.enclosingElement
    }
    ee
  }.value

  final lazy val rootElement: Option[GlobalElementDecl] = {
    enclosingElement match {
      case Some(e) => e.rootElement
      case None => {
        if (this.isInstanceOf[GlobalElementDecl])
          Some(this.asInstanceOf[GlobalElementDecl])
        else
          None
      }
    }
  }

  final lazy val enclosingTerm: Option[Term] = {
    enclosingComponent match {
      case None => None
      case Some(t: Term) => Some(t)
      case _ => enclosingComponent.get.enclosingTerm
    }
  }

  /**
   * path is used in diagnostic messages and code debug
   * messages; hence, it is very important that it be
   * very dependable.
   */
  override lazy val path = {
    val p = scPath.map { _.diagnosticDebugName }.mkString("::")
    p
  }

  /**
   * Elements only e.g., /foo/ex:bar
   */
  final lazy val slashPath: String = {
    val thisOne = "/" + diagnosticDebugName
    val encElem = enclosingElement
    if (encElem.isDefined)
      encElem.get.slashPath + thisOne
    else
      thisOne
  }

  override def toString = diagnosticDebugName

  /**
   * Includes instances. Ie., a global element will appear inside an element ref.
   * a global group inside a group ref, a global type inside an element or for
   * derived simple types inside another simple type, etc.
   *
   * Used in diagnostic messages and code debug messages
   */

  final lazy val scPath: Seq[SchemaComponent] = {
    val ec = enclosingComponent
    val scpOpt = ec.map {
      sc =>
        {
          Assert.invariant(sc != null)
          val parentPath = sc.scPath
          parentPath
        }
    }
    val res = scpOpt.getOrElse(Nil) :+ this
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
  extends SchemaComponent(<fake/>, schemaSetArg) {

  requiredEvaluations(schemaDocuments)

  override def targetNamespace: NS = namespace

  final override protected def enclosingComponentDef = None
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
              case sf: SchemaComponentFactory => sf.locationDescription
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
    // noneOrOne(schemaDocuments.flatMap { _.getGlobalElementDecl(name) }, name)
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
