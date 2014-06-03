package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node
import edu.illinois.ncsa.daffodil.ExecutionMode
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.PrimitiveFactoryBase
import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.xml.XMLUtils

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(xmlArg: Node, val parent: SchemaComponent)
  extends SchemaComponentBase(xmlArg, parent)
  with ImplementsThrowsSDE
  with GetAttributesMixin
  with SchemaComponentIncludesAndImportsMixin
  with ResolvesQNames
  with FindPropertyMixin
  with LookupLocation
  with PropTypes {

  lazy val primitiveFactory: PrimitiveFactoryBase = schemaSet.primitiveFactory
  lazy val prims = primitiveFactory

  val context: SchemaComponent = parent

  /**
   * Annotations can contain expressions, so we need to be able to compile them.
   *
   * We need our own instance so that the expression compiler has this schema
   * component as its context.
   */
  lazy val expressionCompiler = new ExpressionCompiler(this)

  /*
   * Anything non-annotated always returns property not found
   * 
   * Override in annotated components
   */
  def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode
    NotFound(Nil, Nil)
  }
  // FIXME: not sure why non-annotated schema components need to have findProperty
  // on them at all. Who would call it polymorphically, not knowing whether they 
  // have an annotated schema component or not?

  lazy val schemaFile: Option[DFDLSchemaFile] = parent.schemaFile
  lazy val schemaSet: SchemaSet = parent.schemaSet
  lazy val schemaDocument: SchemaDocument = parent.schemaDocument
  lazy val xmlSchemaDocument: XMLSchemaDocument = parent.xmlSchemaDocument
  lazy val schema: Schema = parent.schema
  override def schemaComponent: SchemaComponent = this
  override lazy val fileName: String = parent.fileName

  override lazy val isHidden: Boolean = isHidden_.value
  private val isHidden_ = LV('isHidden) {
    enclosingComponent match {
      case None => Assert.invariantFailed("Root global element should be overriding this.")
      case Some(ec) => ec.isHidden
    }
  }

  override def enclosingComponent = super.enclosingComponent.asInstanceOf[Option[SchemaComponent]]

  lazy val enclosingTerm: Option[Term] = {
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
    val p = scPath.map { _.prettyName }.mkString("::")
    p
  }

  override def toString = prettyName

  /**
   * Includes instances. Ie., a global element will appear inside an element ref.
   * a global group inside a group ref, a global type inside an element or for
   * derived simple types inside another simple type, etc.
   *
   * Used in diagnostic messages and code debug messages
   */

  lazy val scPath: Seq[SchemaComponent] = {
    val ec = enclosingComponent
    val scpOpt = ec.map {
      sc =>
        {
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
  def newDFDLAnnotationXML(label: String) = {
    //
    // This is not a "wired" dfdl prefix.
    // Rather, we are creating a new nested binding for the dfdl prefix to the right DFDL uri.
    // Which applies to this element and what is inside it only.
    // So we're indifferent to what the surrounding context might be using for namespace bindings.
    //
    val dfdlBinding = new scala.xml.NamespaceBinding("dfdl", XMLUtils.DFDL_NAMESPACE.toString, xml.scope)
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, dfdlBinding, true)
  }

  /**
   * Needed by back-end to evaluate expressions.
   */
  lazy val namespaces = {
    val res = XMLUtils.namespaceBindings(xml.scope)
    res
  }

}

/**
 * Local components
 */
trait LocalComponentMixin { self: SchemaComponent =>
  // nothing currently - all components have a parent.
}

/**
 * A schema is all the schema documents sharing a single target namespace.
 *
 * That is, one can write several schema documents which all have the
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema(val namespace: NS, schemaDocs: Seq[SchemaDocument], schemaSetArg: SchemaSet)
  extends SchemaComponent(<fake/>, schemaSetArg) {

  requiredEvaluations(schemaDocuments)

  override lazy val targetNamespace: NS = namespace

  override lazy val enclosingComponent = None
  override lazy val schemaDocument = Assert.usageError("schemaDocument should not be called on Schema")

  override lazy val schemaSet = schemaSetArg

  lazy val schemaDocuments = schemaDocs

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc) // exactly one is good
      case s => {
        schemaSet.schemaDefinitionError(
          "More than one definition for name: %s\n" +
            "defined %s",
          name, s.map { thing =>
            thing match {
              case df: DFDLDefiningAnnotation => df.asAnnotation.fileDescription
              case sc: SchemaComponent => sc.fileDescription
              case _ => Assert.invariantFailed("should only be a SchemaComponent or a DFDLDefiningAnnotation")
            }
          }.mkString("\n and also "))
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

/**
 * Used to encapsulate junk XML. E.g., if user gets prefix wrong, and types
 * xs:format instead of dfdl:format.
 *
 * We want this to be a schema component so we don't have to have another
 * mechanism for reporting file and line numbers and diagnostic stuff.
 */
class JunkComponent(xml: Node, sc: SchemaComponent) extends SchemaComponent(xml, sc)

