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

import edu.illinois.ncsa.daffodil.dsom.IIUtils.IIMap
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Format_AnnotationMixin

/**
 * A schema document corresponds to one file usually named with an ".xsd" extension.
 * The root element of a schema document is xsd:schema where xsd is the namespace for
 * XML Schema.
 *
 * A schema document is important because it is the unit of lexical scoping of format
 * properties in DFDL.
 *
 * Specifically, note that two schema documents may have the same target namespace, but
 * different default formats scoped over their contents.
 *
 * When dealing with plain XML and XSD (not DFDL), the concept of Schema with a single
 * target namespace is more used than the concept of schema document, which is usually
 * only needed to issue diagnostic error messages ("In file foo.xsd, line 938...")
 *
 * Conversely, for DFDL, the concept of schema document is more used because schema documents
 * are where default formats are specified, so it is very important what schema document
 * a schema component was defined within.
 */

/**
 * Common to both types we use for dealing with
 * schema documents.
 */
trait SchemaDocumentMixin { self: SchemaComponent =>

  protected final override def enclosingComponentDef: Option[SchemaComponent] = None

}

/**
 * Handles everything about schema documents that has nothing to
 * do with DFDL. Things like namespace, include, import, elementFormDefault
 * etc.
 */
final class XMLSchemaDocument(xmlArg: Node,
  schemaSetArg: SchemaSet,

  /**
   * ii is the include or import statement DSOM object that
   * lead to this schema document being imported/included.
   */
  val ii: Option[IIBase],
  sfArg: Option[DFDLSchemaFile],
  seenBeforeArg: IIMap,
  /**
   * this flag lets us import into a bootstrap 'fake' document
   * even though it does not have a namespace
   */
  override val isBootStrapSD: Boolean)
  extends SchemaComponentImpl(xmlArg, sfArg.getOrElse(schemaSetArg))
  with SchemaDocumentMixin
  with SchemaDocIncludesAndImportsMixin {

  requiredEvaluations(checkUnsupportedAttributes)

  final lazy val seenBefore = seenBeforeArg

  final override lazy val schemaFile = sfArg
  final override lazy val xmlSchemaDocument = this

  /**
   * Error checks on the xs:schema element itself.
   *
   * E.g., we don't support the xsi:schemaLocation attribute. So we issue a warning for that.
   * and some other attributes as well.
   */

  private def qualOrUnqual(str: String, kind: String) = {
    str match {
      case "unqualified" => str
      case "qualified" => str
      case _ => schemaDefinitionError("Unrecognized value for %s FormDefault='%s'.", kind, str)
    }
  }

  final lazy val elementFormDefault = {
    val efdAttr = (xml \ "@elementFormDefault").text
    if (efdAttr == "") "unqualified"
    else qualOrUnqual(efdAttr, "element")
  }

  final lazy val attributeFormDefault = {
    val afdAttr = (xml \ "@attributeFormDefault").text
    if (afdAttr == "") "unqualified"
    else qualOrUnqual(afdAttr, "attribute")
  }

  final def checkUnsupportedAttributes = LV('checkUnsupportedAttributes) {
    val hasSchemaLocation = (xml \ "@schemaLocation").text != ""
    val hasBlockDefault = (xml \ "@blockDefault").text != ""
    val hasFinalDefault = (xml \ "@finalDefault").text != ""
    schemaDefinitionWarningUnless(!hasSchemaLocation, "schemaLocation is ignored.")
    schemaDefinitionWarningUnless(!hasBlockDefault, "blockDefault is ignored")
    schemaDefinitionWarningUnless(!hasFinalDefault, "finalDefault is ignored")
    schemaDefinitionUnless(attributeFormDefault == "unqualified", "attributeFormDefault='qualified' is not yet implemented.")
    val res = hasSchemaLocation | hasBlockDefault | hasFinalDefault
    res
  }.value

}

/**
 * Handles only things specific to DFDL about schema documents.
 *
 * I.e., default format properties, named format properties, etc.
 */
final class SchemaDocument(xmlSDoc: XMLSchemaDocument)
  extends AnnotatedSchemaComponent
  with SchemaDocumentMixin
  with ResolvesProperties
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {

  final override val xml = xmlSDoc.xml
  final override def parent = xmlSDoc

  override lazy val optReferredToComponent = None

  // override def term = Assert.usageError("not to be called on SchemaDocument")

  /**
   * Implements the selectivity so that if you specify a root element
   * to the compiler, then only that root element (and things reached from it)
   * is compiled. Otherwise all top level elements are compiled.
   */
  requiredEvaluations(defaultFormat)
  if (schemaSet.checkAllTopLevel) {
    requiredEvaluations(globalElementDecls.map { _.forRoot() })
    requiredEvaluations(defineEscapeSchemes)
    requiredEvaluations(defineFormats)
    requiredEvaluations(defineVariables)
    // TODO: about defineVariables:
    // only include these if they have default values or external values.
    // Those then have to be evaluated before any processing,
    // and may depend on other variables with default values or external values.
    // Not these, because we'll pick these up when elements reference them.
    // And we don't compile them independently of that (since they could be very
    // incomplete and would lead to many errors for missing this or that.)
    //
    // Note: don't include these. They get checked if used.
    //    globalSimpleTypeDefs
    //    globalComplexTypeDefs
    //    globalGroupDefs
  }

  override lazy val schemaDocument = this

  override lazy val schema = schemaSet.getSchema(targetNamespace).getOrElse {
    Assert.invariantFailed("schema not found for schema document's namespace.")
  }

  //  lazy val shortFormAnnotationsAreValid: Boolean = {
  //    val dfdlns = XMLUtils.DFDL_NAMESPACE
  //    val attrs = xml.attributes
  //
  //
  //    // Check that any prefixed properties in the DFDL namespace are allowed on
  //    // this specific annotated schema component.
  //
  //    val dfdlAttrs = attrs.filter{ a => a.isPrefixed && a.}
  //  }

  //  def nonDefaultPropertySources = Seq()
  //
  //  def defaultPropertySources = LV('defaultPropertySources) {
  //    val seq = Seq(this.defaultFormatChain)
  //    seq
  //  }.value

  protected def annotationFactory(node: Node): Option[DFDLAnnotation] = {
    val res = node match {
      case <dfdl:format>{ content @ _* }</dfdl:format> => new DFDLFormat(node, this)
      case <dfdl:defineFormat>{ content @ _* }</dfdl:defineFormat> => new DFDLDefineFormat(node, this)
      case <dfdl:defineEscapeScheme>{ content @ _* }</dfdl:defineEscapeScheme> => new DFDLDefineEscapeSchemeFactory(node, this)
      case <dfdl:defineVariable>{ content @ _* }</dfdl:defineVariable> => new DFDLDefineVariable(node, this)
      case _ => {
        val prefix =
          if (node.prefix == null || node.prefix == "") ""
          else node.prefix + ":"
        this.SDE("Invalid dfdl annotation found: %s", prefix + node.label)
      }
    }
    Some(res)
  }

  protected def emptyFormatFactory = new DFDLFormat(newDFDLAnnotationXML("format"), this)
  protected def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

  /*
   * Design note about factories for global elements, and recursive types.
   *
   * The point of these factories is that every local site that uses a global def/decl
   * needs a copy so that the def/decl can have attributes which depend on the context
   * where it is used. That is, we can't share global defs/decls because the contexts change
   * their meaning.
   *
   * This works as is, so long as the DFDL Schema doesn't have recursion in it. Recursion would create
   * an infinite tree of local sites and copies. (There's an issue: DFDL-80 in Jira about putting
   * in the check to rule out recursion)
   *
   * But recursion would be a very cool experimental feature, potentially useful for investigations
   * towards DFDL v2.0 in the future.
   *
   * What's cool: if these factories are changed to memoize. That is, return the exact same global def/decl
   * object if they are called from the same local site, then recursion "just works". Nothing will diverge
   * creating infinite structures, but furthermore, the "contextual" information will be right. That
   * is to say, the first place some global structure is used is the "top" entry. It gets a copy.
   * If that global ultimately has someplace that recurses back to that global structure, it has to be from some other
   * local site inside it, so that's a different local site, so it will get a copy of the global. But
   * that's where it ends because the next "unwind" of the recursion will be at this same local site, so
   * would be returned the exact same def/decl object.
   *
   * Of course there are runtime/backend complexities also. Relative paths, variables with newVariableInstance
   * all of which can go arbitrarily deep in the recursive case.
   */
  lazy val globalElementDecls = {
    val xmlelts = (xml \ "element")
    val factories = xmlelts.map { new GlobalElementDeclFactory(_, this) }
    factories
  }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDefFactory(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDefFactory(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDefFactory(_, this) }

  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]

  lazy val defineFormats = annotationObjs.collect { case df: DFDLDefineFormat => df }
  lazy val defineEscapeSchemes = annotationObjs.collect { case des: DFDLDefineEscapeSchemeFactory => des }
  lazy val defineVariables = annotationObjs.collect { case dv: DFDLDefineVariable => dv }

  /**
   * by name getters for the global things that can be referenced.
   */
  def getGlobalElementDecl(name: String) = {
    val geds = globalElementDecls
    val res = geds.find { _.name == name }
    res
  }
  def getGlobalSimpleTypeDef(name: String) = globalSimpleTypeDefs.find { _.name == name }
  def getGlobalComplexTypeDef(name: String) = globalComplexTypeDefs.find { _.name == name }
  def getGlobalGroupDef(name: String) = globalGroupDefs.find { _.name == name }

  def getDefineFormat(name: String) = defineFormats.find {
    df =>
      val dfName = df.namedQName.local
      val res = dfName == name
      res
  }
  def getDefineVariable(name: String) = {
    val res = defineVariables.find { _.name == name }
    res
  }
  def getDefaultFormat = this.defaultFormat
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}
