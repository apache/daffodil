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

import org.apache.daffodil.dsom.IIUtils.IIMap
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.SeparatorSuppressionPolicyMixin
import org.apache.daffodil.schema.annotation.props.gen.Format_AnnotationMixin
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.xml.XMLUtils

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
  protected final override def enclosingComponentDefs = Seq()

}

/**
 * Handles everything about schema documents that has nothing to
 * do with DFDL. Things like namespace, include, import, elementFormDefault
 * etc.
 */
final class XMLSchemaDocument(
  xmlArg: Node,
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
    val hasAttributeFormDefault = (xml \ "@attributeFormDefault").text != ""
    schemaDefinitionWarningUnless(WarnID.UnsupportedAttributeSchemaLocation, !hasSchemaLocation, "schemaLocation is ignored.")
    schemaDefinitionWarningUnless(WarnID.UnsupportedAttributeBlockDefault, !hasBlockDefault, "blockDefault is ignored")
    schemaDefinitionWarningUnless(WarnID.UnsupportedAttributeFinalDefault, !hasFinalDefault, "finalDefault is ignored")
    schemaDefinitionWarningUnless(WarnID.UnsupportedAttributeFormDefault, !hasAttributeFormDefault, "attributeFormDefault is not part of DFDL and will be ignored")
    val res = hasSchemaLocation | hasBlockDefault | hasFinalDefault | hasAttributeFormDefault
    res
  }.value

  /**
   *  True if root xs:schema element has an xmlns that uses the DFDL URI.
   *
   *  This could be a prefix definition (Most likely xmlns:dfdl='...' but could
   *  be some other prefix.)
   *
   *  Or very very unlikely, it could be the default namespace.
   */
  private def hasDFDLNamespaceDefinition: Boolean = {
    val scope = xml.scope
    val pre = scope.getPrefix(XMLUtils.DFDL_NAMESPACE)
    val hasSomePrefixForDFDLNamespace = pre ne null
    lazy val hasDefaultNamespaceAsDFDLNamespace = {
      val defaultNS = scope.getURI(null)
      defaultNS == XMLUtils.DFDL_NAMESPACE.toString
    }
    val res = hasSomePrefixForDFDLNamespace || hasDefaultNamespaceAsDFDLNamespace
    res
  }

  /**
   * True if this is a DFDL schema that Daffodil should process.
   * False if this schema should be ignored because it has no DFDL annotations.
   *
   * We will ignore this import/include if it does not use the DFDL namespace
   * definition for a prefix (or the default) on the xs:schema element.
   *
   * We do this so that other annotation languages can co-exist with DFDL.
   * That is, they can use all of XML Schema including things DFDL doesn't allow
   * like attribute decls, but only when those schemas are only needed
   * to process non-DFDL annotations. Since Daffodil ignores non-DFDL annotations
   * entirely, Daffodil won't run into these non-DFDL allowed things like
   * attribute decls. But validators like Xerces will see the regular
   * import/include and process normally, which enables validation of
   * all annotations, DFDL and otherwise.
   *
   * Further discussion - see the comments on JIRA ticket DAFFODIL-1909
   */
  lazy val isDFDLSchema = hasDFDLNamespaceDefinition
}

/**
 * Handles only things specific to DFDL about schema documents.
 *
 * I.e., default format properties, named format properties, etc.
 */
final class SchemaDocument(xmlSDoc: XMLSchemaDocument)
  extends AnnotatedSchemaComponent
  with SchemaDocumentMixin
  with ResolvesProperties //
  // Technically, this is not a term, so shouldn't resolve scoped properties
  // This is mixed in to satisfy the contract of FindPropertyMixin, which requires
  // a lookup method that is defined in ResolveProperties.
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {

  final override val xml = xmlSDoc.xml
  final override def optLexicalParent = Option(xmlSDoc)
  final override lazy val xmlSchemaDocument = xmlSDoc

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
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDef(_, this) }
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
