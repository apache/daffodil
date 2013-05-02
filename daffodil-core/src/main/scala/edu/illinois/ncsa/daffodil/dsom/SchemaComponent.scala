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

import scala.xml._
import scala.xml.parsing._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.grammar._
import com.ibm.icu.charset.CharsetICU
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.util.Compile
import edu.illinois.ncsa.daffodil.processors.charset.USASCII7BitPackedCharset
import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.compiler.RootSpec
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import java.io.File
import java.net.URI
import java.net.URL
import edu.illinois.ncsa.daffodil.dsom.IIUtils._
import IIUtils._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.ExecutionMode

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(xmlArg: Node, parent: SchemaComponent)
  extends SchemaComponentBase(xmlArg, parent)
  with ImplementsThrowsSDE
  with GetAttributesMixin
  with SchemaComponentIncludesAndImportsMixin
  with ResolvesQNames
  with FindPropertyMixin
  with LookupLocation
  with PropTypes {

  val context: SchemaComponent = parent

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

  override def enclosingComponent: Option[SchemaComponent] =
    if (parent != null) Some(parent) else None

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
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, dfdlBinding)
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
 * Local components have a lexical parent that contains them
 */
trait LocalComponentMixin { self: SchemaComponent =>
  def parent: SchemaComponent

}
/**
 * Common Mixin for things that have a name attribute.
 */
trait NamedMixin
  extends GetAttributesMixin { self: SchemaComponentBase =>
  override def prettyName = Misc.getNameFromClass(this) + "(" + name + ")"

  requiredEvaluations(name)

  lazy val name = nameFromNameAttribute
  private lazy val nameFromNameAttribute = nameFromNameAttribute_.valueOrElse("??name??")
  private val nameFromNameAttribute_ = LV('nameFromNameAttribute) { getAttributeRequired("name") }

  def xml: Node
  def schemaDocument: SchemaDocument

  lazy val namespace = schemaDocument.targetNamespace // can be "" meaning no namespace
  lazy val prefix = {
    val prefix = xml.scope.getPrefix(namespace.toString) // can be null meaning no prefix
    // cannot be ""
    prefix
  }
}

/**
 * All global components share these characteristics.
 * The difference between this and the not-Global flavor
 * has to do with the elementFormDefault attribute of the xs:schema
 * element. Global things are always qualified
 */
trait GlobalComponentMixin
  extends NamedMixin { self: SchemaComponentBase =>

}

/**
 * elementFormDefault is an attribute of the xs:schema element.
 * It defaults to 'qualified'. That means nested local element definitions,
 * their names are in the target namespace. So, if you have
 * @example {{{
 * <schema elementFormDefault='qualified'
 *         targetNamespace="myURI" xmlns:tns="myURI"...>
 * <element name='foo'...>
 *    <complexType>
 *       <sequence>
 *          <element name='bar'.../>
 *       ...
 * }}}
 *  Now a DFDL/Xpath expression to reach that 'bar' element looks like /tns:foo/tns:bar
 *  Contrarywise, if elementFormDefault='unqualfied'...
 *  <pre>
 *  <schema elementFormDefault='unqualified'
 *         targetNamespace="myURI" xmlns:tns="myURI"...>
 * <element name='foo'...>
 *    <complexType>
 *       <sequence>
 *          <element name='bar'.../>
 *       ...
 * }}}
 * Now a path to reach element bar would look like /tns:foo/bar.
 *
 * See how 'bar' isn't preceded by the tns prefix. That's becasue the child elements are
 * all 'no namespace' elements.
 *
 * This also affects what a result document is like from namespaces perspective.
 * Suppose the above 'bar' element is an xs:int. Then with elemenFormDefault='qualified', an
 * instance would look like:
 * @example {{{
 * <tns:foo><tns:bar>42</tns:bar></tns:foo>
 * }}}
 * or the possibly nicer (for a large result)
 * @example {{{
 * <foo xmlns="myURI"><bar>42</bar></foo>
 * }}}
 * But if elementFormDefault='unqualified', the instance doc would be like:
 * @example {{{
 * <tns:foo><bar>42</bar></tns:foo>
 * }}}
 * In this case you really don't want to setup xmlns='myURI' because this happens:
 * @example {{{
 *  <foo xmlns="myURI><bar xmlns="">42</bar></foo>
 * }}}
 * That is, you must explicitly go to the no-namespace syntax. It doesn't happen implicitly.
 *
 * This trait is mixed into things that are affected by elementFormDefault.
 * Namely the local element declaration class.
 */
trait ElementFormDefaultMixin
  extends NamedMixin { self: SchemaComponent =>

  /**
   * handle elementFormDefault to qualify
   */
  override lazy val namespace =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      NoNamespace // unqualified means no namespace
    else xmlSchemaDocument.targetNamespace

  override lazy val prefix =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      "" // unqualified means no prefix
    else {
      //
      // name is supposed to be qualified by the target namespace
      //
      val tns = namespace
      // record this error on the schemaDocument
      xmlSchemaDocument.schemaDefinitionUnless(tns != "", "Must have a targetNamespace if elementFormDefault='qualified'.")
      val prefix = {
        val existingPrefix = xml.scope.getPrefix(tns.toString)
        if (existingPrefix != null) existingPrefix
        else {
          // There is no prefix bound to this namespace
          // So we have to create a prefix. Let's try "tns", "tns1", "tns2" etc. until
          // we find one that is not bound to a namespace.
          val newPrefix = (0 until Int.MaxValue).flatMap { i =>
            // flatMap collapses the List(None, None, Some(tryPre),...) => List(tryPre,...)
            // then we just take head of this list, and we get tryPre
            // Note: this does NOT create the giant list of all Int values.
            val uniqueSuffix = if (i == 0) "" else i.toString
            val prefixStem = Compiler.generatedNamespacePrefixStem
            val tryPre = prefixStem + uniqueSuffix
            if (xml.scope.getURI(tryPre) == null)
              Some(tryPre) // tryPre is not bound to a namespace, so we can use it.
            else None
          }.head
          newPrefix
        }
      }
      prefix
    }

}

/**
 * Shared characteristics of any annotated schema component.
 * Not all components can carry DFDL annotations.
 */

abstract class AnnotatedSchemaComponent(xml: Node, sc: SchemaComponent)
  extends SchemaComponent(xml, sc)
  with AnnotatedMixin {

  requiredEvaluations(annotationObjs, shortFormPropertiesCorrect, nonDefaultPropertySources, defaultPropertySources)

  //  /**
  //   * only used for debugging
  //   */
  //  override lazy val properties: PropMap =
  //    (nonDefaultPropertySources.flatMap { _.properties.toSeq } ++
  //      defaultPropertySources.flatMap { _.properties.toSeq }).toMap

  final lazy val shortFormPropertiesCorrect: Boolean = {
    // Check that any unprefixed properties are disjoint with ALL DFDL property names.
    // Warning otherwise
    // Insure that some prefix is bound to the dfdl namespace. Warn otherwise.
    // Warn if dfdl: is bound to something else than the DFDL namespace. 
    shortFormAnnotationsAreValid
  }

  /**
   * Since validation of extra attributes on XML Schema elements is
   * normally lax validation, we can't count on validation of DFDL schemas
   * to tell us whether short-form annotations are correct or not.
   *
   * So, we have to do this check ourselves.
   *
   * TBD: change properties code generator to output the various lists of
   * properties that we have to check against. (Might already be there...?)
   *
   */
  def shortFormAnnotationsAreValid: Boolean = true

  def nonDefaultPropertySources: Seq[ChainPropProvider]

  def defaultPropertySources: Seq[ChainPropProvider]

  lazy val nonDefaultFormatChain: ChainPropProvider = formatAnnotation.getFormatChain()
  lazy val defaultFormatChain: ChainPropProvider = schemaDocument.formatAnnotation.getFormatChain()

  private def findDefaultOrNonDefaultProperty(
    pname: String,
    sources: Seq[ChainPropProvider]): PropertyLookupResult = {
    val seq = sources.map { _.chainFindProperty(pname) }
    val optFound = seq.collectFirst { case found: Found => found }
    val result = optFound match {
      case Some(f @ Found(_, _)) => f
      case None => {
        // merge all the NotFound stuff.
        val nonDefaults = seq.flatMap {
          case NotFound(nd, d) => nd
          case _: Found => Assert.invariantFailed()
        }
        val defaults = seq.flatMap {
          case NotFound(nd, d) => d
          case _: Found => Assert.invariantFailed()
        }
        Assert.invariant(defaults.isEmpty)
        val nf = NotFound(nonDefaults, defaults)
        nf
      }
    }
    result
  }

  private def findNonDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, nonDefaultPropertySources)
    result match {
      case f: Found => f
      case NotFound(nd, d) =>
        Assert.invariant(d.isEmpty)
    }
    result
  }

  private def findDefaultProperty(pname: String): PropertyLookupResult = {
    val result = findDefaultOrNonDefaultProperty(pname, defaultPropertySources)
    val fixup = result match {
      case f: Found => f
      case NotFound(nd, d) =>
        Assert.invariant(d.isEmpty)
        NotFound(Seq(), nd) // we want the places we searched shown as default locations searched
    }
    fixup
  }

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    ExecutionMode.requireCompilerMode
    // first try in regular properties
    val regularResult = findNonDefaultProperty(pname)
    regularResult match {
      case f: Found => f
      case NotFound(nonDefaultLocsTried1, defaultLocsTried1) => {
        Assert.invariant(defaultLocsTried1.isEmpty)
        val defaultResult = findDefaultProperty(pname)
        defaultResult match {
          case f: Found => f
          case NotFound(nonDefaultLocsTried2, defaultLocsTried2) => {
            Assert.invariant(nonDefaultLocsTried2.isEmpty)
            // did not find it at all. Return a NotFound with all the places we 
            // looked non-default and default.
            val nonDefaultPlaces = nonDefaultLocsTried1
            val defaultPlaces = nonDefaultLocsTried2
            NotFound(nonDefaultPlaces, defaultPlaces)
          }
        }
      }
    }
  }
}

/**
 * Every component that can be annotated.
 * Review Note:
 * It's no longer clear that this separation is strictly speaking needed.
 * It's possible that this could be collapsed back into AnnotatedSchemaComponent
 * or made smaller anyway.
 *
 */
trait AnnotatedMixin
  extends CommonRuntimeValuedPropertiesMixin
  with EncodingMixin
  with EscapeSchemeRefMixin { self: AnnotatedSchemaComponent =>

  def xml: Node
  def prettyName: String
  def path: String

  /**
   * Anything annotated must be able to construct the
   * appropriate DFDLAnnotation object from the xml.
   */
  def annotationFactory(node: Node): DFDLAnnotation

  lazy val annotationNode = {
    val ann = xml \ "annotation"
    ann
  }

  /**
   * dais = Dfdl App Info nodeSeq
   */
  lazy val dais = {
    val ais = (annotationNode \ "appinfo")
    val dais = ais.filter { ai =>
      {
        ai.attribute("source") match {
          case None => false
          case Some(n) => {
            val str = n.text
            val hasRightSource = (str == "http://www.ogf.org/dfdl/dfdl-1.0/")
            val isAcceptable = str.startsWith("http://www.ogf.org/dfdl")
            (hasRightSource || isAcceptable) // TODO: remove lax check once examples & tests are updated.
          }
        }
      }
    }
    dais
  }

  /**
   * The DFDL annotations on the component, as objects
   * that are subtypes of DFDLAnnotation.
   */
  lazy val annotationObjs = annotationObjs_.value
  private val annotationObjs_ = LV('annotationObjs) {
    // println(dais)
    dais.flatMap { dai =>
      {
        val children = dai.child
        val res = children.filterNot { _.isInstanceOf[Text] }.map { child =>
          {
            annotationFactory(child)
          }
        }
        res
      }
    }
  }

  /**
   * Here we establish an invariant which is that every annotatable schema component has, definitely, has an
   * annotation object. It may have no properties on it, but it will be there. Hence, we can
   * delegate various property-related attribute calculations to it.
   *
   * To realize this, every concrete class must implement (or inherit) an implementation of
   * emptyFormatFactory, which constructs an empty format annotation,
   * and isMyFormatAnnotation which tests if an annotation is the corresponding kind.
   *
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  def emptyFormatFactory: DFDLFormatAnnotation
  def isMyFormatAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation = formatAnnotation_.value
  private val formatAnnotation_ = LV('formatAnnotation) {
    val format = annotationObjs.collect { case fa: DFDLFormatAnnotation if isMyFormatAnnotation(fa) => fa }
    val res = format match {
      case Seq() => emptyFormatFactory // does make things with the right namespace scopes attached!
      case Seq(fa) => fa
      case _ => schemaDefinitionError("Only one format annotation is allowed at each annotation point.")
    }
    res
  }

  /**
   * Annotations can contain expressions, so we need to be able to compile them.
   *
   * We need our own instance so that the expression compiler has this schema
   * component as its context.
   */
  lazy val expressionCompiler = new ExpressionCompiler(this)

  lazy val justThisOneProperties = formatAnnotation.justThisOneProperties

}

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can have
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait DFDLStatementMixin extends ThrowsSDE { self: AnnotatedSchemaComponent =>

  requiredEvaluations(statements)

  def annotationFactoryForDFDLStatement(node: Node, self: AnnotatedSchemaComponent): DFDLAnnotation = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => new DFDLAssert(node, self)
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => new DFDLDiscriminator(node, self)
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => new DFDLSetVariable(node, self)
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => new DFDLNewVariableInstance(node, self)
      case _ => SDE("Invalid DFDL annotation found: %s", node)
    }
  }

  /**
   * Validation won't check whether these are validly in place on a DFDL schema, so
   * we allow any annotated object to have them, and then we can do checking on this list
   * to enforce rules about which kinds of statements are allowed and where.
   *
   * Implement these abstract methods to do the right thing w.r.t. combining
   * statements from group refs and their referenced groups, element refs and their elements,
   * element decls and their simple types, simpleTypes and their base simpleTypes.
   *
   * The local ingredients are here for doing the needed combining and also for checking.
   * E.g., dfdl:newVariableInstance isn't allowed on simpleType, can only have one discriminator per
   * annotation point, and per combined annotation point, discriminators and assertions exclude each other, etc.
   */
  def statements: Seq[DFDLStatement]
  def newVariableInstanceStatements: Seq[DFDLNewVariableInstance]
  final lazy val notNewVariableInstanceStatements = setVariableStatements ++ discriminatorStatements ++ assertStatements
  def assertStatements: Seq[DFDLAssert]
  def discriminatorStatements: Seq[DFDLDiscriminator]
  def setVariableStatements: Seq[DFDLSetVariable]

  final lazy val localStatements = this.annotationObjs.collect { case st: DFDLStatement => st }
  final lazy val localNewVariableInstanceStatements = localStatements.collect { case nve: DFDLNewVariableInstance => nve }
  final lazy val localNotNewVariableInstanceStatements = localStatements.diff(localNewVariableInstanceStatements)
  final lazy val (localDiscriminatorStatements,
    localAssertStatements) = {
    val discrims = localStatements.collect { case disc: DFDLDiscriminator => disc }
    val asserts = localStatements.collect { case asrt: DFDLAssert => asrt }
    checkDiscriminatorsAssertsDisjoint(discrims, asserts)
  }

  final def checkDiscriminatorsAssertsDisjoint(discrims: Seq[DFDLDiscriminator], asserts: Seq[DFDLAssert]): (Seq[DFDLDiscriminator], Seq[DFDLAssert]) = {
    schemaDefinitionUnless(discrims.size <= 1, "At most one discriminator allowed at same location: %s", discrims)
    schemaDefinitionUnless(asserts == Nil || discrims == Nil,
      "Cannot have both dfdl:discriminator annotations and dfdl:assert annotations at the same location.")
    (discrims, asserts)
  }

  final def checkDistinctVariableNames(svs: Seq[DFDLSetVariable]) = {
    val names = svs.map { _.defv.extName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinitionUnless(areAllDistinct, "Variable names must all be distinct at the same location: %s", names)
    svs
  }

  final lazy val localSetVariableStatements = {
    val svs = localStatements.collect { case sv: DFDLSetVariable => sv }
    checkDistinctVariableNames(svs)
  }

}

/**
 * A schema set is exactly that, a set of schemas. Each schema has
 * a target namespace (or 'no namespace'), so a schema set is
 * conceptually a mapping from a namespace URI (or empty string, meaning no
 * namespace) onto schema.
 *
 * Constructing these from XML Nodes is a unit-test
 * interface. The real constructor takes a sequence of file names,
 * and you can optionally specify a root element via the rootSpec argument.
 *
 * A schema set is a SchemaComponent (derived from that base), so as to inherit
 * the error/warning accumulation behavior that all SchemaComponents share.
 * A schema set invokes our XML Loader, which can produce validation errors, and
 * those have to be gathered so we can give the user back a group of them, not
 * just one.
 *
 * Schema set is however, a kind of a fake SchemaComponent in that it
 * doesn't correspond to any user-specified schema object. And unlike other
 * schema components obviously it does not live within a schema document.
 */
class SchemaSet(
  schemaFilesArg: Seq[File],
  rootSpec: Option[RootSpec] = None,
  checkAllTopLevelArg: Boolean = false,
  parent: SchemaComponent = null)
  extends SchemaComponent(<schemaSet/>, parent) // a fake schema component
  with SchemaSetIncludesAndImportsMixin {

  requiredEvaluations(
    isValid,

    if (checkAllTopLevel) {
      checkForDuplicateTopLevels()
      this.allTopLevels
    })

  override lazy val schemaSet = this
  // These things are needed to satisfy the contract of being a schema component.
  override lazy val enclosingComponent = None
  override lazy val schemaDocument = Assert.usageError("schemaDocument should not be called on SchemaSet")

  /**
   * Let's use the filename for the first schema document, rather than giving no information at all.
   * 
   * It would appear that this is only used for informational purposes
   * and as such, doesn't need to be a URL.  Can just be String.
   */
  override lazy val fileName = schemaFilesArg(0).getPath()

  lazy val schemaFiles = schemaFilesArg
  lazy val checkAllTopLevel = checkAllTopLevelArg

  override def warn(th: Diagnostic) = oolagWarn(th)
  override def error(th: Diagnostic) = oolagError(th)

  /**
   * This constructor for unit testing only
   */
  def this(sch: Node, rootNamespace: String = null, root: String = null) =
    this({
      val file = XMLUtils.convertNodeToTempFile(sch)
      val files = List(file)
      files
    },
      if (root == null) None else {
        if (rootNamespace == null) Some(RootSpec(None, root))
        else Some(RootSpec(Some(NS(rootNamespace)), root))
      },
      false)

  /**
   * Registry used to map from infoset nodes back to the schema components
   * that made them.
   */

  lazy val schemaComponentRegistry = new SchemaComponentRegistry()

  lazy val isValid = {
    val isV = OOLAG.keepGoing(false) {
      val files = allSchemaFiles
      val fileValids = files.map { _.isValid }
      val res = fileValids.length > 0 && fileValids.fold(true) { _ && _ }
      res
    }
    isV
  }

  lazy val validationDiagnostics = {
    val files = allSchemaFiles
    val res = files.flatMap { _.validationDiagnostics }
    res
  }

  lazy val schemas = schemas_.value
  private val schemas_ = LV('schemas) {
    val schemaPairs = allSchemaDocuments.map { sd => (sd.targetNamespace, sd) }
    //
    // groupBy is deterministic if the hashCode of the key element is deterministic.
    // our NS objects hashCode is same as their underlying string.
    //
    // Alas, being deterministic doesn't mean it is in an order we expect.
    // but at least it is deterministic.
    val schemaGroups = schemaPairs.groupBy { _._1 } // group by the namespace identifier
    val schemas = schemaGroups.map {
      case (ns, pairs) => {
        val sds = pairs.map { case (ns, s) => s }
        val sch = new Schema(ns, sds.toSeq, this)
        sch
      }
    }
    schemas.toSeq
  }

  /**
   * For checking uniqueness of global definitions in their namespaces
   */

  private type UC = (NS, String, Symbol, SchemaComponent)

  lazy val allTopLevels: Seq[UC] = allTopLevels_.value
  private val allTopLevels_ = LV('allTopLevels) {
    val res = schemas.flatMap { schema =>
      {
        val ns = schema.namespace
        val geds = schema.globalElementDecls.map { g =>
          {
            (ns, g.name, 'Element, g)
          }
        }
        val stds = schema.globalSimpleTypeDefs.map { g =>
          {
            (ns, g.name, 'SimpleType, g)
          }
        }
        val ctds = schema.globalComplexTypeDefs.map { g =>
          {
            (ns, g.name, 'ComplexType, g)
          }
        }
        val gds = schema.globalGroupDefs.map { g =>
          {
            (ns, g.name, 'Group, g)
          }
        }
        val dfs = schema.defineFormats.map { g =>
          {
            (ns, g.name, 'DefineFormat, g)
          }
        }
        val dess = schema.defineEscapeSchemes.map { g =>
          {
            (ns, g.name, 'DefineEscapeScheme, g)
          }
        }
        val dvs = schema.defineVariables.map { g =>
          {
            (ns, g.name, 'DefineVariable, g)
          }
        }
        val all = geds ++ stds ++ ctds ++ gds ++ dfs ++ dess ++ dvs
        all
      }
    }
    res.asInstanceOf[Seq[UC]]
  }

  lazy val groupedTopLevels = groupedTopLevels_.value
  private val groupedTopLevels_ = LV('groupedTopLevels) {
    val grouped = allTopLevels.groupBy {
      case (ns, name, kind, obj) => {
        (kind, ns, name)
      }
    }
    val grouped2 = grouped.map {
      case (idFields, seq) => {
        val onlyObj = seq.map { case (ns, name, kind, obj) => obj }
        if (onlyObj.length > 1) {
          val (ns, name, kind) = idFields
          val obj = onlyObj.head
          val locations = onlyObj.asInstanceOf[Seq[LookupLocation]] // don't like this downcast
          SDEButContinue("multiple definitions for %s  {%s}%s.\n%s", kind.toString, ns, name,
            locations.map { _.locationDescription }.mkString("\n"))
        }
        (idFields, onlyObj)
      }
    }
    val res = grouped2.flatMap { case (_, topLevelThing) => topLevelThing }.toSeq
    res
  }

  // The trick with this is when to call it. If you call it, as
  // a consequence of computing all of this, it will have to parse
  // every file, every included/imported file, etc.
  def checkForDuplicateTopLevels() = {
    groupedTopLevels // demand this.
  }

  /**
   * When the user (of the API) doesn't specify a root element namespace, just a
   * root element name, then this searches for a single element having that name, and if it is
   * unambiguous, it is used as the root.
   */
  def findRootElement(name: String) = {
    // log(Info("%s searching for root element with name %s", Misc.getNameFromClass(this), name))
    val candidates = schemas.flatMap { _.getGlobalElementDecl(name) }
    schemaDefinitionUnless(candidates.length != 0, "No root element found for %s in any available namespace", name)
    schemaDefinitionUnless(candidates.length <= 1, "Root element %s is ambiguous. Candidates are %s.",
      candidates.map { gef => gef.name + " in namespace: " + gef.schemaDocument.targetNamespace })
    Assert.invariant(candidates.length == 1)
    val gef = candidates(0)
    val re = gef.forRoot()
    re
  }

  /**
   * Given a RootSpec, get the global element it specifies. Error if ambiguous
   * or not found.
   */
  def getGlobalElement(rootSpec: RootSpec) = {
    rootSpec match {
      case RootSpec(Some(rootNamespaceName), rootElementName) => {
        val geFactory = getGlobalElementDecl(rootNamespaceName, rootElementName)
        val ge = geFactory match {
          case None => schemaDefinitionError("No global element found for %s", rootSpec)
          case Some(f) => f.forRoot()
        }
        ge
      }
      case RootSpec(None, rootElementName) => {
        findRootElement(rootElementName)
      }
      case _ => Assert.impossible()
    }
  }

  /**
   * Cache of whatever the rootElem decision was
   */
  var rootElemOpt: Option[GlobalElementDecl] = None

  var rootSpecFromPF: Option[RootSpec] = None
  /**
   * Since the root element can be specified by an API call on the
   * Compiler class, or by an API call on the ProcessorFactory, this
   * method reconciles the two. E.g., you can't specify the root both
   * places, it's one or the other.
   *
   * Also, if you don't specify a root element at all, this
   * grabs the first element declaration of the first schema file
   * to use as the root.
   */
  def rootElement(rootSpecFromProcessorFactory: Option[RootSpec]): GlobalElementDecl = {
    rootSpecFromPF = rootSpecFromProcessorFactory
    val rootSpecFromCompiler = rootSpec
    val re =
      (rootSpecFromCompiler, rootSpecFromProcessorFactory) match {
        case (Some(rs), None) =>
          getGlobalElement(rs)

        case (None, Some(rs)) =>
          getGlobalElement(rs)

        case (None, None) => {
          // if the root element and rootNamespace aren't provided at all, then
          // the first element of the first schema document is the root
          val sDocs = this.allSchemaDocuments
          val firstSchemaDocument = sDocs(0)
          val gdeclf = firstSchemaDocument.globalElementDecls
          val firstElement = {
            schemaDefinitionUnless(gdeclf.length >= 1, "No global elements in: " + firstSchemaDocument.fileName)
            val rootElement = gdeclf(0).forRoot()
            rootElement
          }
          firstElement
        }
        case _ => Assert.invariantFailed("illegal combination of root element specifications")
      }
    rootElemOpt = Some(re)
    //
    // Show root as either "{...ns...}name" 
    // or if there is no namespace just "name (in no namespace)"
    //
    val (nsSpecifier, comment) =
      if (re.targetNamespace == NoNamespace) ("", " (in no namespace)")
      else ("{" + re.targetNamespace + "}", "")
    // log(Info("Found root element %s%s%s", nsSpecifier, re.name, comment))
    re
  }

  /**
   * Retrieve schema by namespace name.
   *
   * If the schema has no namespace, then use ""
   */
  def getSchema(namespace: NS) = {
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }

  /**
   * XML Schema global objects.
   * Given a namespace and name, try to retrieve the named object
   *
   * These all return factories for the objects, not the objects themselves.
   */
  def getGlobalElementDecl(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
  def getGlobalSimpleTypeDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }

  /**
   * DFDL Schema top-level global objects
   */
  def getDefaultFormat(namespace: NS, name: String) = getSchema(namespace).flatMap { x => Some(x.getDefaultFormat) }
  def getDefineFormat(namespace: NS, name: String) = {
    val s = getSchema(namespace)
    s.flatMap { _.getDefineFormat(name) }
  }
  def getDefineFormats(namespace: NS, context: ThrowsSDE) = getSchema(namespace) match {
    case None => context.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(namespace: NS, name: String) = {
    val res = getSchema(namespace).flatMap { _.getDefineVariable(name) }
    res
  }
  def getDefineEscapeScheme(namespace: NS, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

  lazy val primitiveTypes = XMLUtils.DFDL_SIMPLE_BUILT_IN_TYPES.map { new PrimitiveType(_) }
  def getPrimitiveType(localName: String) = primitiveTypes.find { _.name == localName }

  lazy val variableMap = {
    val dvs = allSchemaDocuments.flatMap { _.defineVariables }
    val vmap = VariableMap.create(dvs)
    vmap
  }

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
            "defined " + s.map { thing =>
              thing match {
                case df: DFDLDefiningAnnotation => df.asAnnotation.fileDescription
                case sc: SchemaComponent => sc.fileDescription
                case _ => Assert.invariantFailed("should only be a SchemaComponent or a DFDLDefiningAnnotation")
              }
            }.mkString("\n and also "),
          name)
      }
    }
  }

  /**
   * Given a name, retrieve the appropriate object.
   *
   * This just scans each schema document in the schema, checking each one.
   */
  def getGlobalElementDecl(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalElementDecl(name) }, name)
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

  override lazy val enclosingComponent: Option[SchemaComponent] = None

}

/**
 * Handles everything about schema documents that has nothing to
 * do with DFDL. Things like namespace, include, import, elementFormDefault
 * etc.
 */
class XMLSchemaDocument(xmlArg: Node,
  schemaSetArg: SchemaSet,
  iiArg: Option[IIBase],
  sfArg: Option[DFDLSchemaFile],
  seenBeforeArg: IIMap)
  extends SchemaComponent(xmlArg, sfArg.getOrElse(schemaSetArg))
  with SchemaDocumentMixin
  with SchemaDocIncludesAndImportsMixin {

  requiredEvaluations(checkUnsupportedAttributes)

  lazy val seenBefore = seenBeforeArg

  val ii = iiArg

  override lazy val schemaFile = sfArg
  override lazy val xmlSchemaDocument = this

  /**
   * Error checks on the xs:schema element itself.
   *
   * E.g., we don't support the xsi:schemaLocation attribute. So we issue a warning for that.
   * and some other attributes as well.
   */

  def qualOrUnqual(str: String, kind: String) = {
    str match {
      case "unqualified" => str
      case "qualified" => str
      case _ => schemaDefinitionError("Unrecognized value for %s FormDefault='%s'.", kind, str)
    }
  }

  lazy val elementFormDefault = {
    val efdAttr = (xml \ "@elementFormDefault").text
    if (efdAttr == "") "unqualified"
    else qualOrUnqual(efdAttr, "element")
  }

  lazy val attributeFormDefault = {
    val afdAttr = (xml \ "@attributeFormDefault").text
    if (afdAttr == "") "unqualified"
    else qualOrUnqual(afdAttr, "attribute")
  }

  lazy val checkUnsupportedAttributes = checkUnsupportedAttributes_.value
  private val checkUnsupportedAttributes_ = LV('checkUnsupportedAttributes) {
    val hasSchemaLocation = (xml \ "@schemaLocation").text != ""
    val hasBlockDefault = (xml \ "@blockDefault").text != ""
    val hasFinalDefault = (xml \ "@finalDefault").text != ""
    schemaDefinitionWarningUnless(!hasSchemaLocation, "schemaLocation is ignored.")
    schemaDefinitionWarningUnless(!hasBlockDefault, "blockDefault is ignored")
    schemaDefinitionWarningUnless(!hasFinalDefault, "finalDefault is ignored")
    schemaDefinitionUnless(attributeFormDefault == "unqualified", "attributeFormDefault='qualified' is not yet implemented.")
    val res = hasSchemaLocation | hasBlockDefault | hasFinalDefault
    res
  }

}

/**
 * Handles only things specific to DFDL about schema documents.
 *
 * I.e., default format properties, named format properties, etc.
 */
class SchemaDocument(xmlSDoc: XMLSchemaDocument)
  extends AnnotatedSchemaComponent(xmlSDoc.xml, xmlSDoc)
  with SchemaDocumentMixin
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {

  /**
   * Implements the selectivity so that if you specify a root element
   * to the compiler, then only that root element (and things reached from it)
   * is compiled. Otherwise all top level elements are compiled.
   */
  requiredEvaluations(
    defaultFormat,
    if (schemaSet.checkAllTopLevel) {
      globalElementDecls.map { _.forRoot() }
      defineEscapeSchemes
      defineFormats
      defineVariables
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
    })

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

  lazy val nonDefaultPropertySources = Seq()

  lazy val defaultPropertySources = defaultPropertySources_.value
  private val defaultPropertySources_ = LV('defaultPropertySources) {
    val seq = Seq(this.defaultFormatChain)
    seq
  }

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:format>{ content @ _* }</dfdl:format> => new DFDLFormat(node, this)
      case <dfdl:defineFormat>{ content @ _* }</dfdl:defineFormat> => new DFDLDefineFormat(node, this)
      case <dfdl:defineEscapeScheme>{ content @ _* }</dfdl:defineEscapeScheme> => new DFDLDefineEscapeScheme(node, this)
      case <dfdl:defineVariable>{ content @ _* }</dfdl:defineVariable> => new DFDLDefineVariable(node, this)
      case _ => SDE("Invalid dfdl annotation found: %s", node.label)
    }
  }

  def emptyFormatFactory = new DFDLFormat(newDFDLAnnotationXML("format"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

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
    val factories = xmlelts.map { new edu.illinois.ncsa.daffodil.dsom.GlobalElementDeclFactory(_, this) }
    factories
  }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDefFactory(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDefFactory(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDefFactory(_, this) }

  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]

  lazy val defineFormats = annotationObjs.collect { case df: DFDLDefineFormat => df }
  lazy val defineEscapeSchemes = annotationObjs.collect { case des: DFDLDefineEscapeScheme => des }
  lazy val defineVariables = annotationObjs.collect { case dv: DFDLDefineVariable => dv }

  /**
   * by name getters for the global things that can be referenced.
   */
  def getGlobalElementDecl(name: String) = globalElementDecls.find { _.name == name }
  def getGlobalSimpleTypeDef(name: String) = globalSimpleTypeDefs.find { _.name == name }
  def getGlobalComplexTypeDef(name: String) = globalComplexTypeDefs.find { _.name == name }
  def getGlobalGroupDef(name: String) = globalGroupDefs.find { _.name == name }

  def getDefineFormat(name: String) = defineFormats.find { _.name == name }
  def getDefineVariable(name: String) = {
    val res = defineVariables.find { _.name == name }
    res
  }
  def getDefaultFormat = this.defaultFormat
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}

