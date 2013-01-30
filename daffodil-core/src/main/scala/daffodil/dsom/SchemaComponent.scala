package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml._
import daffodil.exceptions._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._
import daffodil.grammar._
import com.ibm.icu.charset.CharsetICU
import daffodil.dsom.OOLAG._
import daffodil.api._
import daffodil.processors.VariableMap
import daffodil.util.Compile
import daffodil.processors.charset.USASCII7BitPackedCharset
import daffodil.processors.charset.CharsetUtils
import daffodil.compiler.RootSpec
import daffodil.util.Misc
import daffodil.compiler.Compiler
import java.io.File
import java.net.URI
import java.net.URL

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(val xml: Node)
  extends GetAttributesMixin
  with SchemaLocation
  with ImplementsThrowsSDE
  with SchemaFileLocatable
  with IIUtils
  with ResolvesQNames
  with FindPropertyMixin
  with LookupLocation
  with PropTypes {

  /*
   * Anything non-annotated always returns property not found
   * 
   * Override in annotated components
   */
  def findPropertyOption(pname: String): PropertyLookupResult = NotFound(Nil, Nil)
  // FIXME: not sure why non-annotated schema components need to have findProperty
  // on them at all. Who would call it polymorphically, not knowing whether they 
  // have an annotated schema component or not?

  lazy val properties: PropMap = Map.empty

  lazy val schemaSet: SchemaSet = schemaDocument.schemaSet
  def schemaDocument: SchemaDocument
  def schemaComponent = this

  lazy val schema: Schema = schema_.value
  private lazy val schema_ = LV('schema) { schemaDocument.schema }

  lazy val targetNamespace: NS = schemaDocument.targetNamespace
  lazy val targetNamespacePrefix = xml.scope.getPrefix(targetNamespace.toString)
  def prettyName: String

  lazy val fileName: URL = fileName_.value
  private lazy val fileName_ = LV('fileName) {
    schemaDocument.fileName
  }

  lazy val isHidden: Boolean = {
    enclosingComponent match {
      case None => Assert.invariantFailed("Root global element should be overriding this.")
      case Some(ec) => ec.isHidden
    }
  }

  def enclosingComponent: Option[SchemaComponent]

  def context = this

  final lazy val path = path_.value
  private lazy val path_ = LV('path) {
    scPath.map { _.prettyName }.mkString("::")
  }

  override def toString = prettyName

  /**
   * Includes instances. Ie., a global element will appear inside an element ref.
   * a global group inside a group ref, a global type inside an element or for
   * derived simple types inside another simple type, etc.
   */
  lazy val scPath: Seq[SchemaComponent] = scPath_.value
  private lazy val scPath_ = LV('scPath) {
    val res = enclosingComponent.map { _.scPath }.getOrElse(Nil) :+ this
    res
  }

  private val scala.xml.Elem(_, _, emptyXMLMetadata, _, _*) = <foo/> // hack way to get empty metadata object.

  /**
   * Used as factory for the XML Node with the right namespace and prefix etc.
   *
   * Given "element" it creates <dfdl:element /> with the namespace definitions
   * based on this schema component's corresponding XSD construct.
   *
   * Makes sure to inherit the scope so we have all the namespace bindings.
   */
  def newDFDLAnnotationXML(label: String) = {
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, xml.scope)
  }

  val NYI = false // our flag for Not Yet Implemented 

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

  lazy val schemaDocument = schemaDocument_.value
  private lazy val schemaDocument_ = LV('schemaDocument) { parent.schemaDocument }
}

/**
 * Common trait for both named SchemaComponents and named (aka Defining) Annotation objects
 *
 * The difference between this and the Global flavor
 * has to do with the elementFormDefault attribute of the xs:schema
 * element. Local things are either qualified or unqualified
 * in their names depending on the elementFormDefault attribute
 * being "qualified" or "unqualified" (unqualified is the default)
 */
trait NamedAnnotationAndComponentMixin
  extends GetAttributesMixin {

  lazy val name = getAttributeRequired("name")
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
  extends NamedAnnotationAndComponentMixin { self: SchemaComponent =>

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
  extends NamedAnnotationAndComponentMixin {
  /**
   * handle elementFormDefault to qualify
   */
  override lazy val namespace =
    if (schemaDocument.elementFormDefault == "unqualified")
      NoNamespace // unqualified means no namespace
    else schemaDocument.targetNamespace

  override lazy val prefix =
    if (schemaDocument.elementFormDefault == "unqualified")
      "" // unqualified means no prefix
    else {
      //
      // name is supposed to be qualified by the target namespace
      //
      val tns = namespace
      // record this error on the schemaDocument
      schemaDocument.schemaDefinition(tns != "", "Must have a targetNamespace if elementFormDefault='qualified'.")
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

abstract class AnnotatedSchemaComponent(xml: Node)
  extends SchemaComponent(xml)
  with AnnotatedMixin {

  /**
   * only used for debugging
   */
  override lazy val properties: PropMap =
    (nonDefaultPropertySources.flatMap { _.properties.toSeq } ++
      defaultPropertySources.flatMap { _.properties.toSeq }).toMap

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
 */
trait AnnotatedMixin
  extends CommonRuntimeValuedPropertiesMixin
  with EncodingMixin { self: AnnotatedSchemaComponent =>

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
  protected lazy val annotationObjs_ = LV('annotationObjs) {
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
  private lazy val formatAnnotation_ = LV('formatAnnotation) {
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
 * Split this out of AnnotatedMixin for separation of
 * concerns reasons.
 *
 * TODO: move to GrammarMixins.scala, or another file
 * of these sorts of traits that are mixed onto the
 * schema components.
 */
trait EncodingMixin { self: AnnotatedSchemaComponent =>
  /**
   * Character encoding common attributes
   *
   * Note that since encoding can be computed at runtime, we
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */

  lazy val isKnownEncoding = {
    val res = encoding.isConstant
    if (res) {
      val encName = encoding.constantAsString.toUpperCase()
      if (encName.startsWith("UTF-16")) {
        schemaDefinition(utf16Width == UTF16Width.Fixed, "Property utf16Width='variable' not supported.")
        //
        // TODO: when runtime encoding is supproted, must also check for utf16Width
        // (and error if unsupported then, or just implement it!)
      }
    }
    res
  }

  lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = encoding.constantAsString.toUpperCase()
    res
  }

  lazy val knownEncodingCharset = {
    CharsetUtils.getCharset(knownEncodingName)
  }

  // Really bad idea. Don't save these. Decoders and Encoders are stateful
  // so they can't be precomputed here and reused without all sorts of 
  // thread issues and reset protocols.
  //  lazy val knownEncodingDecoder = {
  //    val decoder = knownEncodingCharset.newDecoder()
  //    decoder
  //  }
  //
  //  lazy val knownEncodingEncoder = {
  //    val encoder = knownEncodingCharset.newEncoder()
  //    encoder
  //  }

  /**
   * enables optimizations and random-access
   *
   * variable-width character sets require scanning to determine
   * their end.
   */
  lazy val knownEncodingIsFixedWidth = {
    // val res = knownEncodingCharset.isFixedWidth
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => true
      case "US-ASCII-7-BIT-PACKED" => true
      case "UTF-8" => false
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
        if (utf16Width == UTF16Width.Fixed) true
        else false
      }
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => true
      case "ISO-8859-1" => true
      case _ => schemaDefinitionError("Charset %s is not supported.", knownEncodingName)
    }
    res
  }

  lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth

  lazy val knownEncodingWidthInBits = {
    // knownEncodingCharset.width()
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => 8
      case "US-ASCII-7-BIT-PACKED" => 7 // NOTE! 7-bit characters dense packed. 8th bit is NOT unused. 
      case "UTF-8" => -1
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
        if (utf16Width == UTF16Width.Fixed) 16
        else -1
      }
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => 32
      case "ISO-8859-1" => 8
      case _ => schemaDefinitionError("Charset %s is not supported.", knownEncodingName)
    }
    res
  }

  lazy val knownEncodingStringBitLength = {
    //
    // This will be called at runtime, so let's decide
    // what we can, and return an optimized function that 
    // has characteristics of the encoding wired down.
    //
    if (knownEncodingIsFixedWidth) {
      def stringBitLength(str: String) = str.length * knownEncodingWidthInBits
      stringBitLength _
    } else {
      def stringBitLength(str: String) = {
        // variable width encoding, so we have to convert each character 
        // We assume here that it will be a multiple of bytes
        // that is, that variable-width encodings are all some number
        // of bytes.
        str.getBytes(knownEncodingName).length * 8
      }
      stringBitLength _
    }
  }
}

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can have
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait DFDLStatementMixin extends ThrowsSDE { self: AnnotatedMixin =>

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
    schemaDefinition(discrims.size <= 1, "At most one discriminator allowed at same location: %s", discrims)
    schemaDefinition(asserts == Nil || discrims == Nil,
      "Cannot have both dfdl:discriminator annotations and dfdl:assert annotations at the same location.")
    (discrims, asserts)
  }

  final def checkDistinctVariableNames(svs: Seq[DFDLSetVariable]) = {
    val names = svs.map { _.defv.extName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinition(areAllDistinct, "Variable names must all be distinct at the same location: %s", names)
    svs
  }

  final lazy val localSetVariableStatements = {
    val svs = localStatements.collect { case sv: DFDLSetVariable => sv }
    checkDistinctVariableNames(svs)
  }

}

/**
 * SAX style parsing calls your error handler when any error occurs.
 *
 * This just uses the passed in schema component to accumulate
 * the errors.
 */
class SchemaSetErrorHandler(err: SchemaSet) extends org.xml.sax.ErrorHandler {

  def warning(exception: SAXParseException) = {
    val sdw = new SchemaDefinitionWarning(err, "Warning loading schema", exception)
    err.addDiagnostic(sdw)
  }

  def error(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(err, "Error loading schema", exception)
    System.err.println(sde.getMessage())
    err.addDiagnostic(sde)
  }

  def fatalError(exception: SAXParseException) = {
    val sde = new SchemaDefinitionError(err, "Fatal error loading schema", exception)
    err.addDiagnostic(sde)
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
  schemaFileNames: Seq[String],
  rootSpec: Option[RootSpec] = None,
  val checkAllTopLevel: Boolean = false)
  extends SchemaComponent(<schemaSet/>) { // fake schema component

  // These things are needed to satisfy the contract of being a schema component.
  lazy val enclosingComponent = None
  lazy val schemaDocument = Assert.usageError("schemaDocument should not be called on SchemaSet")
  override lazy val fileName = new URL("file:SchemaSet")
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

  //
  // construct our XML loader, giving it an error handler that will
  // turn SAX load-time validation error events into gathered diagnostics on
  // our diagnostics lists.
  //
  lazy val loader = new DaffodilXMLLoader(new SchemaSetErrorHandler(this))

  /**
   * the initial list of schema document nodes provided by
   * the user (at the command line or via API)
   */
  lazy val initialSchemaDocuments = initialSchemaDocuments_.value
  private lazy val initialSchemaDocuments_ = LV('initialSchemaDocuments) {
    val nl = schemaFileNames.map { fn =>
      {
        val node = loader.loadFile(fn)
        node match {
          case <schema>{ _* }</schema> if (
            NS(node.namespace) == XMLUtils.xsdURI ||
            NS(node.namespace) == XMLUtils.DFDLSubsetURI) => {
            // top level is a schema. 
            new SchemaDocument(node, this, None)
          }
          case _ => schemaDefinitionError("The file %s did not contain a schema element as the document element. Found %s in namespace %s.", fn, node.label, node.namespace)
        }
      }
    }
    nl
  }

  lazy val alreadySeenStart: IIMap = alreadySeenStart_.value
  private lazy val alreadySeenStart_ = LV[IIMap]('alreadySeenStart) {
    val seq = initialSchemaDocuments.map { sd =>
      {
        val oneMap = ((sd.targetNamespace, sd.fileName) -> sd)
        oneMap
      }
    }
    seq.toMap.asInstanceOf[IIMap]
  }

  /**
   * Good example of functional programming here.
   *
   * In Java, or in imperative programming style, one would
   * write this by allocating a mutable lookup table/map, and
   * then you would traverse the structure, mutating this state
   * object, checking against it for each new schema file
   * before loading that schema file.
   *
   * Instead this is done in the classic functional programming
   * style using foldLeft and recursion.
   *
   * What foldLeft does is takes a starting value (a immutable map
   * from a pair of (NS, URL) -> SchemaDocument). The second argument
   * is the folding function which takes 2 arguments.
   * For foldLeft it is the left argument which is 'circulated',
   * that is, whatever the folding function produces for one call, is fed
   * back around as the left argument for the next call on the next
   * element.
   *
   * In the below, the thing being fed around is this ever-growing
   * immutable map of the unique schema file instances.
   *
   * Note that a schema file that has no target namespace can be included
   * by other schemas, and it will take on the namespace of the
   * schema into-which it was included. That's why the key to the
   * table is both the namespace, and the filename. Because the same
   * schema file name might have to be read a bunch of times to create
   * instances in the various namespaces. This is called 'chameleon' namespaces
   * in XML Schema parlance.
   *
   * TODO: since the namespace could be stored outside of the schema
   * document, we *should* be able to be clever and share the SchemaDocument
   * structure. For now we're not bothering with this, but it might help
   * for very large schemas if they use include and this chameleon
   * namespace stuff a lot.
   *
   * This right here is the hairy transitive closure algorithm that
   * walks down all schema docs, and all include and imports, and
   * follows them to more schema docs, all the while accumulating a
   * map of what it has already seen so as not to load the same
   * schema twice for the same namespace.
   */
  lazy val allSeenDocs = allSeenDocs_.value
  private lazy val allSeenDocs_ = LV('allSeenDocs) {

    val transitiveClosure = {
      // we fold up the initial schema documents from the user
      // with the initial 'left' value (type IIMap for 'include and import map'
      // ie, a map containing those same
      // schema documents keyed from their namespaces & filenames. 
      initialSchemaDocuments.foldLeft(alreadySeenStart)(unseenSchemaDocs _)
    }
    transitiveClosure
  }

  /**
   * the outer loop fold function. folds up the list of schema documents,
   * accumulating the unseen (so far) schema documents into the result.
   */
  def unseenSchemaDocs(seen: IIMap, sd: SchemaDocument): IIMap = {
    val iis = sd.includesAndImports
    val resultSeen = iis.foldLeft(seen)(foldOneII _)
    resultSeen
  }

  /**
   * The inner loop fold function. Folds up a list of include or import statements.
   * coming from one schema document.
   *
   * This is where we check to see if we can skip the include/import
   * beacuse we already have the file.
   *
   * And the magic here is that for each new schema document that
   * we actually do load, recursively we fold up all unseen schema docs
   * reachable from it.
   *
   * And this idiom threads that seen-map through everything sequentially
   * so that there are never duplicates to remove.
   *
   * Also, this handles circular import namespace relationships. Those
   * are common as a large schema may simply have files that use each other's
   * definitions. It might not be a good well-layered design, but XML
   * schema (and DFDL schema) certainly allows it.
   */
  def foldOneII(seen: IIMap, ii: IIBase) = {
    seen.get((ii.targetNamespace, ii.resolvedLocation)) match {
      case None => {
        val newSd = ii.iiSchemaDocument
        val mapTuple = ((newSd.targetNamespace, newSd.fileName), newSd)
        val resultSeen1 = seen + mapTuple // add this document as seen
        // recursively add all unseen schema docs reachable from it
        // to the set of things we've seen
        val resultSeen2 = unseenSchemaDocs(resultSeen1, newSd)
        resultSeen2
      }
      case Some(_) => seen
    }
  }

  lazy val prettyName = "SchemaSet"

  lazy val allSchemaDocuments = allSeenDocs.map { case (_, sd) => sd }

  lazy val schemas = schemas_.value
  private lazy val schemas_ = LV('schemas) {
    val schemaPairs = allSchemaDocuments.map { sd => (sd.targetNamespace, sd) }
    val schemaGroups = schemaPairs.groupBy { _._1 } // group by the namespace identifier
    val schemas = schemaGroups.map {
      case (ns, pairs) => {
        val sds = pairs.map { case (ns, s) => s }
        val s = new Schema(ns, sds.toSeq, this)
        s
      }
    }
    schemas.toSeq
  }

  /**
   * For checking uniqueness of global definitions in their namespaces
   */
  lazy val allTopLevels: Seq[(NS, String, Symbol, NamedAnnotationAndComponentMixin)] = allTopLevels_.value
  private lazy val allTopLevels_ = LV('allTopLevels) {
    schemas.flatMap { schema =>
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
  }

  lazy val groupedTopLevels = groupedTopLevels_.value
  private lazy val groupedTopLevels_ = LV('groupedTopLevels) {
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
    grouped2
  }

  // The trick with this is when to call it. If you call it, as
  // a consequence of computing all of this, it will have to parse
  // every file, every included/imported file, etc.
  def checkForDuplicateTopLevels() {
    groupedTopLevels // demand this.
  }

  /**
   * When the user (of the API) doesn't specify a root element namespace, just a
   * root element name, then this searches for a single element having that name, and if it is
   * unambiguous, it is used as the root.
   */
  def findRootElement(name: String) = {
    val candidates = schemas.flatMap { _.getGlobalElementDecl(name) }
    val res = if (candidates.length == 0) {
      schemaDefinitionError("No root element found for %s in any available namespace", name)
    } else if (candidates.length > 1) {
      schemaDefinitionError("Root element %s is ambiguous. Candidates are %s.",
        candidates.map { gef => gef.name + " in namespace: " + gef.schemaDocument.targetNamespace })
    } else {
      val gef = candidates(0)
      val re = gef.forRoot()
      re
    }
    res
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
    val rootSpecFromCompiler = rootSpec
    (rootSpecFromCompiler, rootSpecFromProcessorFactory) match {
      case (Some(rs), None) =>
        getGlobalElement(rs)

      case (None, Some(rs)) =>
        getGlobalElement(rs)

      case (None, None) => {
        // if the root element and rootNamespace aren't provided at all, then
        // the first element of the first schema document is the root
        val firstSchema = schemas(0)
        val firstSchemaDocument = firstSchema.schemaDocuments(0)
        val firstElement: GlobalElementDecl = {
          firstSchemaDocument.globalElementDecls match {
            case firstElement :: _ => firstElement.forRoot()
            case _ => toss(new SchemaDefinitionError(None, None, "No global elements in: " + firstSchemaDocument.fileName))
          }
        }
        firstElement
      }
      case _ => Assert.invariantFailed("illegal combination of root element specifications")
    }
  }

  lazy val diagnosticChildren: DiagnosticsList = {
    if (checkAllTopLevel) {
      // checkForDuplicateTopLevels() // debug this later.
      schemas
    } else Nil
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
    val dvs = schemas.flatMap { _.schemaDocuments }.flatMap { _.defineVariables }
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
class Schema(val namespace: NS, schemaDocs: Seq[SchemaDocument], val schemaSet: SchemaSet)
  extends DiagnosticsProviding {

  lazy val prettyName = "schema"
  lazy val path = prettyName

  lazy val targetNamespace: NS = namespace

  lazy val schemaDocuments = schemaDocs

  lazy val diagnosticChildren: DiagnosticsList = schemaDocuments

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc) // exactly one is good
      case s => {
        schemaSet.schemaDefinitionError(
          "More than one definition for name: %s\n" +
            "defined " + s.map { thing =>
              thing match {
                case sc: SchemaComponent => sc.fileDescription
                case df: DFDLDefiningAnnotation => df.asAnnotation.fileDescription
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
class SchemaDocument(xmlArg: Node,
                     schemaSetArg: SchemaSet,
                     include: Option[Include])
  extends AnnotatedSchemaComponent(xmlArg)
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {

  lazy val nonDefaultPropertySources = Seq()

  lazy val defaultPropertySources = {
    val seq = Seq(this.defaultFormatChain)
    seq
  }

  override lazy val schemaSet = schemaSetArg
  /**
   * For include, if the included schema doesn't have a
   * targetNamespace, then we will take on the namespace
   * of whatever we are included into.
   *
   * This is the chameleon namespace concept, and it works
   * inductively. I.e., the included schema could include more
   * schemas, all of them ultimately getting the targetNamespace
   * from the schema enclosing that outermost include.
   *
   * If an included schema DOES have a targetNamespace, it must match what we're
   * included into.
   */
  override lazy val targetNamespace = targetNamespace_.value
  private lazy val targetNamespace_ = LV('targetNamespace) {
    val tnsAttrib = this.getAttributeOption("targetNamespace").map { NS(_) }
    val withCheckedNS = tnsAttrib.map { tns =>
      {
        include.foreach { inc =>
          {
            schemaDefinition(inc.targetNamespace == tns,
              "Included schema does not have the same namespace as the file %s including it.",
              fileName)
          }
        }
        tns
      }
    }
    val resultNS = withCheckedNS.getOrElse {
      include.map { _.targetNamespace }.getOrElse(NoNamespace)
    }
    resultNS
  }

  override lazy val fileName = fileName_.value
  private lazy val fileName_ = LV('fileName) {
    this.fileNameFromAttribute().getOrElse(new URL("urn:unknown"))
  }

  lazy val enclosingComponent: Option[SchemaComponent] = None

  lazy val prettyName = "schemaDoc"

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
  private lazy val checkUnsupportedAttributes_ = LV('checkUnsupportedAttributes) {
    val hasSchemaLocation = (xml \ "@schemaLocation").text != ""
    val hasBlockDefault = (xml \ "@blockDefault").text != ""
    val hasFinalDefault = (xml \ "@finalDefault").text != ""
    schemaDefinitionWarning(!hasSchemaLocation, "schemaLocation is ignored.")
    schemaDefinitionWarning(!hasBlockDefault, "blockDefault is ignored")
    schemaDefinitionWarning(!hasFinalDefault, "finalDefault is ignored")
    schemaDefinition(attributeFormDefault == "unqualified", "attributeFormDefault='qualified' is not yet implemented.")
    val res = hasSchemaLocation | hasBlockDefault | hasFinalDefault
    res
  }

  override lazy val schema = schemaSet.getSchema(targetNamespace).getOrElse {
    Assert.invariantFailed("schema not found for schema document's namespace.")
  }

  lazy val schemaDocument = this

  def annotationFactory(node: Node): DFDLAnnotation = {
    node match {
      case <dfdl:format>{ content @ _* }</dfdl:format> => new DFDLFormat(node, this)
      case <dfdl:defineFormat>{ content @ _* }</dfdl:defineFormat> => new DFDLDefineFormat(node, this)
      case <dfdl:defineEscapeScheme>{ content @ _* }</dfdl:defineEscapeScheme> => new DFDLDefineEscapeScheme(node, this)
      case <dfdl:defineVariable>{ content @ _* }</dfdl:defineVariable> => new DFDLDefineVariable(node, this)
      case _ => Assert.impossible("Invalid dfdl annotation found!")
    }
  }

  def emptyFormatFactory = new DFDLFormat(newDFDLAnnotationXML("format"), this)
  def isMyFormatAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

  private lazy val sset = schema.schemaSet

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
  lazy val globalElementDecls = (xml \ "element").map { new GlobalElementDeclFactory(_, this) }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDefFactory(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDefFactory(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDefFactory(_, this) }

  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]

  lazy val defineFormats = annotationObjs.collect { case df: DFDLDefineFormat => df }
  lazy val defineEscapeSchemes = annotationObjs.collect { case des: DFDLDefineEscapeScheme => des }
  lazy val defineVariables = annotationObjs.collect { case dv: DFDLDefineVariable => dv }

  lazy val alwaysCheckedChildren = {
    checkUnsupportedAttributes // getting this value may cause diagnostics to be recorded on the SchemaDocument
    List(defaultFormat)
  }

  lazy val impNodes = (xml \ "import")
  lazy val incNodes = (xml \ "include")
  lazy val imports = impNodes.map { iNode => new Import(iNode, this) }
  lazy val includes = incNodes.map { iNode => new Include(iNode, this) }
  lazy val includesAndImports = includes ++ imports

  /**
   * Implements the selectivity so that if you specify a root element
   * to the compiler, then only that root element (and things reached from it)
   * is compiled. Otherwise all top level elements are compiled.
   */
  lazy val diagnosticChildren: DiagnosticsList = {
    if (schema.schemaSet.checkAllTopLevel) allGlobalDiagnosticChildren
    else alwaysCheckedChildren
  }

  lazy val allGlobalDiagnosticChildren = {
    alwaysCheckedChildren ++
      globalElementDecls.map { _.forRoot() } ++
      defineEscapeSchemes ++
      defineFormats ++
      defineVariables // TODO: only include these if they have default values or external values. 
    // Those then have to be evaluated before any processing, 
    // and may depend on other variables with default values or external values.
    // Not these, because we'll pick these up when elements reference them.
    // And we don't compile them independently of that (since they could be very
    // incomplete and would lead to many errors for missing this or that.)
    //    globalSimpleTypeDefs ++
    //    globalComplexTypeDefs ++
    //    globalGroupDefs ++
  }

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

