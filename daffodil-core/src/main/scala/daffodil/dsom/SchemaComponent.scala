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

class SchemaDefinitionError(schemaContext: Option[SchemaComponent],
                            annotationContext: Option[DFDLAnnotation],
                            kind: String,
                            args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {
  val isError = true
  val diagnosticKind = "Error"
}

class SchemaDefinitionWarning(schemaContext: Option[SchemaComponent],
                              annotationContext: Option[DFDLAnnotation],
                              kind: String,
                              args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {
  val isError = false
  val diagnosticKind = "Warning"
}

abstract class SchemaDefinitionDiagnosticBase(
  val schemaContext: Option[SchemaComponent],
  val annotationContext: Option[DFDLAnnotation],
  val kind: String,
  val args: Any*) extends Exception with DiagnosticImplMixin {
  def isError: Boolean
  def diagnosticKind: String
  def getSchemaLocations = schemaContext.toList
  def getDataLocations = Nil
  // TODO: Alternate constructor that allows data locations.
  // Because some SDEs are caught only once Processing starts. 
  // They're still SDE but they will have data location information.

  override def toString = {
    lazy val argsAsString = args.map { _.toString }.mkString(", ")
    //
    // Right here is where we would lookup the symbolic error kind id, and 
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg =
      if (kind.contains("%")) kind.format(args: _*)
      else (kind + "(%s)").format(argsAsString)
    val res = "Schema Definition " + diagnosticKind + ": " + msg + " Context was: " + schemaContext.getOrElse("top level")
    res
  }

  override def getMessage = toString
}

/**
 * The core root class of the DFDL Schema object model.
 *
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(val xml: Node)
  extends DiagnosticsProviding
  with GetAttributesMixin
  with SchemaLocation
  with ThrowsSDE {
  def schemaDocument: SchemaDocument
  lazy val schema: Schema = schemaDocument.schema
  lazy val targetNamespace = schema.targetNamespace
  lazy val targetNamespacePrefix = xml.scope.getPrefix(targetNamespace)
  def prettyName: String

  lazy val isHidden: Boolean = {
    enclosingComponent match {
      case None => Assert.invariantFailed("Root global element should be overriding this.")
      case Some(ec) => ec.isHidden
    }
  }

  def enclosingComponent: Option[SchemaComponent]

  def context = this

  final lazy val path = scPath.map { _.prettyName }.mkString("::")

  override def toString = path

  /**
   * Includes instances. Ie., a global element will appear inside an element ref.
   * a global group inside a group ref, a global type inside an element or for
   * derived simple types inside another simple type, etc.
   */
  lazy val scPath: Seq[SchemaComponent] = {
    val res = enclosingComponent.map { _.scPath }.getOrElse(Nil) :+ this
    res
  }

  private val scala.xml.Elem(_, _, emptyXMLMetadata, _, _*) = <foo/> // hack way to get empty metadata object.

  /**
   * Used as factory for the XML Node with the right namespace and prefix etc.
   *
   * Given "element" it creates <dfdl:element /> with the namespace definitions
   * based on this schema component's corresponding XSD construct.
   */
  def newDFDLAnnotationXML(label: String) = {
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, xml.scope)
  }

  val NYI = false // our flag for Not Yet Implemented 

  // TODO: create a trait to share various error stuff with DFDLAnnotation class.
  // Right now there is small code duplication since annotations aren't schema components.
  def SDE(id: String, args: Any*): Nothing = {
    val sde = new SchemaDefinitionError(Some(this), None, id, args: _*)
    throw sde
  }

  def SDW(id: String, args: Any*): Unit = {
    val sdw = new SchemaDefinitionWarning(Some(this), None, id, args: _*)
    addDiagnostic(sdw)
  }

  def subset(testThatWillThrowIfFalse: Boolean, msg: String, args: Any*) = {
    if (!testThatWillThrowIfFalse) subsetError(msg, args: _*)
  }

  def subsetError(msg: String, args: Any*) = {
    val msgTxt = msg.format(args: _*)
    SDE("Subset " + msgTxt)
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

  lazy val schemaDocument = parent.schemaDocument
}

/**
 * Anything named has a name from its corresponding xml's name attribute
 */
trait NamedMixin { self: { def xml: Node } => // this scala idiom means the object this is mixed into has a def for xml of type Node
  lazy val name = (xml \ "@name").text
  lazy val prettyName = name
}

/**
 * All global components share these characteristics.
 */
trait GlobalComponentMixin
  extends NamedMixin { self: SchemaComponent =>
}

abstract class AnnotatedSchemaComponent(xml: Node)
  extends SchemaComponent(xml)
  with AnnotatedMixin

/**
 * Every component that can be annotated.
 */
// Provides some polymorphism across annotated things, 
// and unannotated things like complex types.
trait SharedPropertyLists {
  // use def, can be overriden by lazy val, val, or def
  def localAndFormatRefProperties: Map[String, String]
  def allNonDefaultProperties = localAndFormatRefProperties
}

trait AnnotatedMixin
  extends CommonRuntimeValuedPropertiesMixin
  with SharedPropertyLists { self: SchemaComponent =>

  def prettyName: String
  def path: String

  def defaultProperties: Map[String, String] = {
    this.schemaDocument.localAndFormatRefProperties
  }

  lazy val sDoc = self.schemaDocument
  /**
   * Primary mechanism for a component to get a format property value.
   *
   * Note: use only for format properties, not any old attribute.
   */
  def getPropertyOption(pname: String) = {
    val local = allNonDefaultProperties.get(pname)
    local match {
      case None => {
        defaultProperties.get(pname)
      }
      case Some(_) => {
        local
      }
    }
  }

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
  protected lazy val annotationObjs_ = LV {
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
  private lazy val formatAnnotation_ = LV {
    val format = annotationObjs.find { isMyFormatAnnotation(_) }
    val res = format match {
      case None => emptyFormatFactory
      case Some(x) => x
    }
    res.asInstanceOf[DFDLFormatAnnotation]
  }

  /**
   * Annotations can contain expressions, so we need to be able to compile them.
   *
   * We need our own instance so that the expression compiler has this schema
   * component as its context.
   */
  lazy val expressionCompiler = new ExpressionCompiler(this)

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
   * element decls and their simple types.
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
 * A schema set is exactly that, a set of schemas. Each schema has
 * a target namespace, so a schema set is conceptually a mapping from
 * namespace URI onto schema.
 *
 * Constructing these from a list of schema Nodes is a unit-test
 * interface.
 *
 * schemaNodeList is a list of scala xml Node, each expected to be an <xs:schema...>
 * node. These may have include/import statements in them, and the schemas
 * being included/imported don't have to be within the list.
 *
 */
class SchemaSet(val schemaNodeList: Seq[Node], rootNamespace: String = null, root: String = null)
  extends DiagnosticsProviding {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.

  lazy val prettyName = "SchemaSet"
  lazy val path = prettyName

  lazy val schemaPairs = schemaNodeList.map { s =>
    {
      val ns = (s \ "@targetNamespace").text
      (ns, s)
    }
  }

  lazy val onlyCheckingRoot: Boolean = rootNamespace != null

  lazy val schemaGroups = schemaPairs.groupBy {
    case (ns, s) => ns
  }.toList

  lazy val schemas = schemaGroups.map {
    case (ns, pairs) => {
      val sds = pairs.map(_._2) // Grabs second of 'pairs', list of schema Document
      val res = new Schema(ns, sds, this)
      res
    }
  }

  /**
   * We control how much checking for errors by supplying root element or not.
   * If supplied, then only that is checked for errors.
   * If not supplied, then everything in the schema set is checked.
   */
  lazy val rootElement = {
    if (onlyCheckingRoot) {
      val geFactory = getGlobalElementDecl(rootNamespace, root)
      val ge = geFactory match {
        case None => throw new SchemaDefinitionError(None, None, "No global element found for : " + (rootNamespace, root))
        case Some(f) => f.forRoot()
      }
      Some(ge)
    } else None
  }

  /**
   * In the case there is no root element, then we'll check all element decls,
   * actually all global elements and other top-level constructs of each
   * schema document.
   *
   * If a root element is specified, then we'll validate the schema documents,
   * but we will only check the root element.
   */
  lazy val diagnosticChildren = {
    schemas ++ rootElement.toList
  }

  /**
   * Retrieve schema by namespace name.
   */
  def getSchema(namespace: String) = {
    // TODO: what about when there is no namespace. Can we pass "" ??
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }

  /**
   * XML Schema global objects.
   * Given a namespace and name, try to retrieve the named object
   *
   * These all return factories for the objects, not the objects themselves.
   */
  def getGlobalElementDecl(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
  def getGlobalSimpleTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }

  /**
   * DFDL Schema top-level global objects
   */
  def getDefaultFormat(namespace: String, name: String) = getSchema(namespace).flatMap { x => Some(x.getDefaultFormat) }
  def getDefineFormat(namespace: String, name: String) = {
    val s = getSchema(namespace)
    getSchema(namespace).flatMap { _.getDefineFormat(name) }
  }
  def getDefineFormats(namespace: String, context: ThrowsSDE) = getSchema(namespace) match {
    case None => context.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(namespace: String, name: String) = {
    val res = getSchema(namespace).flatMap { _.getDefineVariable(name) }
    res
  }
  def getDefineEscapeScheme(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

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
class Schema(val namespace: String, val schemaDocs: NodeSeq, val schemaSet: SchemaSet)
  extends DiagnosticsProviding {

  lazy val prettyName = "schema"
  lazy val path = prettyName

  lazy val targetNamespace = namespace

  lazy val schemaDocuments = schemaDocs.map { new SchemaDocument(_, this) }

  lazy val diagnosticChildren = schemaDocuments

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc)
      case _ => throw new SchemaDefinitionError(None, None, "more than one definition for name: " + name)
    }
  }

  /**
   * Given a name, try to retrieve the appropriate object.
   *
   * This just scans each schema document in the schema, checking each one.
   */
  def getGlobalElementDecl(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalElementDecl(name) }, name)
  def getGlobalSimpleTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalSimpleTypeDef(name) }, name)
  def getGlobalComplexTypeDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalComplexTypeDef(name) }, name)
  def getGlobalGroupDef(name: String) = noneOrOne(schemaDocuments.flatMap { _.getGlobalGroupDef(name) }, name)
  def getDefineFormat(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineFormat(name) }, name)
  def getDefineFormats() = schemaDocuments.flatMap { _.defineFormats }
  def getDefineVariable(name: String) = {
    val res = noneOrOne(schemaDocuments.flatMap { _.getDefineVariable(name) }, name)
    res
  }
  def getDefaultFormat = schemaDocuments.flatMap { x => Some(x.getDefaultFormat) }
  def getDefineEscapeScheme(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineEscapeScheme(name) }, name)
  def getGlobalElementDecls = schemaDocuments.flatMap(_.globalElementDecls)
  def getGlobalGroupDefs = schemaDocuments.flatMap(_.globalGroupDefs)
  def getGlobalSimpleTypeDefs = schemaDocuments.flatMap(_.globalSimpleTypeDefs)
  def getGlobalComplexTypeDefs = schemaDocuments.flatMap(_.globalComplexTypeDefs)
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
class SchemaDocument(xmlArg: Node, schemaArg: Schema)
  extends AnnotatedSchemaComponent(xmlArg)
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {

  lazy val enclosingComponent: Option[SchemaComponent] = None

  lazy val prettyName = "schemaDoc"

  lazy val validatedXML = LV {
    try XMLSchemaUtils.validateDFDLSchema(xml)
    catch {
      case e: org.xml.sax.SAXParseException => {
        SDE(e.toString())
      }

    }
    // TODO: Consider this: Should each schema document call validate, or do we have to do them as a "batch", i.e.,
    // will the above validate and revalidate shared files imported by each schema document in the set???
  }

  lazy val localAndFormatRefProperties = this.formatAnnotation.getFormatPropertiesNonDefault()

  /**
   * A schema document doesn't have default properties of its own.
   * The schema document's localAndFormatRefProperties become the default properties for
   * the components contained within this schema document.
   */
  override lazy val defaultProperties = Map.empty[String, String]

  override lazy val schema = schemaArg

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

  //
  // There's some scala way of avoiding all this downcasting, using some clever type parameterization scheme
  // Have to ask a Scala types expert.
  //
  lazy val defineFormats = {
    val df = annotationObjs.filter { _.isInstanceOf[DFDLDefineFormat] }.map { _.asInstanceOf[DFDLDefineFormat] }
    df
  }

  lazy val defineEscapeSchemes = {
    val desc = annotationObjs.filter { _.isInstanceOf[DFDLDefineEscapeScheme] }.map { _.asInstanceOf[DFDLDefineEscapeScheme] }
    desc
  }

  lazy val defineVariables = {
    val dv = annotationObjs.filter { _.isInstanceOf[DFDLDefineVariable] }.map { _.asInstanceOf[DFDLDefineVariable] }
    dv
  }

  lazy val alwaysCheckedChildren =
    List(validatedXML, defaultFormat)

  lazy val diagnosticChildren = {
    if (schema.schemaSet.onlyCheckingRoot) alwaysCheckedChildren // we'll still validate the schema, just not recurse into children.
    else allGlobalDiagnosticChildren
  }

  lazy val allGlobalDiagnosticChildren = {
    alwaysCheckedChildren ++
      globalElementDecls.map { _.forRoot() } ++
      defineEscapeSchemes ++
      defineFormats ++
      defineVariables
  }
  // Not these, because we'll pick these up when elements reference them.
  // And we don't compile them independently of that (since they could be very
  // incomplete and would lead to many errors for missing this or that.)
  //    globalSimpleTypeDefs ++
  //    globalComplexTypeDefs ++
  //    globalGroupDefs ++

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

