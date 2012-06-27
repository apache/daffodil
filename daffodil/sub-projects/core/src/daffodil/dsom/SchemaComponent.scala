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

/**
 * The core root class of the DFDL Schema object model.
 * 
 * Every schema component has a schema document, and a schema, and a namespace.
 */
abstract class SchemaComponent(val xml : Node)
  extends DiagnosticsProviding
  with GetAttributesMixin  {
  def schemaDocument: SchemaDocument
  lazy val schema: Schema = schemaDocument.schema
  lazy val namespace = schemaDocument.targetNamespace
  def prettyName : String
  
  def scPath : String
  lazy val path = scPath
  
  private val scala.xml.Elem(_, _, emptyXMLMetadata, _, _*) = <foo/> // hack way to get empty metadata object.
  
  /**
   * Used as factory for the XML Node with the right namespace and prefix etc.
   * 
   * Given "element" it creates <dfdl:element /> with the namespace definitions
   * based on this schema component's corresponding XSD construct.
   */
  def newDFDLAnnotationXML(label : String) = {
    scala.xml.Elem("dfdl", label, emptyXMLMetadata, xml.scope)
  }

  val NYI = false // our flag for Not Yet Implemented 

}

/**
 * Local components have a lexical parent that contains them
 */
trait LocalComponentMixin { self : SchemaComponent =>
  def parent: SchemaComponent
  lazy val schemaDocument = parent.schemaDocument
  
  lazy val scPath = {
    parent.scPath + "::" + prettyName
  }
  
}

/**
 * Anything named has a name from its corresponding xml's name attribute 
 */
trait NamedMixin { self : { def xml : Node } => // this scala idiom means the object this is mixed into has a def for xml of type Node
  lazy val name = (xml \ "@name").text
  lazy val detailName = name // TODO: remove synonym name unless there is a good reason for this.
  lazy val prettyName = name
}

/**
 * All global components share these characteristics.
 */
trait GlobalComponentMixin
  extends NamedMixin { self: SchemaComponent =>
    // nothing here yet
    override lazy val scPath = schemaDocument.scPath + "::" + prettyName
}


/**
 * Every component that can be annotated.
 */
trait AnnotatedMixin 
extends CommonRuntimeValuedPropertiesMixin { self : SchemaComponent =>
    
  def prettyName : String
  def path : String
  
  def localAndFormatRefProperties: Map[String,String]
  def defaultProperties: Map[String,String] = {
    this.schemaDocument.localAndFormatRefProperties
  }
  
  /**
   * Primary mechanism for a component to get a format property value.
   * 
   * Note: use only for format properties, not any old attribute. 
   */
  def getPropertyOption(pname: String) = {
    val local = localAndFormatRefProperties.get(pname) 
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
            (hasRightSource || isAcceptable) //TODO: remove lax check once examples & tests are updated.
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
  private lazy val annotationObjs_ = LV{
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
   * and isMyAnnotation which tests if an annotation is the corresponding kind.
   *
   * Given that, formatAnnotation then either finds the right annotation, or constructs one, but our invariant
   * is imposed. There *is* a formatAnnotation.
   */
  def emptyFormatFactory: DFDLFormatAnnotation
  def isMyAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation : DFDLFormatAnnotation = {
    val format = annotationObjs.find { isMyAnnotation(_) }
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
   * Needed by back-end to construct jdom nodes. 
   * 
   * An expression can be in any annotation, and its path can lead to a node
   * So, we need the namespace in which to create that node.
   */
  lazy val jdomNamespace = {
    val jdomns = org.jdom.Namespace.getNamespace(schemaDocument.targetNamespace)
    jdomns
  }
  
  /**
   * Needed by back-end to construct jdom nodes.
   */
  lazy val namespaces = {
    val nss = new Namespaces
    nss.addNamespace(jdomNamespace)
    nss
  }
  
  /**
   * Character encoding common attributes
   * 
   * Note that since encoding can be computed at runtime, we 
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */
 
  lazy val isKnownEncoding = encoding.isConstant
  
  lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = encoding.constantAsString.toUpperCase()
    res
  }
  
  lazy val knownEncodingCharset = {
    val charset = CharsetICU.forNameICU(knownEncodingName).asInstanceOf[CharsetICU]
    charset
  }
  
  lazy val knownEncodingDecoder = {
    val decoder = knownEncodingCharset.newDecoder()
    decoder
  }
  
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
      case "UTF-8" => false
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" | "UTF-32" | "UTF-32BE" | "UTF-32LE" => true
      case _ => Assert.notYetImplemented() // TODO change to SDE charset unsupported, not NYI.
    }
    res
  }

  lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth
  
  lazy val knownEncodingWidth = {
    // knownEncodingCharset.width()
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => 1
      case "UTF-8" => -1
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => 2
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => 4
      case _ => Assert.notYetImplemented() // TODO change to SDE charset unsupported, not NYI.
    }
    res
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
class SchemaSet(val schemaNodeList: Seq[Node]) 
extends DiagnosticsProviding {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.

  lazy val prettyName="schemaSet"
    
  lazy val scPath = ""
  lazy val path = scPath
    
  lazy val schemaPairs = schemaNodeList.map { s =>
    {
      val ns = (s \ "@targetNamespace").text
      (ns, s)
    }
  }

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
  
  lazy val diagnosticChildren = schemas

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
  def getDefineFormat(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineFormat(name) }
  def getDefineFormats(namespace: String) = getSchema(namespace) match {
    case None => Assert.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineVariable(name) }
  def getDefineEscapeScheme(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

  lazy val primitiveTypes = XMLUtils.DFDL_SIMPLE_BUILT_IN_TYPES.map{new PrimitiveType(_)}
  def getPrimitiveType(localName: String) = primitiveTypes.find{_.name == localName}

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
  lazy val scPath = ""
  lazy val path = scPath
    
  lazy val targetNamespace = namespace

  lazy val schemaDocuments = schemaDocs.map { new SchemaDocument(_, this) }
  
  lazy val diagnosticChildren = schemaDocuments

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc)
      case _ => Assert.SDE("more than one definition for name: " + name)
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
  def getDefineVariable(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineVariable(name) }, name)
  def getDefineEscapeScheme(name: String) = noneOrOne(schemaDocuments.flatMap { _.getDefineEscapeScheme(name) }, name)

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
  extends SchemaComponent(xmlArg)
  with AnnotatedMixin
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {
  
  lazy val prettyName = "schemaDoc"
  lazy val scPath = prettyName
    
  lazy val validatedXML = LV{
      XMLSchemaUtils.validateDFDLSchema(xml)
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
  
  lazy val detailName="" // TODO: Maybe a filename would be nice if available.

  override lazy val schema = schemaArg

  lazy val targetNamespace = schema.targetNamespace
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
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

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

  
  lazy val diagnosticChildren = {
    List(validatedXML, defaultFormat) ++
    globalElementDecls.map{ _.forRoot() } ++
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
  def getDefineVariable(name: String) = defineVariables.find { _.name == name }
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}

