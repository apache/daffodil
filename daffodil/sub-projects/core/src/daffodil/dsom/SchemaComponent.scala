package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.xml._
import daffodil.exceptions._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
/*
 * Not using XSOM - too many issues. Schema Documents aren't first class objects, and we need
 * them to implement lexical scoping of default formats over the contents of a schema document.
 * 
 * import com.sun.xml.xsom.parser.{ SchemaDocument => XSSchemaDocument, _ }
 * import com.sun.xml.xsom._
 * import com.sun.xml.xsom.util._
 */
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._

import daffodil.grammar._
import com.ibm.icu.charset.CharsetICU

trait SchemaComponent {
  def schemaDocument: SchemaDocument
  lazy val schema: Schema = schemaDocument.schema
  def xml: Node

  val NYI = false // our flag for Not Yet Implemented 

  lazy val expressionCompiler = new ExpressionCompiler(this)
}

trait LocalComponentMixin
  extends SchemaComponent {
  def parent: SchemaComponent
  lazy val schemaDocument = parent.schemaDocument
}

/**
 * Anything named has a name from its name attribute and a namespace from the
 * schema document it is part of.
 */
trait NamedMixin { self: SchemaComponent =>
  lazy val name = (xml \ "@name").text
  lazy val namespace = self.schemaDocument.targetNamespace
  lazy val detailName = name
}

trait GlobalComponentMixin
  extends SchemaComponent
  with NamedMixin



trait AnnotatedMixin 
extends SchemaComponent
with CommonRuntimeValuedPropertiesMixin {
  
  def getPropertyOption(pname: String) = formatAnnotation.getPropertyOption(pname)
    
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

  lazy val annotationObjs = {
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
  def emptyFormatFactory: DFDLAnnotation
  def isMyAnnotation(a: DFDLAnnotation): Boolean

  lazy val formatAnnotation = {
    val format = annotationObjs.find { isMyAnnotation(_) }
    val res = format match {
      case None => emptyFormatFactory
      case Some(x) => x
    }
    res
  }
  
  /**
   * Annotations can contain expressions, so we need to be able to compile them.
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
   */
  
 
  lazy val isKnownEncoding = encodingExpr.isConstant
  
  lazy val knownEncodingName = {
    Assert.invariant(isKnownEncoding)
    val res = encodingExpr.constant.asInstanceOf[String].toUpperCase()
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
  
  lazy val knownEncodingIsFixedWidth = {
    // val res = knownEncodingCharset.isFixedWidth
    val res = knownEncodingName match {
      case "US-ASCII" => true
      case "UTF-8" => false
      case _ => Assert.notYetImplemented()
    }
    res
  }

  lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth
  
  lazy val knownEncodingWidth = {
    // knownEncodingCharset.width()
    val res = knownEncodingName match {
      case "US-ASCII" => 1
      case "UTF-8" => -1
      case _ => Assert.notYetImplemented()
    }
    res
  }
  
}



abstract class Annotated(xmlArg: Node)
  extends SchemaComponent 
  with AnnotatedMixin {
  
  lazy val xml = xmlArg
  
  
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
class SchemaSet(schemaNodeList: Seq[Node]) {
  // TODO Constructor(s) or companion-object methods to create a SchemaSet from files.

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

  /**
   * Retrieve schema by namespace name.
   */
  def getSchema(namespace: String) = {
    // TODO: what about when there is no namespace. Can we pass "" ??
    val schemaForNamespace = schemas.find { s => s.targetNamespace == namespace }
    schemaForNamespace
  }

  /**
   * Given a namespace and name, try to retrieve the named object
   */
  def getGlobalElementDecl(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalElementDecl(name) }
  def getGlobalSimpleTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalSimpleTypeDef(name) }
  def getGlobalComplexTypeDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalComplexTypeDef(name) }
  def getGlobalGroupDef(namespace: String, name: String) = getSchema(namespace).flatMap { _.getGlobalGroupDef(name) }
  def getDefineFormat(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineFormat(name) }
  def getDefineFormats(namespace: String) = getSchema(namespace) match {
    case None => Assert.schemaDefinitionError("Failed to find a schema for namespace:  " + namespace)
    case Some(sch) => sch.getDefineFormats()
  }
  def getDefineVariable(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineVariable(name) }
  def getDefineEscapeScheme(namespace: String, name: String) = getSchema(namespace).flatMap { _.getDefineEscapeScheme(name) }

  lazy val primitiveTypes = XMLUtil.DFDL_SIMPLE_BUILT_IN_TYPES.map{new PrimitiveType(_)}
  def getPrimitiveType(localName: String) = primitiveTypes.find{_.name == localName}
}

/**
 * A schema is all the schema documents sharing a single target namespace.
 *
 * That is, one can write several schema documents which all have the
 * same target namespace, and in that case all those schema documents make up
 * the 'schema'.
 */
class Schema(val namespace: String, val schemaDocs: NodeSeq, val schemaSet: SchemaSet) {

  lazy val targetNamespace = namespace

  lazy val schemaDocuments = schemaDocs.map { new SchemaDocument(_, this) }

  private def noneOrOne[T](scs: Seq[T], name: String): Option[T] = {
    scs match {
      case Nil => None
      case Seq(sc) => Some(sc)
      case _ => throw new DFDLSchemaDefinitionException("more than one definition for name: " + name)
    }
  }

  /**
   * Given a name, try to retrieve the appropriate object.
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
class SchemaDocument(xmlArg: Node, schemaArg: => Schema)
  extends AnnotatedMixin
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {
  
  lazy val detailName="" // TODO: Maybe a filename would be nice if available.
  //
  // schemaArg is call by name, so that in the corner case of NoSchemaDocument (used for non-lexically enclosed annotation objects), 
  // we can pass an Assert.invariantFailed to bomb if anyone actually tries to use the schema.
  //
  // This is one of the techniques we use to avoid using null and having to test for null.
  //

  override lazy val schema = schemaArg
  lazy val xml = xmlArg
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

  def emptyFormatFactory = new DFDLFormat(<dfdl:format/>, this)
  def isMyAnnotation(a: DFDLAnnotation) = a.isInstanceOf[DFDLFormat]

  private lazy val sset = schema.schemaSet

  lazy val globalElementDecls = (xml \ "element").map { new GlobalElementDecl(_, this) }
  lazy val globalSimpleTypeDefs = (xml \ "simpleType").map { new GlobalSimpleTypeDef(_, this) }
  lazy val globalComplexTypeDefs = (xml \ "complexType").map { new GlobalComplexTypeDef(_, this) }
  lazy val globalGroupDefs = (xml \ "group").map { new GlobalGroupDef(_, this) }

  lazy val defaultFormat = formatAnnotation.asInstanceOf[DFDLFormat]

  lazy val defaultProperties = defaultFormat.combinedLocalProperties

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

  /**
   * Issue: cloning, so that each referer gets a copy of the object
   * TBD: don't design so as to rule-out recursion some day.
   */
  def getGlobalElementDecl(name: String) = globalElementDecls.find { _.name == name }
  def getGlobalSimpleTypeDef(name: String) = globalSimpleTypeDefs.find { _.name == name }
  def getGlobalComplexTypeDef(name: String) = globalComplexTypeDefs.find { _.name == name }
  def getGlobalGroupDef(name: String) = globalGroupDefs.find { _.name == name }
  def getDefineFormat(name: String) = defineFormats.find { _.name == name }
  def getDefineVariable(name: String) = defineVariables.find { _.name == name }
  def getDefineEscapeScheme(name: String) = defineEscapeSchemes.find { _.name == name }

}

/**
 * Singleton to use when something usually has a schema document it refers to, but sometimes doesn't but you
 * have to supply something. Use this.
 *
 * This is an alternative to everybody having to use Option[SchemaDocument] for these corner cases, or passing null, etc.
 */
object NoSchemaDocument extends SchemaDocument(
  <schema/>, // dummy piece of XML that has no attributes, no annotations, no children, etc. Convenient to avoid conditional tests.
  Assert.invariantFailed("object NoSchemaDocument has no schema."))

