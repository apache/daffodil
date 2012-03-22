package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation(node: Node, annotatedSC: AnnotatedMixin) {
  lazy val xml = node

  private [dsom] 
  def getLocalFormatRef(): String = {
    val ref = xml \ "@ref"
    ref.text
  }

  lazy val detailName = xml.label

  //
  // Always look for a local property first
  //
  // package private since we want to unit test these and put the test code in a different object.
  // (Note: I hate repeating the darn package name all over the place here....)
  private [dsom] def getLocalPropertyOption(name: String): Option[String] = {
    if (hasConflictingPropertyError) {
      throw new DFDLSchemaDefinitionException("Short and Long form properties overlap: " + conflictingProperties)
    }
    lazy val localProp = combinedLocalProperties.get(name)
    localProp
  }


  //
  // reference chain lookup
  //
  private [dsom] def getRefPropertyOption(qName : String, pName :
String) : Option[String] = {
    None
    // replace with call to taylor's loop detecting reference chasing code.
    // first it gets the map using the qname (via lazy val so it's done once)
    // then it gets the pname from the map.
    var refStack: Set[String] = Set.empty[String]
    lazy val props: Map[String, String] = getDefineFormatPropertiesByRef(qName, refStack)
    lazy val propOpt: Option[String] = props.find( p => p._1 == pName) match {
      case None => None
      case Some( x ) => Option(x._1)
    }
    propOpt
  }

  //
  // NoDefault - local then reference chain rooted here
  //
  private [dsom] def getPropertyOptionNoDefault(name : String) :
Option[String] = {
    val local = getLocalPropertyOption(name)
    local match {
      case Some(_) => local
      case None => {
        val ref = getLocalFormatRef() //getLocalPropertyOption("ref")
//        ref match {
//          case Some(qname) => {
//            val refChainProp = getRefPropertyOption(qname, name)
//            refChainProp
//          }
//          case None => None
//        }
        if (ref.length() > 0) {
          val refChainProp = getRefPropertyOption(ref, name)
          refChainProp
        }
        else { None }
      }
    }
  }

 //
 // default - get the lexically enclosing default format annotation  // and do a no-default lookup on it (which looks locally there, and on the reference chain rooted  // at that default format annotation.
 //
 private [dsom] def getDefaultPropertyOption(name : String) : Option[String] = {
    val lexicalDefaultFormatAnnotation = annotatedSC.schemaDocument.formatAnnotation
    val prop = lexicalDefaultFormatAnnotation.getPropertyOptionNoDefault(name)
// no default for the default.
    prop
  }

 //
 // This is the primary public entry point.
 //
 def getPropertyOption(name: String): Option[String] = {
  // Assert.usage(name != "ref", "ref is not a format property")
  // Assert.usage(name != "name", "name is not a format property")
    val nonDef = getPropertyOptionNoDefault(name) // local and local ref chain
    nonDef match {
      case Some(_) => nonDef
      case None => {
        val default = getDefaultPropertyOption(name) // default and default ref chain
        default
      }
    }
  }

//  def getPropertyOption(name: String): Option[String] = {
//    if (hasConflictingPropertyError) {
//      throw new DFDLSchemaDefinitionException("Short and Long form properties overlap: " + conflictingProperties)
//    }
//
//    val result = allProperties.get(name)
//    result
//  }

  //lazy val formatRef = combinedLocalProperties.get("ref")
  //lazy val refProp = formatRef.map { getFormatProperties(_) }
 // lazy val allProperties = refProp match {
 //   case Some(pMap) => combinePropertiesWithOverriding(combinedLocalProperties, pMap)
 //   case None => combinedLocalProperties
 // }

  lazy val shortFormProperties = {
    val xsA = annotatedSC.xml
    xsA.attributes.asAttrMap.map {
      case (prop_name, value) if (prop_name.contains("dfdl:")) => (prop_name.replace("dfdl:", ""), value)
      case x => x
    }
  }.toSet

  lazy val longFormProperties = node.attributes.asAttrMap.toSet // No colon in name

  lazy val conflictingProperties =
    shortFormProperties.intersect(longFormProperties).union(
      shortFormProperties.intersect(elementFormProperties)).union(
        longFormProperties.intersect(elementFormProperties))

  lazy val hasConflictingPropertyError = conflictingProperties.size != 0

  lazy val combinedLocalProperties = shortFormProperties.union(longFormProperties).union(elementFormProperties).toMap

  lazy val elementFormProperties = (xml \ "property").map {
    case p @ <dfdl:property>{ value }</dfdl:property> => ((p \ "@name").text, value.text)
    case _ => Assert.impossibleCase()
  }.toSet

  // Added by Taylor Wise
  //
  def getQName(name: String): (String, String) = {
    val parts = name.split(":").toList
    val (prefix, localName) = parts match {
      case List(local) => ("", local)
      case List(pre, local) => (pre, local)
      case _ => Assert.impossibleCase()
    }
    val nsURI = xml.getNamespace(prefix) // should work even when there is no namespace prefix.
    // Assert.schemaDefinition(nsURI != null, "In QName " + name + ", the prefix " + prefix + " was not defined.")
    // TODO: accumulate errors, don't just throw on one.
    // TODO: error location for diagnostic purposes. 
    // see: http://stackoverflow.com/questions/4446137/how-to-track-the-source-line-location-of-an-xml-element
    (nsURI, localName)
  }

  // Added by Taylor Wise
  //
  def combinePropertiesWithOverriding(localProps: Map[String, String], refProps: Map[String, String]): Map[String, String] = {
    var result: Map[String, String] = localProps
    // Iterate over the ref's properties, if we find that
    // a local instance of that property exists, add the local instance
    // otherwise add the ref instance
    refProps foreach {
      case (refKey, refValue) => {
        val foundProp = localProps.find { p => p._1 == refKey }
        foundProp match {
          case Some(thisProp) => { /* Local version exists, don't add */ }
          case None => { result += (refKey -> refValue) }
        } // end-prop-match
      } // end-for-each-case
    } // end-for-each
    result
  }

  // Added by Taylor Wise
  //
  def getDefineFormatPropertiesByRef(qName: String, refStack: Set[String]): Map[String, String] = {
    var props = Map.empty[String, String]
    var localRefStack = refStack.toSet[String]

    val (nsURI, localName): (String, String) = getQName(qName)

    // Verify that we don't have circular references
    if (refStack.contains(localName)) {
      Assert.schemaDefinitionError("Circular reference detected for "
        + localName + " while obtaining Format Properties!\nStack: "
        + refStack.toString())
    }

    localRefStack = localRefStack + localName

    // Retrieve the defineFormat that matches this qName
    val ss = annotatedSC.schema.schemaSet
    val dfs = ss.getDefineFormats(nsURI)
    
    val foundDF = dfs.find { df => df.name == localName }
    foundDF match {
      case Some(aDF) => {
        // Found a defineFormat, grab its format properties
        val aFormat = aDF.formatAnnotation

        val ref: String = aFormat.getLocalFormatRef()

        // Does this format have a ref?
        if (ref.length() > 0) {
          // Local format properties
          val formatProps = aFormat.combinedLocalProperties

          // Add the local properties to the props list
          formatProps foreach { case (key, value) => props += (key -> value) }

          // Has a ref, go get the ref's properties
          val refFormatProps = getDefineFormatPropertiesByRef(ref, localRefStack)

          val result: Map[String, String] = combinePropertiesWithOverriding(formatProps, refFormatProps)
          props = props ++ result
        } else {
          // No ref, just return this format's properties
          val formatProps = aFormat.combinedLocalProperties
          props = formatProps
        } // end-if-else

      }
      case None => { /* Do Nothing */ }
    } // end-match

    props
  } // end-getDefineFormatProperties

  // Added by Taylor W.
  // 
  def getFormatProperties(): Map[String, String] = {
    var refStack: Set[String] = Set.empty[String]
    var props: Map[String, String] = Map.empty[String, String]
    
    // Fetch Local Format Properties
    val localProps = combinedLocalProperties.filterNot( x => x._1 == "ref" || x._1 == "name")

    // Fetch Default Format Properties
    val defaultProps = annotatedSC.schemaDocument.defaultFormat.combinedLocalProperties.filterNot(x => x._1 == "ref" || x._1 == "name")
    
    // Combine Local and Default properties via overriding
    props = combinePropertiesWithOverriding(localProps, defaultProps)
    
    val ref = getLocalFormatRef()
    val refProps = getDefineFormatPropertiesByRef(ref, refStack)
    
    val res: Map[String, String] = combinePropertiesWithOverriding(props, refProps)

    res
  } // end-getFormatProperties
}

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(node: Node, annotatedSC: AnnotatedMixin)
  extends DFDLAnnotation(node, annotatedSC)

/**
 * Base class for assertions, variable assignments, etc
 */
abstract class DFDLStatement(node: Node, annotatedSC: AnnotatedMixin)
  extends DFDLAnnotation(node, annotatedSC)

class DFDLFormat(node: Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd)
  with Format_AnnotationMixin
  with SeparatorSuppressionPolicyMixin {
}

class DFDLElement(node: Node, decl: AnnotatedElementMixin)
  extends DFDLFormatAnnotation(node, decl) with Element_AnnotationMixin {
}

class DFDLGroup(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) 
  with Group_AnnotationMixin 
  with SeparatorSuppressionPolicyMixin {
}

class DFDLSequence(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) 
  with Sequence_AnnotationMixin 
  with SeparatorSuppressionPolicyMixin {
}

class DFDLChoice(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Choice_AnnotationMixin {
}

class DFDLSimpleType(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with SimpleType_AnnotationMixin {
}

class DFDLDefineFormat(node: Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd) with DefineFormat_AnnotationMixin {
  lazy val formatAnnotation = Utility.trim(node) match {
    case <dfdl:defineFormat>{ f @ <dfdl:format>{ contents @ _* }</dfdl:format> }</dfdl:defineFormat> =>
      new DFDLFormat(f, sd)
    case _ => Assert.impossibleCase()
  }
}

class DFDLEscapeScheme(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with EscapeScheme_AnnotationMixin {
}

class DFDLDefineEscapeScheme(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with DefineEscapeScheme_AnnotationMixin {
  lazy val escapeScheme = Utility.trim(node) match {
    case <dfdl:defineEscapeScheme>{ e @ <dfdl:escapeScheme>{ contents @ _* }</dfdl:escapeScheme> }</dfdl:defineEscapeScheme> =>
      new DFDLEscapeScheme(e, NoSchemaDocument)
    case _ => Assert.impossibleCase()
  }
}

class DFDLAssert(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) with Assert_AnnotationMixin {
}

class DFDLDiscriminator(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) with Discriminator_AnnotationMixin {
  lazy val testBody = node.child.text
}

class DFDLDefineVariable(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) with DefineVariable_AnnotationMixin {
}

class DFDLNewVariableInstance(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) with NewVariableInstance_AnnotationMixin {
}

class DFDLSetVariable(node: Node, decl: AnnotatedMixin)
  extends DFDLStatement(node, decl) with SetVariable_AnnotationMixin {
}


