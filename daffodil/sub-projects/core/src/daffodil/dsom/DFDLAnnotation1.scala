package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._

/**
 * Base class for any DFDL annotation
 */
abstract class DFDLAnnotation(node: Node, annotatedSC: AnnotatedMixin) {
  lazy val xml = node

  def getPropertyOption(name: String): Option[String] = {
    if (hasConflictingPropertyError) {
      throw new DFDLSchemaDefinitionException("Short and Long form properties overlap: " + conflictingProperties)
    }

    val result = allProperties.get(name)
    result
  }

  lazy val formatRef = combinedLocalProperties.get("ref")
  lazy val refProp = formatRef.map { getFormatProperties(_) }
  lazy val allProperties = refProp match {
    case Some(pMap) => combinePropertiesWithOverriding(combinedLocalProperties, pMap)
    case None => combinedLocalProperties
  }

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
    var result: Map[String, String] = Map.empty[String, String]
    // Iterate over the ref's properties, if we find that
    // a local instance of that property exists, do not add it.
    refProps foreach {
      case (refKey, refValue) => {
        val foundProp = localProps.find { p => p._1 == refKey }
        foundProp match {
          case Some(thisProp) => { /* Found, don't add! */ }
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

        val ref: String = aFormat.getPropertyOption("ref") match {
          case Some(x) => x
          case None => ""
        }

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
  def getFormatProperties(qName: String): Map[String, String] = {
    var refStack: Set[String] = Set.empty[String]
    var props: Map[String, String] = Map.empty[String, String]

    // Fetch Default Format Properties
    val defaultProps = annotatedSC.schemaDocument.defaultFormat.combinedLocalProperties

    // Append the default properties to props collection
    defaultProps foreach { case (key, value) => props += (key -> value) }

    props = props ++ getDefineFormatPropertiesByRef(qName, refStack)

    props
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
  extends DFDLFormatAnnotation(node, sd) with Format_AnnotationMixin {
}

class DFDLElement(node: Node, decl: AnnotatedElementMixin)
  extends DFDLFormatAnnotation(node, decl) with Element_AnnotationMixin {
}

class DFDLGroup(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Group_AnnotationMixin {
}

class DFDLSequence(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Sequence_AnnotationMixin {
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


