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
    lazy val localProp = combinedLocalProperties.get(name)
    localProp match {
      case Some(_) => localProp
      case None => annotatedSC.schemaDocument.defaultProperties.get(name)
    }
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

class DFDLDefineFormat(node: Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with DefineFormat_AnnotationMixin {
  lazy val formatAnnotation = Utility.trim(node) match {
    case <dfdl:defineFormat>{ f @ <dfdl:format>{ contents @ _* }</dfdl:format> }</dfdl:defineFormat> =>
      new DFDLFormat(f, NoSchemaDocument)
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


