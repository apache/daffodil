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
abstract class DFDLAnnotation

/**
 * Base class for annotations that carry format properties
 */
abstract class DFDLFormatAnnotation(node : Node, annotatedSC : AnnotatedMixin) extends DFDLAnnotation {
  
  lazy val xml = node
  
  def getPropertyOption(name: String) = {
    val a = xml.attribute(name)
    a match {
      case None => None
      case Some(ns) => Some(ns.text)
    }
  }
}

/**
 * Base class for assertions, variable assignments, etc
 */
abstract class DFDLStatement

class DFDLFormat(node : Node, sd: SchemaDocument)
  extends DFDLFormatAnnotation(node, sd) with Format_AnnotationMixin {
}

class DFDLElement(node : Node, decl: AnnotatedElementMixin)
  extends DFDLFormatAnnotation(node, decl) with Element_AnnotationMixin {
}

class DFDLGroup(node : Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Group_AnnotationMixin {
}

class DFDLSequence(node : Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Sequence_AnnotationMixin {
}

class DFDLChoice(node : Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with Choice_AnnotationMixin {
}

class DFDLSimpleType(node : Node, decl: AnnotatedMixin)
  extends DFDLFormatAnnotation(node, decl) with SimpleType_AnnotationMixin {
}
