package daffodil.dsom

import java.io.File
import scala.xml.{ XML, Utility, Node }
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import daffodil.compiler._
import daffodil.Implicits._
import daffodil.dsom._
import daffodil.schema.annotation.props.gen.{ YesNo, TextNumberRep, SeparatorPosition, Representation, OccursCountKind, NilKind, LengthKind, ChoiceLengthKind, ByteOrder, BinaryNumberRep, AlignmentUnits }
import daffodil.schema.annotation.props.AlignmentType
import daffodil.util.{ TestUtils, Misc, Logging }
import daffodil.xml.XMLUtils
import daffodil.exceptions.SchemaFileLocatable
import junit.framework.Assert.{ assertTrue, assertEquals, assertFalse, fail }

class HasProps(xml: Node) extends DFDLNonDefaultFormatAnnotation(xml, Fakes.fakeElem)
  with ImplementsThrowsSDE {

}

class TestPropertyScoping extends JUnitSuite {
  val x1 = new HasProps(<fake alignmentUnits="bytes"/>)

  @Test def test1() {
    //    println(x1.formatRefs)
    //    println(x1.shortFormProperties)
    //    println(x1.longFormProperties)
    //    println(x1.elementFormProperties)
    assertTrue(x1.verifyPropValue("alignmentUnits", "bytes"))
  }

}