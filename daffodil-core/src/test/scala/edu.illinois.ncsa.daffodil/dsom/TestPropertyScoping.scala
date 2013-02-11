package edu.illinois.ncsa.daffodil.dsom

import java.io.File
import scala.xml.{ XML, Utility, Node }
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ YesNo, TextNumberRep, SeparatorPosition, Representation, OccursCountKind, NilKind, LengthKind, ChoiceLengthKind, ByteOrder, BinaryNumberRep, AlignmentUnits }
import edu.illinois.ncsa.daffodil.schema.annotation.props.AlignmentType
import edu.illinois.ncsa.daffodil.util.{ TestUtils, Misc, Logging }
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
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