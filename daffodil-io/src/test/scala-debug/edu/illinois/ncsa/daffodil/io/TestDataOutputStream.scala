package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import junit.framework.Assert._
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import org.apache.commons.io.IOUtils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder

class TestDataOutputStream {

  final def getString(baos: ByteArrayOutputStreamWithGetBuf) = {
    val is = new ByteArrayInputStream(baos.getBuf)
    val ir = new InputStreamReader(is, "ascii")
    val line = IOUtils.toString(ir)
    val res = line.replace("""\u0000""", "")
    res
  }

  @Test def test1 {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putLong(-1L, 32)

    out.flush()

    val buf = baos.getBuf()

    assertEquals(255, buf(0))
    assertEquals(255, buf(1))
    assertEquals(255, buf(2))
    assertEquals(255, buf(3))
    if (buf.length > 4)
      assertEquals(0, buf(4))

  }

}
