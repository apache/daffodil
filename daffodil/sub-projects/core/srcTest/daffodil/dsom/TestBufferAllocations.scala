package daffodil.dsom

import org.scalatest.junit.JUnit3Suite
import daffodil.grammar.{InStreamFromByteChannel, PState}

/**
 * Created with IntelliJ IDEA.
 * User: Jeffrey C, Jacobs
 * Date: 6/15/12
 * Time: 7:39 PM
 * To change this template use File | Settings | File Templates.
 */

class TestBufferAllocations extends JUnit3Suite {

  def testMakeTooSmallBufferAndRead() {
    var in = Compiler.stringToReadableByteChannel("One Two Three Four, Can I have a little more, Five Six Seven Eight Nine Ten I love you!")
    val inStream = new InStreamFromByteChannel(in, 3)
    // This should not be true as the byte buffer should have spare capacity at the end meaning it read everything
    assert(inStream.bb.capacity < inStream.count)
  }
}
