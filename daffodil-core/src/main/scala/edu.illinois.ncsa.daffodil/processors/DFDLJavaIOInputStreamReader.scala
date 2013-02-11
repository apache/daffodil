package edu.illinois.ncsa.daffodil.processors

import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.io.InputStream

object DFDLJavaIOInputStreamReader {

  def apply(in: InputStream, charset: Charset, bitOffset0to7: Int, bitLimit: Long) = {
    new DFDLJavaIOInputStreamReader(in, DFDLJavaIOStreamDecoder.forInputStreamReader(in, charset, bitOffset0to7, bitLimit))
  }

}

/**
 * This class was ported from java in order to use our customized
 * version of the StreamDecoder.
 *
 * This was necessary because in DFDL we want malformed input to be
 * treated as the end of a data stream.
 */
class DFDLJavaIOInputStreamReader private (val in: InputStream, val sd: DFDLJavaIOStreamDecoder)
  extends java.io.Reader(in: InputStream) {

  def getEncoding: String = sd.getEncoding
  override def read: Int = sd.read()
  def read(cbuf: Array[Char], offset: Int, length: Int): Int = sd.read(cbuf, offset, length)
  override def ready: Boolean = sd.ready()
  def close: Unit = sd.close()
}
