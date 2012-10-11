package delimsearch.io
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.io.InputStream

/**
 * This class was ported from java in order to use our customized
 * version of the StreamDecoder.
 * 
 * This was necessary because in DFDL we want malformed input to be
 * treated as the end of a data stream.
 */
class DFDLJavaIOInputStreamReader(val in : InputStream, val sd : DFDLJavaIOStreamDecoder) 
extends java.io.Reader(in : InputStream) {

  def this(in : InputStream, charsetName : String) = {
    this(in, DFDLJavaIOStreamDecoder.forInputStreamReader(in, new Object, {
      if (charsetName == null) { throw new NullPointerException("charsetName") }
      charsetName
    }))
  }

  def this(in : InputStream, cs : Charset) = {
    this(in, DFDLJavaIOStreamDecoder.forInputStreamReader(in, new Object, {
      if (cs == null) { throw new NullPointerException("charset") }
      cs
    }))
  }

  def this(in : InputStream, dec : CharsetDecoder) = {
    this(in, DFDLJavaIOStreamDecoder.forInputStreamReader(in, new Object, {
      if (dec == null) { throw new NullPointerException("charset decoder") }
      dec
    }))
  }

  def getEncoding : String = sd.getEncoding
  override def read : Int = sd.read()
  def read(cbuf : Array[Char], offset : Int, length : Int) : Int = sd.read(cbuf, offset, length)
  override def ready : Boolean = sd.ready()
  def close : Unit = sd.close()
}
