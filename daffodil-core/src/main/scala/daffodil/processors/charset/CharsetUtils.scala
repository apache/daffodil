package daffodil.processors.charset
import daffodil.exceptions.Assert
import java.nio.charset.Charset
import com.ibm.icu.charset.CharsetICU
import java.nio.charset.IllegalCharsetNameException
import java.io.UnsupportedEncodingException

object CharsetUtils {

  def getCharset(charsetName: String): Charset = {
    // We should throw if csn is null. Tolerating this would just lead to bugs.
    Assert.usage(charsetName != null)
    Assert.usage(charsetName != "")

    // There is no notion of a default charset in DFDL.
    // So this can be val.
    val csn: String = charsetName

    val cs = try {
      val cs =
        if (csn.toUpperCase() == "US-ASCII-7-BIT-PACKED") USASCII7BitPackedCharset
        else CharsetICU.forNameICU(csn)
      Some(cs)
    } catch {
      case e: IllegalCharsetNameException => None
    }
    cs.getOrElse(throw new UnsupportedEncodingException(csn))
  }
}