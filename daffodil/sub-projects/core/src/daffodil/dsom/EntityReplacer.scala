package daffodil.dsom

import java.util.regex.Matcher
import java.util.regex.Pattern

class EntityReplacer {

  lazy val dfdlEntityName = "NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC[1-4]|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|NBSP|NEL|LS"

  lazy val entityCharacterUnicode: List[(String, String, Pattern)] = List(("NUL", "\u0000", Pattern.compile("%" + "NUL" + ";", Pattern.MULTILINE)),
    ("SOH", "\u0001", Pattern.compile("%" + "SOH" + ";", Pattern.MULTILINE)),
    ("STX", "\u0002", Pattern.compile("%" + "STX" + ";", Pattern.MULTILINE)),
    ("ETX", "\u0003", Pattern.compile("%" + "ETX" + ";", Pattern.MULTILINE)),
    ("EOT", "\u0004", Pattern.compile("%" + "EOT" + ";", Pattern.MULTILINE)),
    ("ENQ", "\u0005", Pattern.compile("%" + "ENQ" + ";", Pattern.MULTILINE)),
    ("ACK", "\u0006", Pattern.compile("%" + "ACK" + ";", Pattern.MULTILINE)),
    ("BEL", "\u0007", Pattern.compile("%" + "BEL" + ";", Pattern.MULTILINE)),
    ("BS", "\u0008", Pattern.compile("%" + "BS" + ";", Pattern.MULTILINE)),
    ("HT", "\u0009", Pattern.compile("%" + "HT" + ";", Pattern.MULTILINE)),
    ("LF", "\u000A", Pattern.compile("%" + "LF" + ";", Pattern.MULTILINE)),
    ("VT", "\u000B", Pattern.compile("%" + "VT" + ";", Pattern.MULTILINE)),
    ("FF", "\u000C", Pattern.compile("%" + "FF" + ";", Pattern.MULTILINE)),
    ("CR", "\u000D", Pattern.compile("%" + "CR" + ";", Pattern.MULTILINE)),
    ("SO", "\u000E", Pattern.compile("%" + "SO" + ";", Pattern.MULTILINE)),
    ("SI", "\u000F", Pattern.compile("%" + "SI" + ";", Pattern.MULTILINE)),
    ("DLE", "\u0010", Pattern.compile("%" + "DLE" + ";", Pattern.MULTILINE)),
    ("DC1", "\u0011", Pattern.compile("%" + "DC1" + ";", Pattern.MULTILINE)),
    ("DC2", "\u0012", Pattern.compile("%" + "DC2" + ";", Pattern.MULTILINE)),
    ("DC3", "\u0013", Pattern.compile("%" + "DC3" + ";", Pattern.MULTILINE)),
    ("DC4", "\u0014", Pattern.compile("%" + "DC4" + ";", Pattern.MULTILINE)),
    ("NAK", "\u0015", Pattern.compile("%" + "NAK" + ";", Pattern.MULTILINE)),
    ("SYN", "\u0016", Pattern.compile("%" + "SYN" + ";", Pattern.MULTILINE)),
    ("ETB", "\u0017", Pattern.compile("%" + "ETB" + ";", Pattern.MULTILINE)),
    ("CAN", "\u0018", Pattern.compile("%" + "CAN" + ";", Pattern.MULTILINE)),
    ("EM", "\u0019", Pattern.compile("%" + "EM" + ";", Pattern.MULTILINE)),
    ("SUB", "\u001A", Pattern.compile("%" + "SUB" + ";", Pattern.MULTILINE)),
    ("ESC", "\u001B", Pattern.compile("%" + "ESC" + ";", Pattern.MULTILINE)),
    ("FS", "\u001C", Pattern.compile("%" + "FS" + ";", Pattern.MULTILINE)),
    ("GS", "\u001D", Pattern.compile("%" + "GS" + ";", Pattern.MULTILINE)),
    ("RS", "\u001E", Pattern.compile("%" + "RS" + ";", Pattern.MULTILINE)),
    ("US", "\u001F", Pattern.compile("%" + "US" + ";", Pattern.MULTILINE)),
    ("SP", "\u0020", Pattern.compile("%" + "SP" + ";", Pattern.MULTILINE)),
    ("DEL", "\u007F", Pattern.compile("%" + "DEL" + ";", Pattern.MULTILINE)),
    ("NBSP", "\u00A0", Pattern.compile("%" + "NBSP" + ";", Pattern.MULTILINE)),
    ("NEL", "\u0085", Pattern.compile("%" + "NEL" + ";", Pattern.MULTILINE)),
    ("LS", "\u2028", Pattern.compile("%" + "LS" + ";", Pattern.MULTILINE)))

  lazy val escapeReplacements: List[(String, String, Pattern)] = List(("%", "\u0025", Pattern.compile("%%", Pattern.MULTILINE)))

  lazy val charEntityPattern = Pattern.compile("%(" + dfdlEntityName + ");", Pattern.MULTILINE)
  lazy val hexPattern = Pattern.compile("%#x[0-9a-fA-F]+;", Pattern.MULTILINE)
  lazy val decPattern = Pattern.compile("%#[0-9]+;", Pattern.MULTILINE)

  def hasDfdlCharEntity(input: String): Boolean = {
    val p: Pattern = charEntityPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasDecimalCodePoint(input: String): Boolean = {
    val p: Pattern = decPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def hasHexCodePoint(input: String): Boolean = {
    val p: Pattern = hexPattern
    val m: Matcher = p.matcher(input)
    m.find()
  }

  def replaceHex(input: String): String = {
    var res: String = input

    // While we have Hex Code Points in the string
    // Find and replace with their character equivalents.
    while (hasHexCodePoint(res)) {
      val p: Pattern = hexPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace("%#x", "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr, 16)

        res = res.replaceAll(rawStr, intStr.asInstanceOf[Char].toString())
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceDecimal(input: String): String = {
    var res: String = input

    // While we have Decimal Code Points in the string
    // Find and replace with their character equivalents.
    while (hasDecimalCodePoint(res)) {
      val p: Pattern = decPattern
      var m: Matcher = p.matcher(res)

      if (m.find()) {
        val rawStr = m.group().toString()
        val trimmedStr = rawStr.replace("%#", "").replace(";", "")
        val intStr = Integer.parseInt(trimmedStr)

        res = res.replaceAll(rawStr, intStr.asInstanceOf[Char].toString())
        m = p.matcher(res) // update Matcher
      }
    }

    res
  }

  def replaceDfdlEntity(input: String): String = {
    replace(input, entityCharacterUnicode)
  }

  def replaceEscapes(input: String): String = {
    replace(input, escapeReplacements)
  }

  def replaceAll(input: String): String = {
    var res: String = input

    res = replaceHex(res)
    res = replaceDecimal(res)
    res = replace(res, entityCharacterUnicode)
    res = replace(res, escapeReplacements)

    res
  }

  // Replacement helper function
  // 	entity = what you are replacing (informational only)
  // 	unicode = what you are replacing it with
  // 	pattern = what you are replacing via RegEx match on this pattern
  private def replace(input: String, chars: List[(String, String, Pattern)]): String = {
    var res: String = input
    chars.foreach {
      x =>
        {
          val (entity, unicode, pattern) = x
          val m: Matcher = pattern.matcher(res)
          res = m.replaceAll(unicode)
        }
    }
    res
  }

}