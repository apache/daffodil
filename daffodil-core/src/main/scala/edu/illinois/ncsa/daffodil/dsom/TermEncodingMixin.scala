package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.processors.EncodingRuntimeData
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.Representation
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors.KnownEncodingMixin

trait TermEncodingMixin extends KnownEncodingMixin { self: Term =>

  /**
   * Character encoding common attributes
   *
   * Note that since encoding can be computed at runtime, we
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */

  final lazy val isKnownEncoding = {
    val isKnown = encoding.isConstant
    if (isKnown) {
      val encName = encoding.constantAsString.toUpperCase()
      if (encName.startsWith("UTF-16")) {
        utf16Width // demand this so checking is done
        true
      }
    }
    isKnown
  }

  /**
   * When the encoding is known, this tells us the mandatory
   * alignment required. This is always 1 or 8.
   * <p>
   * We only have one non-8-bit encoding right now, but there
   * are some 5, 6, and 9 bit encodings out there.
   */
  override final lazy val knownEncodingAlignmentInBits = {
    if (isKnownEncoding) {
      knownEncodingName match { // canonical form of encoding names is all upper case
        case "US-ASCII-7-BIT-PACKED" => {
          SDW("Character set encoding name US-ASCII-7-BIT-PACKED is deprecated. Please update your DFDL schema to use the name X-DFDL-US-ASCII-7-BIT-PACKED.")
          1
        }
        case "X-DFDL-US-ASCII-7-BIT-PACKED" => 1 // new official name.
        case "X-DFDL-US-ASCII-6-BIT-PACKED" => 1 // 6 bit is in the official DFDL standard too
        case _ => 8
      }
    } else 8 // unknown encodings always assumed to be 8-bit aligned.
  }

  lazy val encodingInfo =
    new EncodingRuntimeData(termRuntimeData, schemaFileLocation, encoding, optionUTF16Width, defaultEncodingErrorPolicy,
      termChildrenEncodingInfo, summaryEncoding, isKnownEncoding, isScannable, knownEncodingAlignmentInBits)

  private lazy val termChildrenEncodingInfo: Seq[EncodingRuntimeData] = termChildren.map { _.encodingInfo }

  /**
   * True if this element itself consists only of text. No binary stuff like alignment
   * or skips.
   * <p>
   * Not recursive into contained children.
   */
  final lazy val isLocallyTextOnly: Boolean = {
    val res = this match {
      case eb: ElementBase => {
        eb.hasNoSkipRegions &&
          hasTextAlignment &&
          ((eb.isSimpleType && eb.impliedRepresentation == Representation.Text) ||
            eb.isComplexType)
      }
      case mg: ModelGroup => {
        mg.hasNoSkipRegions &&
          hasTextAlignment
      }
      case gr: GroupRef => {
        gr.group.isLocallyTextOnly
      }
    }
    res
  }
  /**
   * True if it is sensible to scan this data e.g., with a regular expression.
   * Requires that all children have same encoding as enclosing groups and
   * elements, requires that there is no leading or trailing alignment regions,
   * skips. We have to be able to determine that we are for sure going to
   * always be properly aligned for text.
   * <p>
   * Caveat: we only care that the encoding is the same if the term
   * actually could have text (couldHaveText is an LV) as part of its
   * representation. For example, a sequence
   * with no initiator, terminator, nor separators can have any encoding at all,
   * without disqualifying an element containing it from being scannable. There
   * has to be text that would be part of the scan.
   * <p>
   * If the root element isScannable, and encodingErrorPolicy is 'replace',
   * then we can use a lower-overhead I/O layer - basically we can use a java.io.InputStreamReader
   * directly.
   * <p>
   * We are going to depend on the fact that if the encoding is going to be this
   * X-DFDL-US-ASCII-7-BIT-PACKED thingy (7-bits wide code units, so aligned at 1 bit) that
   * this encoding must be specified statically in the schema.
   * <p>
   * If an encoding is determined at runtime, then we will
   * insist on it being 8-bit aligned code units.
   */

  final lazy val isScannable: Boolean = {
    if (!isRepresented) true
    else {
      val res = summaryEncoding match {
        case Mixed => false
        case Binary => false
        case NoText => false
        case Runtime => false
        case _ => true
      }
      res
    }
  }

  /**
   * If s1 and s2 are the same encoding name
   * then s1, else "mixed". Also "notext" combines
   * with anything.
   */
  private def combinedEncoding(
    s1: EncodingLattice,
    s2: EncodingLattice): EncodingLattice = {
    (s1, s2) match {
      case (x, y) if (x == y) => x
      case (Mixed, _) => Mixed
      case (_, Mixed) => Mixed
      case (Binary, Binary) => Binary
      case (Binary, _) => Mixed
      case (_, Binary) => Mixed
      case (NoText, x) => x
      case (x, NoText) => x
      case (x, y) => Mixed
    }
  }

  /**
   * Roll up from the bottom. This is abstract interpretation.
   * The top (aka conflicting encodings) is "mixed"
   * The bottom is "noText" (combines with anything)
   * The values are encoding names, or "runtime" for expressions.
   * <p>
   * By doing expression analysis we could do a better job
   * here and determine when things that use expressions
   * to get the encoding are all going to get the same
   * expression value. For now, if it is an expression
   * then we lose.
   */
  final lazy val summaryEncoding: EncodingLattice = {
    val myEnc = if (!isRepresented) NoText
    else if (!isLocallyTextOnly) Binary
    else if (!couldHaveText) NoText
    else if (!isKnownEncoding) Runtime
    else NamedEncoding(this.knownEncodingName)
    val childEncs: Seq[EncodingLattice] = termChildren.map { x => x.summaryEncoding }
    val res = childEncs.fold(myEnc) { (x, y) => combinedEncoding(x, y) }
    res
  }

  /**
   * Returns true if this term either cannot conflict because it has no textual
   * aspects, or if it couldHaveText then the encoding must be same.
   */
  //  private def hasCompatibleEncoding(t2: Term): Boolean = {
  //    Assert.usage(isKnownEncoding)
  //    Assert.usage(t2.isKnownEncoding)
  //    if (!couldHaveText) true
  //    else if (!t2.couldHaveText) true
  //    else this.knownEncodingCharset == t2.knownEncodingCharset
  //  }

  /**
   * no alignment properties that would explicitly create
   * a need to align in a way that is not on a suitable boundary
   * for a character.
   */
  final lazy val hasTextAlignment = {
    val av = alignmentValueInBits
    val kav = this.knownEncodingAlignmentInBits
    av % kav == 0
  }

  private val RawByte = """\%\#r([0-9a-fA-F]{2})\;""".r
  /**
   * -1 means must be determined once encoding is known, which is at runtime.
   * Otherwise integer that contains the byte value.
   */
  final lazy val fillByteValue: Int = LV('fillByteValue) {
    fillByte match {
      case RawByte(hex) => {
        val cooked = EntityReplacer { e => e.replaceAll(fillByte, Some(self)) }
        Assert.invariant(cooked.length == 1)
        val char = cooked(0)
        val charCode = char.toInt
        charCode
      }
      case _ => fillByteForCharacter(fillByte)
    }
  }.value

  private def fillByteForCharacter(fillByte: String): Int = {
    val cookedFillByte: String = EntityReplacer { e => e.replaceAll(fillByte) }
    summaryEncoding match {
      case NamedEncoding(encName) => fillByteForCharacterKnownEncoding(encName, cookedFillByte)
      case Runtime => {
        this.subsetError("The dfdl:fillByte property value cannot be specified as a character when the dfdl:encoding property is a computed expression.")
        // -1
      }
      case Binary | NoText | Mixed => {
        if (isKnownEncoding) fillByteForCharacterKnownEncoding(knownEncodingName, cookedFillByte)
        else schemaDefinitionError("Illegal value for fillByte property: '%s'." +
          "\nData with binary representation, or with a mixture of text and binary representations," +
          " but with no dfdl:encoding property must have dfdl:fillByte specified using a DFDL Byte Value Entity" +
          " For example: %%#rHH; where H is a hex digit 0-9 A-F.", fillByte)
      }
    }
  }

  private def fillByteForCharacterKnownEncoding(encName: String, fillByteChar: String): Int = {
    if (knownEncodingIsFixedWidth) {
      if (knownEncodingWidthInBits == 8) {
        //
        // ok. Character has to be a single byte character.
        // we have to convert it to the byte value for that encoding
        //
        val bytes = fillByteChar.getBytes(encName)
        Assert.invariant(bytes.length == 1)
        bytes(0)
      } else {
        schemaDefinitionError("The fillByte property cannot be specified as a character ('%s') when the dfdl:encoding property is '%s' because that encoding is not a single-byte character set.", fillByte, encName)
      }
    } else {
      // not fixed width.
      val bytes = fillByteChar.getBytes(encName)
      if (bytes.length > 1) {
        schemaDefinitionError("The fillByte must be a single-byte character, for encoding '%s', but the specified character '%s' occupies %n bytes", encName, fillByteChar, bytes.length)
      } else {
        // ok. Must be exactly one.
        bytes(0)
      }
    }
  }
}
