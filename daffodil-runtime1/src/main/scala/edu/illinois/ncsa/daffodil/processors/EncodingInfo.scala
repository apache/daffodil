package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation
import edu.illinois.ncsa.daffodil.exceptions.NoSchemaFileLocation
import edu.illinois.ncsa.daffodil.util.PreSerialization

/**
 * To eliminate circularities between RuntimeData objects and the
 * encoding compiled expression, all information derived from encodings
 * must come from one of these objects.
 *
 * That way we can construct this separately, the compilation of the
 * compiled expression for encoding can happily insist on a runtimeData object
 * existing to provide the information it generally needs.
 */

/**
 * This is the object we serialize.
 *
 * At compile time we will create an encodingInfo
 * for ourselves supplying as context a schema component.
 *
 * At runtime we will create an encodingInfo supplying as context
 * a TermRuntimeData object.
 */

class ISO8859EncodingInfo(trd: TermRuntimeData)
  extends EncodingInfo(
    trd,
    NoSchemaFileLocation,
    new ConstantExpression(NodeInfo.NonEmptyString, "ISO-8859-1"),
    None, // utf16Width is not relevant
    EncodingErrorPolicy.Replace, // don't care so use this value
    Nil // no term children to consider.
    )

class EncodingInfo(
  @transient termRuntimeDataArg: => TermRuntimeData,
  override val schemaFileLocation: SchemaFileLocation,
  val encoding: CompiledExpression,
  val optionUTF16Width: Option[UTF16Width],
  val defaultEncodingErrorPolicy: EncodingErrorPolicy,
  val termChildrenEncodingInfo: Seq[EncodingInfo])
  extends ImplementsThrowsSDE with PreSerialization {

  lazy val termRuntimeData = termRuntimeDataArg

  override def preSerialization: Any = {
    super.preSerialization
    termRuntimeData
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)

  /**
   * True if this element itself consists only of text. No binary stuff like alignment
   * or skips.
   * <p>
   * Not recursive into contained children.
   */
  lazy val isLocallyTextOnly: Boolean = {
    val res = termRuntimeData match {
      case erd: ElementRuntimeData => {
        erd.hasNoSkipRegions &&
          hasTextAlignment &&
          ((erd.isSimpleType && erd.impliedRepresentation == Representation.Text) ||
            erd.isComplexType)
      }
      case mgrd: ModelGroupRuntimeData => {
        mgrd.hasNoSkipRegions &&
          hasTextAlignment
      }
    }
    res
  }

  private lazy val utf16Width = {
    schemaDefinitionUnless(optionUTF16Width.isDefined, "Property utf16Width must be provided.")
    val res = optionUTF16Width.get
    schemaDefinitionUnless(res == UTF16Width.Fixed, "Property utf16Width='variable' not supported.")
    res
  }
  /**
   * Character encoding common attributes
   *
   * Note that since encoding can be computed at runtime, we
   * create values to tell us if the encoding is known or not
   * so that we can decide things at compile time when possible.
   */

  lazy val isKnownEncoding = {
    //
    // Be sure we check this. encodingErrorPolicy='error' is harder
    // to support because you have to get decode errors precisely
    // in that case. Means you can't do things like just use
    // a buffered reader since filling the buffer may encounter
    // the error even though the parser won't actually consume
    // that much of the data. 
    //
    schemaDefinitionUnless(defaultEncodingErrorPolicy == EncodingErrorPolicy.Replace,
      "Property encodingErrorPolicy='error' not supported.")
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
   * Note that the canonical form for encoding names is all upper case.
   */
  lazy val knownEncodingName = {
    if (!isKnownEncoding) "(computed at runtime)"
    else {
      val res = encoding.constantAsString.toUpperCase()
      res
    }
  }

  lazy val knownEncodingCharset = {
    Assert.invariant(isKnownEncoding)
    new DFDLCharset(knownEncodingName)
  }

  // Really bad idea. Don't save these. Decoders and Encoders are stateful
  // so they can't be precomputed here and reused without all sorts of 
  // thread issues and reset protocols.
  //  lazy val knownEncodingDecoder = {
  //    val decoder = knownEncodingCharset.newDecoder()
  //    decoder
  //  }
  //
  //  lazy val knownEncodingEncoder = {
  //    val encoder = knownEncodingCharset.newEncoder()
  //    encoder
  //  }

  /**
   * When the encoding is known, this tells us the mandatory
   * alignment required. This is always 1 or 8.
   * <p>
   * We only have one non-8-bit encoding right now, but there
   * are some 5, 6, and 9 bit encodings out there.
   */
  lazy val knownEncodingAlignmentInBits = {
    if (isKnownEncoding) {
      knownEncodingName match {
        case "US-ASCII-7-BIT-PACKED" => 1 // canonical form of encoding names is all upper case
        case _ => 8
      }
    } else 8 // unknown encodings always assumed to be 8-bit aligned.
  }

  /**
   * enables optimizations and random-access
   *
   * variable-width character sets require scanning to determine
   * their end.
   */
  lazy val knownEncodingIsFixedWidth = {
    if (!isKnownEncoding) false
    else {
      val res = knownEncodingName.toUpperCase match {
        case "US-ASCII" | "ASCII" => true
        case "US-ASCII-7-BIT-PACKED" => true
        case "UTF-8" => false
        case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
          if (utf16Width == UTF16Width.Fixed) true
          else false
        }
        case "UTF-32" | "UTF-32BE" | "UTF-32LE" => true
        case "ISO-8859-1" => true
        case _ => schemaDefinitionError("Text encoding '%s' is not supported.", knownEncodingName)
      }
      res
    }
  }
  
  lazy val knownEncodingIsUnicode = {
    if (!isKnownEncoding) { false }
    else { knownEncodingName.toUpperCase.startsWith("UTF") }
  }

  lazy val mustBeAnEncodingWith8BitAlignment = {
    !isKnownEncoding || knownEncodingAlignmentInBits == 8
  }

  lazy val couldBeVariableWidthEncoding = !knownEncodingIsFixedWidth

  lazy val knownEncodingWidthInBits = {
    // knownEncodingCharset.width()
    val res = knownEncodingName match {
      case "US-ASCII" | "ASCII" => 8
      case "US-ASCII-7-BIT-PACKED" => 7 // NOTE! 7-bit characters dense packed. 8th bit is NOT unused. 
      case "UTF-8" => -1
      case "UTF-16" | "UTF-16LE" | "UTF-16BE" => {
        if (utf16Width == UTF16Width.Fixed) 16
        else -1
      }
      case "UTF-32" | "UTF-32BE" | "UTF-32LE" => 32
      case "ISO-8859-1" => 8
      case _ => schemaDefinitionError("Text encoding '%s' is not supported.", knownEncodingName)
    }
    res
  }

  def knownEncodingStringBitLength(str: String) = {
    //
    // This will be called at runtime, so let's decide
    // what we can, and return an optimized function that 
    // has characteristics of the encoding wired down.
    //
    if (knownEncodingIsFixedWidth) {
      str.length * knownEncodingWidthInBits
    } else {
      // variable width encoding, so we have to convert each character 
      // We assume here that it will be a multiple of bytes
      // that is, that variable-width encodings are all some number
      // of bytes.
      str.getBytes(knownEncodingName).length * 8
    }
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
   * US-ASCII-7Bit-PACKED thingy (7-bits wide code units, so aligned at 1 bit) that
   * this encoding must be specified statically in the schema.
   * <p>
   * If an encoding is determined at runtime, then we will
   * insist on it being 8-bit aligned code units.
   */

  final lazy val isScannable: Boolean = {
    if (!termRuntimeData.isRepresented) true
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
  def combinedEncoding(
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
  lazy val summaryEncoding: EncodingLattice = {
    val myEnc = if (!termRuntimeData.isRepresented) NoText
    else if (!isLocallyTextOnly) Binary
    else if (!termRuntimeData.couldHaveText) NoText
    else if (!this.isKnownEncoding) Runtime
    else NamedEncoding(this.knownEncodingName)
    val childEncs: Seq[EncodingLattice] = termChildrenEncodingInfo.map { x => x.summaryEncoding }
    val res = childEncs.fold(myEnc) { (x, y) => combinedEncoding(x, y) }
    res
  }

  /**
   * Returns true if this term either cannot conflict because it has no textual
   * aspects, or if it couldHaveText then the encoding must be same.
   */
  def hasCompatibleEncoding(t2: EncodingInfo): Boolean = {
    if (!termRuntimeData.couldHaveText) true
    else if (!t2.termRuntimeData.couldHaveText) true
    else this.knownEncodingCharset == t2.knownEncodingCharset
  }

  /**
   * no alignment properties that would explicitly create
   * a need to align in a way that is not on a suitable boundary
   * for a character.
   */
  lazy val hasTextAlignment = {
    this.knownEncodingAlignmentInBits == termRuntimeData.alignmentValueInBits
  }
}

