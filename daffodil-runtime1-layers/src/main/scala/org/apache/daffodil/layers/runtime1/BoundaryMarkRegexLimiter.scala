package org.apache.daffodil.layers.runtime1

import java.io.InputStream
import java.io.OutputStream
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.LayerBoundaryMarkInsertingJavaOutputStream
import org.apache.daffodil.io.RegexLimitingStream
import org.apache.daffodil.runtime1.layers.api.LayerLimiter
import org.apache.daffodil.runtime1.layers.api.LayerRuntime

/**
 * Reusable implementation mixin that allows an explicit (not runtime computed)
 * boundary mark to be a regex. This allows for look-ahead behaviors, such as
 * the boundary mark being a CRLF *not* followed by a space or tab.
 */
abstract class BoundaryMarkRegexLimiter extends LayerLimiter {

  /**
   * Regular expression that matches the boundary mark text.
   *
   * NOTE: this regex cannot contain ANY capturing groups (per scaladoc on RegexLimitingStream)
   */
  protected def regexForBoundaryMarkMatch: String

  /**
   * String that is the maximumLength boundaryMark.
   * This is only used for its length.
   */
  protected def maximumLengthBoundaryMark: String

  /**
   * This boundary mark is inserted when unparsing after the layer-encoded content.
   */
  protected def boundaryMarkToInsert: String

  override def wrapLayerLimitingInputStream(
    jis: InputStream,
    lrd: LayerRuntime,
  ) = {
    val s =
      new RegexLimitingStream(
        jis,
        regexForBoundaryMarkMatch,
        maximumLengthDelimiterExample = maximumLengthBoundaryMark,
        StandardCharsets.ISO_8859_1,
      )
    s
  }

  override def wrapLayerLimitingOutputStream(
    jos: OutputStream,
    lrd: LayerRuntime,
  ) = {
    val newJOS =
      new LayerBoundaryMarkInsertingJavaOutputStream(
        jos,
        boundaryMarkToInsert,
        StandardCharsets.ISO_8859_1,
      )
    newJOS
  }
}
