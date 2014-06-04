package edu.illinois.ncsa.daffodil.compiler

/**
 * Size and length limit constants used by the code, some of which will be tunable
 * by the user. Turning them to lower sizes/lengths may improve performance and
 * diagnostic behavior when a format does not need their full range,
 * both by reducing memory footprint, but
 * also by reducing the amount of time taken to scan to the end of what is allowed
 * and fail (and backtrack to try something else) when, for an example, a delimiter
 * is missing from the data.
 */
object DaffodilTunableParameters {

  //FIXME: These tunables need to be changable per compilation hence
  //stored on the ProcessorFactory, not global like this. 

  //Some - like time limits, are settable on the processor, not the PF or Compiler.
  //TODO: make tunable via setter call on PF.
  //
  // A few of these seem like runtime limits, but they get compiled into regular expressions
  // that we generate.

  def maxFieldContentLengthInBytes: Long = 1024 // Can be as large as Int.MaxValue
  def maxOccursBounds: Long = 1024 // Can be as large as Int.MaxValue
  def maxSkipLength: Long = 1024 // applicable to leadingSkip and trailingSkip
  def maxBinaryDecimalVirtualPoint: Int = 200 // Can be as large as Int.MaxValue
  def minBinaryDecimalVirtualPoint: Int = -200 // Can be as small as Int.MinValue

  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.

  def generatedNamespacePrefixStem = "tns"

  def readerByteBufferSize: Long = 8192

  /**
   * When unexpected text is found where a delimiter is expected, this is the maximum
   * number of bytes (characters) to display when the expected delimiter is a variable
   * length delimiter.
   */
  def maxLengthForVariableLengthDelimiterDisplay: Int = 10 // will display this number of bytes

  /**
   * In certain I/O optimized situations (text-only, encodingErrorPolicy='replace', fixed-width encoding)
   * input files larger than this will be mmapped. Input files smaller than this
   * will be simply read using ordinary I/O (because for small files that is just faster).
   * This exists because mmap is more expensive than ordinary I/O for small files.
   */
  def inputFileMemoryMapLowThreshold: Long = 32 * 1024 * 1024 // 32Meg

  /**
   * TODO: In the future, when we can stream and handle input larger than the JVM single
   * object limits, input files larger than this will be streamed, i.e., using java.io.InputStream.
   * They will not be memory mapped. A CharBuffer 2x larger may be created, and that
   * cannot exceed the JVM maximum size, so this has to be no bigger (and perhaps quite a bit
   * smaller) than 1/2 the maximum JVM object size.
   */
  // def inputFileMemoryMapHighThreshold: Long = 256 * 1024 * 1024 // 256 Meg

}
