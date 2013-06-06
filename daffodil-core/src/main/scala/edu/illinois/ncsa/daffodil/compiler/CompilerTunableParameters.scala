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
object CompilerTunableParameters {

  //FIXME: These tunables need to be changable per compilation hence
  //stored on the ProcessorFactory, not global like this. 

  //Some - like time limits, are settable on the processor, not the PF or Compiler.
  //TODO: make tunable via setter call on PF.
  //
  // A few of these seem like runtime limits, but they get compiled into regular expressions
  // that we generate.

  def maxFieldContentLengthInBytes: Long = 1024 // Can be as large as Int.MaxValue
  def occursCountMax: Long = 1024 // Can be as large as Int.MaxValue 
  def maxSkipLength: Long = 1024 // applicable to leadingSkip and trailingSkip

  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.

  def generatedNamespacePrefixStem = "tns"

  def readerByteBufferSize: Long = 8192
}
