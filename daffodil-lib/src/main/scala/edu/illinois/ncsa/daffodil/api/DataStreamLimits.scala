package edu.illinois.ncsa.daffodil.api

trait DataStreamLimits {
  def maximumSimpleElementSizeInBytes: Long
  def maximumSimpleElementSizeInCharacters: Long
  def maximumForwardSpeculationLengthInBytes: Long
  def maximumRegexMatchLengthInCharacters: Long
  def defaultInitialRegexMatchLimitInChars: Long
}