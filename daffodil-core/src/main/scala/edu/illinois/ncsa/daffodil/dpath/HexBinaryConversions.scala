package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions.Assert

object HexBinaryConversions {

  def toByteArray(b: Byte): Array[Byte] = Array(b)
  def toByteArray(s: Short): Array[Byte] = {
    val upper = ((s >> 8) & 0x00FF).toByte
    val lower = (s & 0x00FF).toByte
    Array[Byte](upper, lower)
  }
  def toByteArray(i: Integer): Array[Byte] = {
    val byte0 = ((i >> 24) & 0x000000FF).toByte
    val byte1 = ((i >> 16) & 0x000000FF).toByte
    val byte2 = ((i >> 8) & 0x000000FF).toByte
    val byte3 = (i & 0x000000FF).toByte

    Array[Byte](byte0, byte1, byte2, byte3)
  }
  def toByteArray(l: Long): Array[Byte] = {
    val i0: Integer = ((l >> 32) & 0xFFFFFFFF).toInt
    val i1: Integer = (l & 0xFFFFFFFF).toInt
    val arr0 = toByteArray(i0)
    val arr1 = toByteArray(i1)

    arr0 ++ arr1
  }

}