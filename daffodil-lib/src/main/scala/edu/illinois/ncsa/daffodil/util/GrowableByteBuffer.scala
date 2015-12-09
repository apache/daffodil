///*
//
///* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
// *
// * Developed by: Tresys Technology, LLC
// *               http://www.tresys.com
// *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of
// * this software and associated documentation files (the "Software"), to deal with
// * the Software without restriction, including without limitation the rights to
// * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// * of the Software, and to permit persons to whom the Software is furnished to do
// * so, subject to the following conditions:
// *
// *  1. Redistributions of source code must retain the above copyright notice,
// *     this list of conditions and the following disclaimers.
// *
// *  2. Redistributions in binary form must reproduce the above copyright
// *     notice, this list of conditions and the following disclaimers in the
// *     documentation and/or other materials provided with the distribution.
// *
// *  3. Neither the names of Tresys Technology, nor the names of its contributors
// *     may be used to endorse or promote products derived from this Software
// *     without specific prior written permission.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
// * SOFTWARE.
// */
//
// * To change this template, choose Tools | Templates
// * and open the template in the editor.
// */
//
//package edu.illinois.ncsa.daffodil.util
//
//import java.nio.ByteBuffer
//import java.nio.ByteOrder
//
///**
// * TODO: Do we really need this class. Why isn't a built in output stream/writer/target thingy
// * the right thing.... our I/O needs are not that unique.
// */
//class GrowableByteBuffer(private var bbuf: ByteBuffer) {
//
//  private def grow(addCap: Int) {
//    val newSize = bbuf.capacity + addCap
//    val newBytes = new Array[Byte](newSize)
//    val oldPos = bbuf.position
//    bbuf.rewind
//    bbuf.get(newBytes, 0, oldPos)
//    bbuf = ByteBuffer.wrap(newBytes)
//    bbuf.position(oldPos)
//  }
//
//  def order(bo: ByteOrder): GrowableByteBuffer = { bbuf.order(bo); this }
//  def order = bbuf.order
//
//  def position = bbuf.position
//  def position(i: Int): GrowableByteBuffer = { bbuf.position(i); this }
//
//  def limit = bbuf.limit
//  def limit(i: Int): GrowableByteBuffer = { bbuf.limit(i); this }
//
//  def array = bbuf.array
//  def byteBuffer = bbuf
//
//  def mark: GrowableByteBuffer = {
//    bbuf.mark
//    this
//  }
//  def reset: GrowableByteBuffer = {
//    bbuf.reset
//    this
//  }
//
//  def remaining = bbuf.remaining
//
//  def checkGrow(itemSize: Int) {
//    if ((bbuf.limit - bbuf.position) < itemSize)
//      if (itemSize > bbuf.capacity)
//        grow(itemSize)
//      else
//        grow(bbuf.capacity)
//  }
//
//  def put(b: Byte): GrowableByteBuffer = {
//    checkGrow(1)
//    bbuf.put(b)
//    this
//  }
//
//  def put(bb: ByteBuffer): GrowableByteBuffer = {
//    checkGrow(bb.capacity)
//    bbuf.put(bb)
//    this
//  }
//
//  def put(a: Array[Byte]): GrowableByteBuffer = {
//    checkGrow(a.length)
//    bbuf.put(a)
//    this
//  }
//  def putInt(i: Int): GrowableByteBuffer = {
//    checkGrow(4)
//    bbuf.putInt(i)
//    this
//  }
//
//  def putLong(l: Long): GrowableByteBuffer = {
//    checkGrow(8)
//    bbuf.putLong(l)
//    this
//  }
//
//  def putShort(s: Short): GrowableByteBuffer = {
//    checkGrow(2)
//    bbuf.putShort(s)
//    this
//  }
//
//  def putFloat(f: Float): GrowableByteBuffer = {
//    checkGrow(4)
//    bbuf.putFloat(f)
//    this
//  }
//
//  def putDouble(d: Double): GrowableByteBuffer = {
//    checkGrow(8)
//    bbuf.putDouble(d)
//    this
//  }
//
//  def putChar(c: Char): GrowableByteBuffer = {
//    checkGrow(2)
//    bbuf.putChar(c)
//    this
//  }
//}
