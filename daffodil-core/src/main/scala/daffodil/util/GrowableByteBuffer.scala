/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package daffodil.util

import java.nio.ByteBuffer
import java.nio.ByteOrder

/**
 * TODO: Do we really need this class. Why isn't a built in output stream/writer/target thingy 
 * the right thing.... our I/O needs are not that unique.
 */
class GrowableByteBuffer(private var bbuf:ByteBuffer) {
  
  private def grow(addCap:Int) {
    var newSize = bbuf.capacity + addCap
    var newBytes = new Array[Byte](newSize)
    var oldPos = bbuf.position
    bbuf.rewind
    bbuf.get(newBytes, 0, oldPos)
    bbuf = ByteBuffer.wrap(newBytes)
    bbuf.position(oldPos)
  }
  
  def order(bo:ByteOrder):GrowableByteBuffer= { bbuf.order(bo); this}
  def order = bbuf.order
  
  def position = bbuf.position
  def position(i:Int):GrowableByteBuffer = { bbuf.position(i);this}

  def limit = bbuf.limit
  def limit(i:Int):GrowableByteBuffer = {bbuf.limit(i);this}
  
  def array = bbuf.array
  def byteBuffer = bbuf
  
  def mark:GrowableByteBuffer = { 
    bbuf.mark
    this 
  }
  def reset:GrowableByteBuffer = {
    bbuf.reset
    this
  }
  
  def remaining = bbuf.remaining
  
  def checkGrow(itemSize: Int) {
    if ( (bbuf.limit - bbuf.position) < itemSize)
      if ( itemSize > bbuf.capacity)
        grow(itemSize)
      else
        grow(bbuf.capacity)
  }
  
  def put(b:Byte):GrowableByteBuffer = {
    checkGrow(1)
    bbuf.put(b)
    this
  }
  
  def put(bb:ByteBuffer):GrowableByteBuffer = {
    checkGrow(bb.capacity)
    bbuf.put(bb)
    this
  }
  
  def put(a:Array[Byte]):GrowableByteBuffer = {
    checkGrow(a.length)
    bbuf.put(a)
    this
  }
  def putInt(i:Int):GrowableByteBuffer = {
    checkGrow(4)
    bbuf.putInt(i)
    this
  }
  
  def putLong(l:Long):GrowableByteBuffer = {
    checkGrow(8)
    bbuf.putLong(l)
    this
  }
  
  def putShort(s:Short): GrowableByteBuffer = {
    checkGrow(2)
    bbuf.putShort(s)
    this
  }
  
  def putFloat(f:Float): GrowableByteBuffer = {
    checkGrow(4)
    bbuf.putFloat(f)
    this
  }
  
  def putDouble(d:Double): GrowableByteBuffer = {
    checkGrow(8)
    bbuf.putDouble(d)
    this
  }
  
  def putChar(c:Char): GrowableByteBuffer = {
    checkGrow(2)
    bbuf.putChar(c)
    this
  }
}
