/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.io

import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.exceptions.Assert

/**
 * The InputSource class is really just a mechanism to provide bytes an
 * InputSourceDataInputStream, which does the heavily lift about converter
 * bits/bytes to numbers and characters. This class does not need to know
 * anything about bits, it is purely byte centric. One core difference from
 * this vs an InputStream is that is must have the capability to backtrack to
 * arbitrary points in the InputStreams history. To aide in this, methods are
 * called to let the InputSource know which byte positions might we might need
 * to backtrack to, which can allow it to free data that know longer is needed.
 * One can almost thing of things as an InputStream that supports multiple
 * marks with random access.
 */
abstract class InputSource {

  /**
   * Determine whether the underlying data has the specified number of bytes
   * available starting at the current byte position. This function must block
   * until either nBytes are known to be available or end-of-file is reached.
   * This does not advance the current position.
   *
   * @param nBytes the number of bytes to determine if are available
   * @return true if nBytes are available, false otherwise
   */
  def areBytesAvailable(nBytes: Long): Boolean

  /**
   * Return the number of currently available bytes.
   *
   * This should not be used to determine the length of the data, as more bytes
   * may become available in the future. This should really only be used for
   * debug purposes.
   */
  def bytesAvailable(): Long

  /**
   * Return a single byte at the current byte position with a value in the
   * range of 0 to 255.
   *
   * Increments the current byte position if successful.
   *
   * @return the byte at the current byte position if it exists, or -1 if EOF is reached.
   */
  def get(): Int

  /**
   * Return a byte array with data from the current byte position.
   *
   * Stores the next len bytes of data in dest starting at index off. In len
   * bytes are not available or len bytes cannot fit in the dest array starting
   * at the given offset, the dest array is not modified and false is returned.
   *
   * @return true if len bytes are available and written to the dest array,
   *         false otherwise
   */
  def get(dest: Array[Byte], off: Int, len: Int): Boolean

  /**
   * Get the current byte position, using zero-based indexing
   *
   * @return the current byte position
   */
  def position(): Long

  /**
   * Set the current byte position, using zero-based indexing
   *
   * bytPos0b cannot be greater than the most recent read data. In other words,
   * this can only be used to move backwards in data.
   *
   * @param bytePos0b the new current byte position
   */
  def position(bytePos0b: Long): Unit

  /**
   * Set the specified byte position as a location that that one may want to
   * call setPosition in the future. This is essentially setting a mark in the
   * data that can be reset back to later. Implementations are allowed to free
   * any bytes before a locked byte position. Any bytes after a locked position
   * cannot be freed until that lock is release.
   *
   * Note that this "lock" has nothing to do with synchronization, but behaves
   * more like marks that must be accessable until released.
   *
   * @param bytePos0b the byte position to lock
   */
  def lockPosition(bytePos0b: Long): Unit

  /**
   * Release a previously locked byte position, allowing the implementation to
   * free any unlocked bytes.
   *
   * @param bytePos0b the byte position to release
   */
  def releasePosition(bytePos0b: Long): Unit

  /**
   * Alerts the implementation to attempt to free data that is no longer used,
   * if possible. If possible, this should free any unlocked bytes.
   */
  def compact(): Unit

  private var _debugging = false

  final def areDebugging: Boolean = _debugging

  final def setDebugging(setting: Boolean): Unit = _debugging = setting
}

/**
 * Implements the InputSource interface, reading data from a generic
 * java.io.InputStream and storing the data in buckets of a defined size.
 * Buckets are freed when no "locks" exist inside the bucket to minimize memory
 * usage. Note that "locks" in this sense are the InputSource locks on
 * bytePosition and are not about syncrhonization. This more of a reference
 * count, counting buckets to determine which buckets are no longer needed and
 * can be freed when the reference count goes to zero.
 *
 * @param inputStream the java.io.Inputstream to read data from
 * @param bucketSize the size of each individual bucket
 */ 
class BucketingInputSource(inputStream: java.io.InputStream, bucketSize: Int = 1 << 13)
  extends InputSource {

  private class Bucket {
    var refCount = 0
    val bytes = new Array[Byte](bucketSize)
  }

  /**
   * Array of buckets
   */
  private val buckets = {
    val b = new ArrayBuffer[Bucket]()
    // initialize with an empty bucket
    b += new Bucket()
    b
  }

  /**
   * Used to keep track of the oldest bucket stored in the bucket array. When a
   * bucket is released, this number will be incremented, allowing us to
   * quickly know which buckets have something in them. All indices in the
   * buckets ArrayBuffer less than this index should have a value of null
   */
  private var oldestBucketIndex = 0

  /**
   * Stores the current byte position. When a call to get is made, the
   * byte(s) at this position is retrieved and this value is incremented.
   */
  private var curBytePosition0b: Long = 0

  /**
   * Contains the total number of bytes that have been read and stored in a
   * bucket.
   */
  private var totalBytesBucketed: Long = 0

  /**
   * Stores the byte position represented by the 0th index of the 0th bucket.
   * This implementation will periodically throw away old unneeded buckets and
   * move the remaining buckets to the front of the buckets ArrayBuffer. This
   * value is used to determine which bucket to grab data from, essentially
   * acting as an offset of curBytePosition0b.
   */
  private var headBucketBytePosition0b: Long = 0

  /**
   * Stores the number of bytes that have been filled in the last bucket. A
   * bucket is allocated to bucketSize, but not all may be used if the source
   * InputStream did not give us enough or ran out of data. This says how much
   * data was in the last bucket in such a case.
   */
  private var bytesFilledInLastBucket: Int = 0

  /**
   * Adds new buckets to the buckets array until either we run out of data or
   * we fill up to byteIndex bytes in the bucketIndex. This modifies
   * bytesFilledInLastBucket, which may be less than bucketSize if the last
   * bucket was not completely filled.
   *
   * @return false if EOF was hit before filling in the necessary bytes. true otherwise.
   */
  private def fillBucketsToIndex(goalBucketIndex: Long, bytesNeededInBucket: Long): Boolean = {
    var lastBucketIndex = buckets.length - 1
    var hasMoreData = true
    var needsMoreData = goalBucketIndex > lastBucketIndex || (goalBucketIndex == lastBucketIndex && bytesNeededInBucket > bytesFilledInLastBucket)

    while (needsMoreData && hasMoreData) {
      // Try to fill the rest of this bucket, regardless of how many bytes are
      // actually needed
      val emptyBytesInLastBucket = bucketSize - bytesFilledInLastBucket

      // Try to read enough bytes to fill the rest of this bucket. Note that
      // the .read() function could hit EOF (returns -1) or could return
      // anywhere from zero to emptyBytesInLastBucket bytes
      val bytesRead = inputStream.read(buckets(lastBucketIndex).bytes, bytesFilledInLastBucket, emptyBytesInLastBucket)

      if (bytesRead == -1) {
        // Needed more data but hit EOF, break out with error
        hasMoreData = false
      } else {
        totalBytesBucketed += bytesRead
        bytesFilledInLastBucket += bytesRead

        if (bytesFilledInLastBucket == bucketSize) {
          // We completely filled in this bucket, let's create a new bucket for
          // the next time this inner loop is hit (might be immediately if we
          // didn't reach the target bucket, or might be the next time this
          // function is called if we did). Increment index so that if we do
          // continue this loop immediately, we're filling in the new bucket.
          buckets += new Bucket()
          bytesFilledInLastBucket = 0
          lastBucketIndex += 1
        }

        if ((lastBucketIndex == goalBucketIndex && bytesNeededInBucket <= bytesFilledInLastBucket) || lastBucketIndex > goalBucketIndex) {
          // Filled data at least to the target byte in the target bucket
          // We are done
          needsMoreData = false
        } else {
          // Either we didn't get to the target bucket, or we did but we didn't
          // fill in enough bytes in the target bucket. In either case, we need
          // more bytes in this bucket, so loop around again using the new
          // bucket index and bytesFilledInLastBucket, and try to fill it up.
          // Nothing needs to be done here.
        }
      }
    }

    // this function succeeds if we got to a point where we no longer need
    // anymore data (i.e. we filled in at least needed bytes in the specified
    // goalBucketIndex).
    !needsMoreData
  }

  @inline
  final private def bytePositionToIndicies(bytePos0b: Long): (Long, Long) = {
    val offsetBytePosition0b = bytePos0b - headBucketBytePosition0b
    val bucketIndex = offsetBytePosition0b / bucketSize
    val byteIndex = offsetBytePosition0b % bucketSize
    (bucketIndex, byteIndex)
  }

  /**
   * Determines whether the input stream has nBytes more bytes available
   * starting at the current byte position. Read bytes are cached in buckets.
   * Blocks until either nBytes are known to be available or EOF is reached.
   * Does not advance the current byte position.
   */
  def areBytesAvailable(nBytes: Long): Boolean = {
    val finalBytePosition0b = curBytePosition0b + nBytes
    if (finalBytePosition0b <= totalBytesBucketed) {
      true
    } else {
      val (bucketIndex, byteIndex) = bytePositionToIndicies(finalBytePosition0b)
      Assert.invariant(bucketIndex >= oldestBucketIndex)
      val filled = fillBucketsToIndex(bucketIndex, byteIndex)
      filled
    }
  }

  /**
   * Calculate how many bytes are currently buffered starting from the current
   * position
   */ 
  def bytesAvailable(): Long = {
    var available = 0L
    val (curBucketIndex, curByteIndex) = bytePositionToIndicies(curBytePosition0b)
    
    var i = curBucketIndex
    while (i < buckets.length) {
      val startByteIndex = if (i == curBucketIndex) curByteIndex else 0
      val endByteIndex = if (i == buckets.length - 1) bytesFilledInLastBucket else bucketSize
      val bytesAvailableInBucket = endByteIndex - startByteIndex
      available += bytesAvailableInBucket
      i += 1
    }

    available
  }

  /**
   * Return a single byte at the current byte position. Increments
   * curBytePosition0b if successful. The Byte is return as an integer in the
   * range 0 to 255. Returns -1 if EOF is reached.
   */
  def get(): Int = {
    val hasByte = areBytesAvailable(1)
    if (!hasByte) {
      -1
    } else {
      val (bucketIndex, byteIndex) = bytePositionToIndicies(curBytePosition0b)
      val byte = buckets(bucketIndex.toInt).bytes(byteIndex.toInt)
      curBytePosition0b += 1
      byte & 0xFF
    }
  }

  /**
   * Return a byte array with data from the current byte position. Stores the
   * next len bytes of data in dest starting at index off. Returns true if len
   * bytes are available, false otherwise and writes nothing to dest.
   */
  def get(dest: Array[Byte], off: Int, len: Int): Boolean = {
    Assert.invariant(dest.length - off >= len)

    val hasBytes = areBytesAvailable(len)
    if (!hasBytes) {
      false
    } else {
      var (bucketIndex, byteIndex) = bytePositionToIndicies(curBytePosition0b)
      var bytesStillToGet = len
      var destOffset = off
      while (bytesStillToGet > 0) {
        val bytesToGetFromCurrentBucket = Math.min(bucketSize - byteIndex, bytesStillToGet).toInt

        Array.copy(buckets(bucketIndex.toInt).bytes, byteIndex.toInt, dest, destOffset, bytesToGetFromCurrentBucket)

        destOffset += bytesToGetFromCurrentBucket
        bytesStillToGet -= bytesToGetFromCurrentBucket
        // next read will read from the next bucket
        bucketIndex += 1
        byteIndex = 0
      }
      curBytePosition0b += len
      true
    }
  }

  def position(): Long = curBytePosition0b

  def position(bytePos0b: Long): Unit = {
    val (bucketIndex, _) = bytePositionToIndicies(bytePos0b)
    Assert.invariant(bucketIndex >= oldestBucketIndex && bucketIndex < buckets.length)
    curBytePosition0b = bytePos0b
  }

  def lockPosition(bytePos0b: Long): Unit = {
    val (bucketIndex, _) = bytePositionToIndicies(bytePos0b)
    Assert.invariant(bucketIndex >= oldestBucketIndex && bucketIndex < buckets.length)
    buckets(bucketIndex.toInt).refCount += 1
  }

  def releasePosition(bytePos0b: Long): Unit = {
    val (bucketIndex, _) = bytePositionToIndicies(bytePos0b)
    Assert.invariant(bucketIndex >= oldestBucketIndex && bucketIndex < buckets.length)
    buckets(bucketIndex.toInt).refCount -= 1 

    if (bucketIndex == oldestBucketIndex && buckets(bucketIndex.toInt).refCount == 0) {
      // We just freed the last reference to the oldest bucket. So try to
      // release as many buckets as possible. Note that this might still not
      // release anything if the oldest bucket contains the current byte
      // position.
      releaseBuckets()
    }
  }

  private def releaseBuckets(): Unit = {
    // we only want to release buckets when not debugging so that data is never
    // cleaned up and is always available
    if (!areDebugging) {
      // Look for old buckets that are no longer referenced by a mark (i.e.
      // refCount is zero), set them to null so they are garbage collected. Make
      // sure not to not remove whatever bucket holds the current byte position,
      // even if there are no marks--we need to still read from that bucket.
      val (curBucketIndex, _) = bytePositionToIndicies(curBytePosition0b)
      while (oldestBucketIndex < curBucketIndex && buckets(oldestBucketIndex).refCount == 0) {
        buckets(oldestBucketIndex) = null
        oldestBucketIndex += 1 
      }
    }
  }

  /**
   * This should be called when at the end of a parse when all marks have been
   * released, or just called periodically to ensure the size of the
   * buckets arraybuffer does not grow too big. This will move all existing
   * buckets to the front of the buckets ArrayBuffer and update offsets
   * accordingly. This way, there are not a bunch of null empty buckets at the
   * front of the buckets array taking up space. 
   */
  def compact(): Unit = {
    releaseBuckets()
    buckets.remove(0, oldestBucketIndex.toInt)
    val bytesRemoved = oldestBucketIndex * bucketSize
    headBucketBytePosition0b += bytesRemoved
    oldestBucketIndex = 0
  }
}



/**
 * Wraps a java.nio.ByteBuffer in a InputSource
 *
 * When an instance of this class is created, it creates a readOnly copy of the
 * ByteBuffer. The current position of the ByteBuffer is considered index 0.
 * For example, if thed passed in ByteBuffer had position 2, calling
 * setPosition(0) would reset the byteBuffer back to position 2. The limit of
 * the ByteBuffer is considered the end of data.
 */

class ByteBufferInputSource(byteBuffer: ByteBuffer)
  extends InputSource {
  
  private val bb = byteBuffer.asReadOnlyBuffer

  private val positionOffset = bb.position

  def areBytesAvailable(nBytes: Long): Boolean = {
    bb.remaining >= nBytes
  }

  def bytesAvailable(): Long = {
    bb.remaining
  }

  def get(): Int = {
    if (!areBytesAvailable(1)) {
      -1
    } else {
      bb.get() & 0xFF
    }
  }

  def get(dest: Array[Byte], off: Int, len: Int): Boolean = {
    Assert.invariant(dest.length - off >= len)

    if (!areBytesAvailable(len)) {
      false
    } else {
      bb.get(dest, off, len)
      true
    }
  }

  def position(): Long = bb.position - positionOffset

  def position(bytePos0b: Long): Unit = bb.position((bytePos0b + positionOffset).toInt)

  def lockPosition(bytePos0b: Long): Unit = {} // noop

  def releasePosition(bytePos0b: Long): Unit = {} // noop

  def compact(): Unit = { // noop
    // Note that ByteBuffer.compact() does not do the same thing as this
    // method. The point of this compact() method is to free up storage of
    // bytes that no longer need to be used when we know we will never need to
    // backtrack to them. However, the ByteBuffer.compact() method doesn't free
    // allocated space, it just copies unread bytes to the beginning of the
    // ByteBuffer so that more space is available for calls to put(). To meet
    // the intention of compact() to free up space, we would need to allocate a
    // smaller ByteBuffer and copy all unread bytes into that, and let the old one
    // be garbage collected. For a large ByteBuffer with lots of unread data,
    // that would be expensive. Furthermore, the original ByteBuffer would
    // mostly likely still exist in memory created by the original caller, so
    // this would just be using up more memory. So let's just do nothing here.
    // Similarly, lock/releasePosition methods do not need to do anything,
    // since those are only used to prevent memory segments from being
    // compacted.
  }
}
