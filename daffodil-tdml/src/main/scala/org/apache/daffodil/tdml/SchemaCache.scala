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

package org.apache.daffodil.tdml

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.equality._; object ENoWarnTDML { EqualitySuppressUnusedImportWarning() }
import scala.collection.mutable
import org.apache.daffodil.exceptions.ThinThrowable
import org.apache.daffodil.api.URISchemaSource
import org.apache.daffodil.api.Diagnostic
import org.apache.daffodil.exceptions.Assert

/**
 * Cache that saves the compiled data processor objects (memory structures, not files
 * of the serialized representation)
 */
object SchemaDataProcessorCache extends SchemaCache[(Seq[Diagnostic], DFDL.DataProcessor), Seq[Diagnostic]]

/**
 * A cache of things associated with URISchemaSources.
 *
 * Recompiles if URI is a file and the modification date has changed.
 *
 * Thread safe so that multiple threads can share a compiled schema for the same URI.
 * One thread will wait while the other compiles it.
 */

class SchemaCache[CachedType, DiagnosticType] {

  private class Cache
    extends mutable.HashMap[(URISchemaSource, Boolean), (URISchemaSource, CachedType)] {

    override def getOrElseUpdate(key: (URISchemaSource, Boolean), body: => (URISchemaSource, CachedType)) = synchronized {
      super.getOrElseUpdate(key, body)
    }

  }

  object Types {
    type CompileResult = Either[DiagnosticType, CachedType]
    type CompileFailure = Left[DiagnosticType, CachedType]
  }
  import Types._

  private val compiledSchemaCache = new Cache

  def resetCache {
    compiledSchemaCache.clear()
  }

  def numCacheEntries = compiledSchemaCache.keysIterator.toSeq.length

  private class SchemaCompileFailed(val dp: CompileResult) extends Throwable with ThinThrowable

  /**
   * Uses thread-safe operation to populate cache that is potentially shared across
   * threads that are running tests in parallel. We want only one compilation of
   * a schema for a given URI schema source, because schema compilation can be quite
   * expensive.
   *
   * If compilation fails with diagnostics, the cache is not populated.
   *
   * If the same URI is used, and it identifies a file, then if the modification time
   * is such that the file is newer then when last compiled, the newer file will
   * be compiled and cached.
   */
  def compileAndCache(uss: URISchemaSource, useSerializedProcessor: Boolean)(doCompileByName: => CompileResult): CompileResult = {
    lazy val doCompile = doCompileByName // exactly once
    val key = (uss, useSerializedProcessor)
    synchronized {
      // if the file is newer then when last compiled, drop from the cache.
      val optExistingEntry = compiledSchemaCache.get(key)
      if (optExistingEntry.isDefined) {
        val (originalUSS, _) = optExistingEntry.get // odd - when I did this using a match-case, I got a match error....
        if (uss.isNewerThan(originalUSS)) compiledSchemaCache.remove(key)
      }
    }
    val compResult: CompileResult = {
      try {
        val (_, dataProc) = compiledSchemaCache.getOrElseUpdate(key, {
          val dp: CompileResult = doCompile
          if (dp.isRight) {
            // populate cache with successful compile result
            (uss, dp.right.get)
          } else {
            // prevent populating the cache
            throw new SchemaCompileFailed(dp)
          }
        })
        Right(dataProc)
      } catch {
        case f: SchemaCompileFailed => {
          Assert.invariant(f.dp.isInstanceOf[CompileFailure])
          f.dp
        }
      }
    }
    compResult
  }
}
