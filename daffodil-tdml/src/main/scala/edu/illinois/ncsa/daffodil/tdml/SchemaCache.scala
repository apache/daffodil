package edu.illinois.ncsa.daffodil.tdml

import java.io.File
import edu.illinois.ncsa.daffodil.api.DFDL
import edu.illinois.ncsa.daffodil.equality._; object ENoWarnTDML { EqualitySuppressUnusedImportWarning() }
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.processors.HasSetDebugger
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.api.URISchemaSource
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Cache that saves the compiled data processor objects (memory structures, not files
 * of the serialized representation)
 */
object SchemaDataProcessorCache extends SchemaCache[DFDL.DataProcessor, Seq[Diagnostic]]

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
    extends mutable.HashMap[URISchemaSource, (URISchemaSource, CachedType)] {

    override def getOrElseUpdate(key: URISchemaSource, body: => (URISchemaSource, CachedType)) = synchronized {
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
  def compileAndCache(uss: URISchemaSource)(doCompileByName: => CompileResult): CompileResult = {
    lazy val doCompile = doCompileByName // exactly once
    synchronized {
      // if the file is newer then when last compiled, drop from the cache.
      val optExistingEntry = compiledSchemaCache.get(uss)
      if (optExistingEntry.isDefined) {
        val (originalUSS, _) = optExistingEntry.get // odd - when I did this using a match-case, I got a match error....
        if (uss.isNewerThan(originalUSS)) compiledSchemaCache.remove(originalUSS)
      }
    }
    val compResult: CompileResult = {
      try {
        val (_, dataProc) = compiledSchemaCache.getOrElseUpdate(uss, {
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
