package edu.illinois.ncsa.daffodil.dsom
//
// OLD Algorithm for transitive closure of include/import graph.
// Keeping for a while. Might want this somewhere else
//
//  lazy val alreadySeenStart: IIMap = alreadySeenStart_.value
//  private val alreadySeenStart_ = LV[IIMap]('alreadySeenStart) {
//    val seq = initialSchemaDocuments.map { sd =>
//      {
//        val oneMap = ((sd.targetNamespace, sd.fileName) -> sd)
//        oneMap
//      }
//    }
//    seq.toMap.asInstanceOf[IIMap]
//  }
//
//  /**
//   * Good example of functional programming here.
//   *
//   * In Java, or in imperative programming style, one would
//   * write this by allocating a mutable lookup table/map, and
//   * then you would traverse the structure, mutating this state
//   * object, checking against it for each new schema file
//   * before loading that schema file.
//   *
//   * Instead this is done in the classic functional programming
//   * style using foldLeft and recursion.
//   *
//   * What foldLeft does is takes a starting value (a immutable map
//   * from a pair of (NS, URL) -> SchemaDocument). The second argument
//   * is the folding function which takes 2 arguments.
//   * For foldLeft it is the left argument which is 'circulated',
//   * that is, whatever the folding function produces for one call, is fed
//   * back around as the left argument for the next call on the next
//   * element.
//   *
//   * In the below, the thing being fed around is this ever-growing
//   * immutable map of the unique schema file instances.
//   *
//   * Note that a schema file that has no target namespace can be included
//   * by other schemas, and it will take on the namespace of the
//   * schema into-which it was included. That's why the key to the
//   * table is both the namespace, and the filename. Because the same
//   * schema file name might have to be read a bunch of times to create
//   * instances in the various namespaces. This is called 'chameleon' namespaces
//   * in XML Schema parlance.
//   *
//   * TODO: since the namespace could be stored outside of the schema
//   * document, we *should* be able to be clever and share the SchemaDocument
//   * structure. For now we're not bothering with this, but it might help
//   * for very large schemas if they use include and this chameleon
//   * namespace stuff a lot.
//   *
//   * This right here is the hairy transitive closure algorithm that
//   * walks down all schema docs, and all include and imports, and
//   * follows them to more schema docs, all the while accumulating a
//   * map of what it has already seen so as not to load the same
//   * schema twice for the same namespace.
//   */
//  lazy val allSeenDocs = allSeenDocs_.value
//  private val allSeenDocs_ = LV('allSeenDocs) {
//
//    val transitiveClosure = {
//      // we fold up the initial schema documents from the user
//      // with the initial 'left' value (type IIMap for 'include and import map'
//      // ie, a map containing those same
//      // schema documents keyed from their namespaces & filenames. 
//      initialSchemaDocuments.foldLeft(alreadySeenStart)(unseenSchemaDocs _)
//    }
//    transitiveClosure
//  }
//
//  /**
//   * the outer loop fold function. folds up the list of schema documents,
//   * accumulating the unseen (so far) schema documents into the result.
//   */
//  def unseenSchemaDocs(seen: IIMap, sd: SchemaDocument): IIMap = {
//    val iis = sd.includesAndImports
//    val resultSeen = iis.foldLeft(seen)(foldOneII _)
//    resultSeen
//  }
//
//  /**
//   * The inner loop fold function. Folds up a list of include or import statements.
//   * coming from one schema document.
//   *
//   * This is where we check to see if we can skip the include/import
//   * beacuse we already have the file.
//   *
//   * And the magic here is that for each new schema document that
//   * we actually do load, recursively we fold up all unseen schema docs
//   * reachable from it.
//   *
//   * And this idiom threads that seen-map through everything sequentially
//   * so that there are never duplicates to remove.
//   *
//   * Also, this handles circular import namespace relationships. Those
//   * are common as a large schema may simply have files that use each other's
//   * definitions. It might not be a good well-layered design, but XML
//   * schema (and DFDL schema) certainly allows it.
//   */
//  def foldOneII(seen: IIMap, ii: IIBase) = {
//    seen.get((ii.targetNamespace, ii.resolvedLocation)) match {
//      case None => {
//        val res =
//          try {
//            // try catch so that we'll keep going even if errors 
//            // are occurring so we accumulate more errors.
//            val newSd = ii.iiSchemaDocument
//            val mapTuple = ((newSd.targetNamespace, newSd.fileName), newSd)
//            val resultSeen1 = seen + mapTuple // add this document as seen
//            // recursively add all unseen schema docs reachable from it
//            // to the set of things we've seen
//            val resultSeen2 = unseenSchemaDocs(resultSeen1, newSd)
//            resultSeen2
//          } catch {
//            case eah: ErrorAlreadyHandled =>
//              seen
//          }
//        res
//      }
//      case Some(_) => seen
//    }
//  }
