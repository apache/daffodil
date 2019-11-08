package org.apache.daffodil.runtime1

import org.apache.daffodil.xml.QNameBase
import org.apache.daffodil.infoset.PartialNextElementResolver
import org.apache.daffodil.infoset.SeveralPossibilitiesForNextElement
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.infoset.NoNextElement
import org.apache.daffodil.infoset.OnlyOnePossibilityForNextElement
import org.apache.daffodil.api.WarnID
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.PossibleNextElements

/**
 * Part of Daffodil's Runtime 1 Streaming Unparser Support.
 *
 * When streaming in some representation of an infoset for unparsing,
 * Daffodil (in Runtime 1) must resolve what element to construct.
 * This is context specific, and given sharing of elements and groups,
 * requires a runtime stack of TermRuntimeData, each stack entry has
 * an associated PartialNextElementResolver.
 *
 * This object computes that PartialNextElementResolver based on the
 * possible elements that can follow the current term.
 */
object CompileNextElementResolver {
  import PossibleNextElements._

  /**
   * The NextElementResolver is used to determine what infoset event comes next, and "resolves" which is to say
   * determines the ElementRuntimeData for that infoset event. This can be used to construct the initial
   * infoset from a stream of XML events.
   */
  final def apply(
    context: Term,
    possibles: PossibleNextElements): PartialNextElementResolver = {
    //
    // Annoying, but scala's immutable Map is not covariant in its first argument
    // the way one would normally expect a collection to be.
    // So Map[NamedQName, ElementRuntimeData] is not a subtype of Map[QNameBase, ElementRuntimeData]
    // So we need a cast upward to Map[QNameBase,ElementRuntimeData]
    //
    val schemaFileLocation = context.schemaFileLocation
    val (sibs, isRequiredInInfoset) = possibles match {
      case Closed(sibs) => (sibs, true)
      case Open(sibs) => (sibs, false)
    }

    val resolver = sibs.size match {
      case 0 => new NoNextElement(schemaFileLocation)
      case 1 => new OnlyOnePossibilityForNextElement(
        schemaFileLocation,
        sibs.head.e.erd,
        isRequiredInInfoset)
      case _ => {
        val eltMap = sibs.map {
          sib => (sib.e.namedQName, sib.e.erd)
        }.toMap.asInstanceOf[Map[QNameBase, ElementRuntimeData]]
        val groupedByName = possibles.sibs.groupBy(_.e.namedQName.local)
        var hasNamesDifferingOnlyByNS = false
        groupedByName.foreach {
          case (_, sameNamesEB) =>
            if (sameNamesEB.length > 1) {
              context.SDW(
                WarnID.NamespaceDifferencesOnly,
                "Neighboring QNames differ only by namespaces. " +
                  "Infoset representations that do not support namespaces " +
                  "cannot differentiate between these elements and " +
                  "may fail to unparse. QNames are: %s",
                sameNamesEB.map(_.e.namedQName.toExtendedSyntax).mkString(", "))
              hasNamesDifferingOnlyByNS = true
            }
        }
        new SeveralPossibilitiesForNextElement(
          schemaFileLocation,
          eltMap,
          hasNamesDifferingOnlyByNS,
          isRequiredInInfoset)
      }
    }
    resolver
  }

}
