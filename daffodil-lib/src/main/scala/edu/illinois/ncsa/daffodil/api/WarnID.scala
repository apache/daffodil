package edu.illinois.ncsa.daffodil.api

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.{ Enum => PropsEnum }

  sealed trait WarnID extends WarnID.Value

  object WarnID extends PropsEnum[WarnID] {
    case object All extends WarnID; forceConstruction(All)
    case object MultipleChoiceBranches extends WarnID; forceConstruction(MultipleChoiceBranches)
    case object EscapeSchemeRefUndefined extends WarnID; forceConstruction(EscapeSchemeRefUndefined)

    def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Call find(name) method instead.")

    def find(name: String): Option[WarnID] = optionStringToEnum("warning identifier", name)
  }
