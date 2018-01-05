package org.apache.daffodil.api

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.{ Enum => PropsEnum }

  sealed trait WarnID extends WarnID.Value

  object WarnID extends PropsEnum[WarnID] {
    case object All extends WarnID; forceConstruction(All)
    case object MultipleChoiceBranches extends WarnID; forceConstruction(MultipleChoiceBranches)
    case object EscapeSchemeRefUndefined extends WarnID; forceConstruction(EscapeSchemeRefUndefined)

    def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Call find(name) method instead.")

    def find(name: String): Option[WarnID] = optionStringToEnum("warning identifier", name)
  }
