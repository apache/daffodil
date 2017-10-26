package org.apache.daffodil.api

import org.apache.daffodil.util.Enum

  /**
   * Specified how unqualified path steps are resolved.
   *
   * NoNamespace:
   *  Unqualified path steps remain unqualified and will only match elements in
   *  NoNamespace. A prefix must be provided to match namespaced elements.
   *
   * DefaultNamespace:
   *  Unqualified path steps will always use the default namespace. If a default
   *  namespace is defined, it is not possible to match a NoNamespace element
   *  with this policy. Because of this, this may not work well with
   *  elementFormDefault="unqualified".
   *
   * PreferDefaultNamespace
   *  Attempt to use the default namespace to resolve a step. If that fails to
   *  match an element, then try to resolve using NoNamespace.
   */
  object UnqualifiedPathStepPolicy extends Enum {
    abstract sealed trait Type extends EnumValueType
    case object NoNamespace extends Type
    case object DefaultNamespace extends Type
    case object PreferDefaultNamespace extends Type
  }