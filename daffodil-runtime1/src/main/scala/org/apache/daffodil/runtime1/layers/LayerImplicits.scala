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
package org.apache.daffodil.runtime1.layers

import java.util.Optional
import scala.compat.java8.OptionConverters._

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.Evaluatable

/**
 * Implicits that let you convert from Scala `Option` to Java Optional` or vice versa
 * as well as `Option[Option[T]]` to Java Optional<Optional<T>>` or vice versa,
 * by just calling `x.asJava` or `x.asScala`
 *
 * Implicits that let you convert from Scala `Maybe` to Java `Optional` by just
 * calling `maybe.toOptional`
 *
 * And a few other utilities.
 */
object LayerImplicits {

  /**
   * Used because we often care about whether properties are defined, and if defined
   * whether they are constant or not.
   *
   * This is represented as nested Options in Scala. So we provide this Java-friendly
   * conversion to Java's Optional type.
   *
   * asJava method converts a Scala `Option[Option[T]]` to Java `Optional<Optional<T>>`
   * @param underlying the `Option[Option[T]]`
   * @tparam T generic type
   */
  implicit class OptionsToOptionals[T](val underlying: Option[Option[T]]) extends AnyVal {

    /**
     * Use on a Scala `Option[Option[T]]`
     * @return a Java `Optional<Optional<T>>` object
     */
    def asJava: Optional[Optional[T]] = underlying.map { _.asJava }.asJava
  }

  /**
   * Convenience for users who want to call the Java-oriented APIs from Scala code
   * @param underlying the `Optional<Optional<T>>` Java object
   * @tparam T generic type
   */
  implicit class OptionalsToOptions[T](val underlying: Optional[Optional[T]]) extends AnyVal {

    /**
     * Use on a Java `Optional<Optional<T>>` object
     * @return a Scala `Option[Option[T]]` object
     */
    def asOption: Option[Option[T]] = underlying.asScala.map { _.asScala }
  }

  /**
   * Utility for converting `Maybe[S]` to `Optional[T]` using an `S` to `T` mapping function.
   * @param s2t an `S` to `T` function, such as the apply method of a `Map[S, T]`
   * @param in a `Maybe[S]`
   * @tparam S type of the argument `Maybe[S]`
   * @tparam T type of the returned `Optional[T]`
   * @return an `Optional[T]`
   */
  def maybeSToOptionalT[S <: AnyRef, T](s2t: S => T, in: Maybe[S]): Optional[T] =
    if (in.isEmpty) Optional.empty() else Optional.of(s2t(in.get))

  /**
   * Converts a maybe evaluatable that could be a constant or expression into
   * an option option.
   *
   * The outer option carries the Maybe information. It
   * tells you if the maybe evaluatable was defined or not.
   * The inner option is only present if the outer was defined, and it contains the
   * constant value if the evaluatable was a constant, and is undefined if the
   * evaluatable was a runtime-valued expression.
   * @param mev a Maybe Evaluatable T
   * @tparam T the type of the Evaluatable expression (or constant)
   * @return a nested Option to indicate defined/not and constant/non-constant.
   */
  def maybeEVConstToOptOpt[T <: AnyRef](mev: Maybe[_ <: Evaluatable[T]]): Option[Option[T]] =
    if (mev.isEmpty) None else Some(mev.get.optConstant)

  /**
   * Converts a `Maybe[Evaluatable[T]}` that could be a constant or expression into
   * a Java `Optional<Optional<T>>``to allow ispection and use at schema compilation time,
   * with convenience, from Java code.
   *
   * The outer option carries the Maybe information. It
   * tells you if the maybe evaluatable was defined or not.
   * The inner option is only present if the outer was defined, and it contains the
   * constant value if the evaluatable was a constant, and is empty if the
   * evaluatable was a runtime-valued expression.
   * @param mev a `Maybe[Evaluatable[T]]``
   * @tparam T the type of the Evaluatable expression (or constant). Must be an `AnyRef``
   * @return a nested Java Optional to indicate defined/not and constant/non-constant.
   */
  def maybeEVConstToOptionalOptional[T <: AnyRef](
    mev: Maybe[_ <: Evaluatable[T]],
  ): Optional[Optional[T]] =
    if (mev.isEmpty) Optional.empty() else Optional.of(Maybe.toOptional(mev.get.maybeConstant))
}
