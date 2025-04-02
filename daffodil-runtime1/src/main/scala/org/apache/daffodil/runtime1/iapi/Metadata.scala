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
package org.apache.daffodil.runtime1.iapi

import java.lang.{ Long => JLong }
import scala.xml.NamespaceBinding

/**
 * This is the supportable API for access to the RuntimeData structures
 * which provide access to static information about a given compiled schema
 * metadata object. 
 *
 * This is used to interface other data processing fabrics to Daffodil
 * data and metadata, by mapping to/from these metadata objects.
 */
trait Metadata {

  /**
   * Provides the file context of a metadata component. This refers to the specific
   * DFDL schema file where the corresponding DFDL schema text resides corresponding
   * to this metadata object.
   *
   * This is for use in diagnostic messaging. It is not the actual file URI, because
   * those may contain personal-identifying information about the person/acccount and
   * system that compiled the schema. It will provide enough content about the file URI that
   * a user will be able to identify which file, but some prefix of the path
   * components trimmed to make it of a manageable length.
   *
   * Used along with `schemaFileLineNumber`
   * and `schemaFileLineColumnNumber`
   * this can give a precise location in the DFDL schema file.
   * @return a string containing the file information, or null if unknown.
   */
  def schemaFileInfo: String

  /**
   * Provides the line number to go with `schemaFileInfo`.
   * @return the line number as a string, or null if unknown.
   */
  def schemaFileLineNumber: JLong

  /**
   * Provides the column number within the text line, to go with `schemaFileLineNumber`.
   * @return the column number within the text line, as a string, or null if unknown.
   */
  def schemaFileLineColumnNumber: JLong

  /**
   * The name of the schema component, in a form suitable for diagnostic messages.
   * Unnamed components like sequence or choice groups have a diagnosticDebugName, despite not having
   * any actual name.
   * @return the name of the component, suitable for use in diagnostic messages.
   */
  def diagnosticDebugName: String
}

/*
 * Provides metadata access that is common to all Terms, which include
 * Elements of simple or complex type, as well as the Sequence and Choice groups.
 */
trait TermMetadata extends Metadata {
  // nothing here
}

/**
 * Common metadata access for all elements, of simple or complex type.
 */
trait ElementMetadata extends TermMetadata {

  /**
   * @return the name of this element. In the case of a global/qualified name, this is only the local
   *         part of the QName.
   */
  def name: String

  /**
   * @return the namespace URI as a string, or null if no namespace.
   */
  def namespace: String

  /**
   * @return the namespace bindings needed to construct an XML element from a Daffodil infoset
   *         element of simple or complex type.
   */
  def minimizedScope: NamespaceBinding

  /**
   * @return the namespace prefix part of the XML QName of this component, or null if there
   *         is no prefix defined or no namespace.
   */
  def prefix: String

  /**
   *  @return true if two or more occurrences are possible.
   *          Note that having only 0 or 1 occurrence is not considered an array,
   *          but rather an optional element.
   */
  def isArray: Boolean

  /**
   * @return true if only 0 or 1 occurrence are possible.
   */
  def isOptional: Boolean

  /**
   * @return the QName string for this element.
   */
  def toQName: String

  /**
   * @return true if the element is declared to be nillable.
   */
  def isNillable: Boolean

  /**
   * Provides access to the runtime properties. This is an extended collection of
   * name-value pairs which are associated with a schema component.
   *
   * Runtime properties are intended to use for new ad-hoc property extensions to
   * DFDL. These name-value pairs are visible to infoset outputters as well.
   *
   * @return a java-compatible map of name-value pairs.
   */
  def runtimeProperties: java.util.Map[String, String]

}

/**
 * Access to metadata values exclusive to elements of complex type.
 */
trait ComplexElementMetadata extends ElementMetadata {
  // no specific methods
}

/**
 * Access to metadata values exclusive to elements of simple type.
 */
trait SimpleElementMetadata extends ElementMetadata {

  /**
   * Primitive Type enum usable from Java
   * @return
   */
  def dfdlType: DFDLPrimType
}

/**
 * Access to metadata values shared by both sequences and choices
 * which are known collectively as Model Groups.
 */
trait ModelGroupMetadata extends TermMetadata {}

/**
 * Access to metadata values specific to sequences
 */
trait SequenceMetadata extends ModelGroupMetadata {}

/**
 * Access to metadata values specific to choices
 */
trait ChoiceMetadata extends ModelGroupMetadata {}

/**
 * Base class used by clients who want to walk the runtime1 metadata information.
 *
 * The runtime1 [[Metadata]] is the aspects of the DFDL schema information that are
 * needed at runtime.
 *
 * Interfacing Daffodil to other data handling systems requires both a metadata bridge
 * be built that takes Daffodil metadata into that system's metadata, and a data bridge
 * that takes Daffodil data to that system's data.
 *
 * Bridging this runtime1 library to other data handling software is most easily done
 * directly from runtime1's metadata and data structures.
 * (The Daffodil Schema Compiler's walkers are an alternative, but are more
 * for interfacing the Daffodil schema compiler data structures to other compiler backends.)
 *
 * This walker/handler works on the pre-compiled binary schema
 * just as well as if the schema was just compiled. This bypasses the need for Daffodil's
 * schema compiler to be involved at all in interfacing to say, Apache Drill or other
 * data fabrics. A pre-compiled DFDL schema is all that is needed.
 */
abstract class MetadataHandler() {

  /**
   * Called for simple type element metadata (for declarations or references)
   *
   * There are no separate start/end methods for simple element declarations.
   *
   * @param m the simple element metadata for the element declaration.
   */
  def simpleElementMetadata(m: SimpleElementMetadata): Unit

  /**
   * Called for complex type element metadata (for declarations or references)
   *
   * Subsequent calls will be for the model group making up the content
   * of the element.
   * @param m the complex element metadata for the element declaration that is starting
   */
  def startComplexElementMetadata(m: ComplexElementMetadata): Unit

  /**
   * Called for complex type element metadata (for declarations or references)
   *
   * This is called after all the calls corresponding to the content of the
   * complex type element.
   * @param m the complex element metadata for the element declaration that is ending
   */
  def endComplexElementMetadata(m: ComplexElementMetadata): Unit

  /**
   * Called for sequence group definitions.
   *
   * Subsequent calls will be for the content of the sequence.
   * @param m the sequence metadata for the sequence definition that is starting
   */
  def startSequenceMetadata(m: SequenceMetadata): Unit

  /**
   * Called for sequence group definitions.
   *
   * This is called after all the calls corresponding to the content
   * of the sequence group.
   * @param m the sequence metadata for the sequence definition that is ending
   */
  def endSequenceMetadata(m: SequenceMetadata): Unit

  /**
   * Called for choice group definitions.
   *
   * Subsequent calls will be for the content of the choice.
   * @param m the choice metadata for the choice definition that is starting
   */
  def startChoiceMetadata(m: ChoiceMetadata): Unit

  /**
   * Called for choice group definitions.
   *
   * This is called after all the calls corresponding to the content
   * of the choice group.
   * @param m the choice metadata for the choice definition that is ending
   */
  def endChoiceMetadata(m: ChoiceMetadata): Unit

}
