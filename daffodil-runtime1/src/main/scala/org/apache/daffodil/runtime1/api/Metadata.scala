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
package org.apache.daffodil.runtime1.api

import java.lang.{ Long => JLong }
import scala.collection.JavaConverters._
import scala.xml.NamespaceBinding

import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

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
   * <p/>
   * This is for use in diagnostic messaging. It is not the actual file URI, because
   * those may contain personal-identifying information about the person/acccount and
   * system that compiled the schema. It will provide enough content about the file URI that
   * a user will be able to identify which file, but some prefix of the path
   * components trimmed to make it of a manageable length.
   * <p/>
   * Used along with [[org.apache.daffodil.runtime1.api.Metadata.schemaFileLineNumber]]
   * and [[org.apache.daffodil.runtime1.api.Metadata.schemaFileLineColumnNumber]],
   * this can give a precise location in the DFDL schema file.
   * @return a string containing the file information, or null if unknown.
   */
  def schemaFileInfo: String

  /**
   * Provides the line number to go with [[org.apache.daffodil.runtime1.api.Metadata.schemaFileInfo]].
   * @return the line number as a string, or null if unknown.
   */
  def schemaFileLineNumber: JLong

  /**
   * Provides the column number within the text line, to go with [[org.apache.daffodil.runtime1.api.Metadata.schemaFileLineNumber]].
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
   * <p/>
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

  def primitiveType: PrimitiveType
}

/**
 * Instances are static objects that represent the DFDL primitive types.
 */
trait PrimitiveType {
  def name: String
}

/**
 * Static methods related to PrimitiveType objects
 */
object PrimitiveType {

  private lazy val _list: java.util.List[PrimitiveType] =
    NodeInfo.allDFDLTypes.asInstanceOf[Seq[PrimitiveType]].asJava

  /**
   * Get a primitive type given a name string.
   *
   * @param name lookup key. Case insensitive.
   * @return the PrimitiveType with that name, or null if there is no such primitive type.
   */
  def fromName(name: String): PrimitiveType =
    NodeInfo.primitiveTypeFromName(name)

  /**
   * A list of all the primitive type objects.
   */
  def list: java.util.List[PrimitiveType] = _list

  val String: PrimitiveType = PrimType.String
  val Int: PrimitiveType = PrimType.Int
  val Byte: PrimitiveType = PrimType.Byte
  val Short: PrimitiveType = PrimType.Short
  val Long: PrimitiveType = PrimType.Long
  val Integer: PrimitiveType = PrimType.Integer
  val Decimal: PrimitiveType = PrimType.Decimal
  val UnsignedInt: PrimitiveType = PrimType.UnsignedInt
  val UnsignedByte: PrimitiveType = PrimType.UnsignedByte
  val UnsignedShort: PrimitiveType = PrimType.UnsignedShort
  val UnsignedLong: PrimitiveType = PrimType.UnsignedLong
  val NonNegativeInteger: PrimitiveType = PrimType.NonNegativeInteger
  val Double: PrimitiveType = PrimType.Double
  val Float: PrimitiveType = PrimType.Float
  val HexBinary: PrimitiveType = PrimType.HexBinary
  val AnyURI: PrimitiveType = PrimType.AnyURI
  val Boolean: PrimitiveType = PrimType.Boolean
  val DateTime: PrimitiveType = PrimType.DateTime
  val Date: PrimitiveType = PrimType.Date
  val Time: PrimitiveType = PrimType.Time
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
   */
  def simpleElementMetadata(m: SimpleElementMetadata): Unit

  /**
   * Called for complex type element metadata (for declarations or references)
   *
   * Subsequent calls will be for the model group making up the content
   * of the element.
   */
  def startComplexElementMetadata(m: ComplexElementMetadata): Unit

  /**
   * Called for complex type element metadata (for declarations or references)
   *
   * This is called after all the calls corresponding to the content of the
   * complex type element.
   * @param m
   */
  def endComplexElementMetadata(m: ComplexElementMetadata): Unit

  /**
   * Called for sequence groups.
   *
   * Subsequent calls will be for the content of the sequence.
   * @param m
   */
  def startSequenceMetadata(m: SequenceMetadata): Unit

  /**
   * Called for sequence groups.
   *
   * This is called after all the calls corresponding to the content
   * of the sequence group.
   * @param m
   */
  def endSequenceMetadata(m: SequenceMetadata): Unit

  /**
   * Called for choice groups.
   *
   * Subsequent calls will be for the content of the choice.
   * @param m
   */
  def startChoiceMetadata(m: ChoiceMetadata): Unit

  /**
   * Called for choice groups.
   *
   * This is called after all the calls corresponding to the content
   * of the choice group.
   * @param m
   */
  def endChoiceMetadata(m: ChoiceMetadata): Unit

}
