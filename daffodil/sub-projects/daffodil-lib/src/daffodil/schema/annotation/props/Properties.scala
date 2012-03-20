package daffodil.schema.annotation.props

// Copyright (C) 2012, Michael J. Beckerle. All Rights Reserved.

import daffodil.exceptions._

 /**
   * Enum class as basis for our DFDL properties
   * 
   * This is the best Enum idiom I could find on StackOverflow and other Scala web sites. 
   * <p>
   * An enumeration for a DFDL property is defined like this:
   * <pre>
   * ////////////////////////////////////////////////////////////////////////
   * // <xsd:simpleType name="BinaryNumberRepEnum" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/">
   * // 		<xsd:restriction base="xsd:string">
   * // 			<xsd:enumeration value="packed"></xsd:enumeration>
   * // 			<xsd:enumeration value="bcd"></xsd:enumeration>
   * // 			<xsd:enumeration value="binary"></xsd:enumeration>
   * // 		</xsd:restriction>
   * // 	</xsd:simpleType>
   * 
   * sealed trait BinaryNumberRep extends BinaryNumberRep.Value
   * object BinaryNumberRep extends Enum[BinaryNumberRep] {
   *   case object Packed extends BinaryNumberRep ; forceConstruction(Packed)
   *   case object Bcd extends BinaryNumberRep ; forceConstruction(Bcd)
   *   case object Binary extends BinaryNumberRep ; forceConstruction(Binary)
   * 
   *   def apply(name: String) : BinaryNumberRep = stringToEnum("binaryNumberRep", name)
   * }
   *   
   * trait BinaryNumberRepMixin extends PropertyMixin {
   *   //
   *   // lazy so if we don't need the property we don't access it. If we do access it, and it
   *   // is not found, it is a schema definition error.
   *   //
   *   lazy val binaryNumberRep = BinaryNumberRep(getProperty("binaryNumberRep"))
   *   ...other stuff...
   * }
   * </pre>
   * The first bunch of // comments is the fragment of the DFDL Annotations schema which 
   * defines the enumeration. The scala code to realize this follows (this is the output 
   * from a code generator). This is just in here to serve as documentation.
   * <p>
   * For the most as a programmer you don't have to deal with the above code much. To 
   * use a property you mix in the trait BinaryNumberRepMixin into the class which is 
   * to have access to the property. This is actually done with traits that group these individual enum
   * traits into the proper sets for the various schema components. 
   * <p>
   * Then within that class's code, to access the property value you just use the lazy val, 
   * in this case binaryNumberRep. The type of which will be the enum, with values given by the
   * case objects BinaryNumberRep.Packed, BinaryNumberRep.Bcd, and BinaryNumberRep.Binary. 
   * <p>
   * Code using properties never deals with the strings for these enumerations, 
   * and never deals with where/how to retrieve properties for any properties, enum or otherwise.
   * Also, retrieving properties is unconditional. 
   * If you ask for a property value and it's not defined, it's a schema-definition error.
   * (This is part of the DFDL spec. Implementations are not to provide default values for properties.)
   */
  abstract class EnumBase
  abstract class EnumValueBase 
  abstract class Enum[A] extends EnumBase {
    class Value extends EnumValueBase { self: A => {
    	val theVal = this
        _values :+= theVal
        _values
      }
    }
    
    def toPropName(prop : A) = {
      val s = prop.toString
      val capS = s.substring(0,1).toLowerCase()
      val propName = capS + s.substring(1)
      propName
    }
    
    private var _values = List.empty[A]
    def stringToEnum(enumTypeName : String, str : String) = {
      val opt = _values.find(_.toString.toLowerCase() == str.toLowerCase)
      opt match {
        case Some(e) => e
        case None => Assert.unknownPropertyValue(enumTypeName, str) 
      }
    }
    /**
     * Useful for diagnostic messages where you want to say "must be one of ...." and list the possibilities.
     */
    def allValues = _values
    
    /**
     * Scala delays construction of case objects (presumably because many programs don't use them at all)
     * We need to force creation of our inner property case objects because constructing them also has
     * the side-effect of registering them in the _values list.
     */
    def forceConstruction(obj : Any) {
      //Assert.invariant(obj.toString() != "") //TODO: is forceConstruction needed at all?
    }
  } // end class

/**
 * base mixin for traits representing collections of DFDL properties
 * 
 * The key here is the getPropertyOption member. This function is implemented by 
 * the things that this is mixed into. E.g., like the element object that represents
 * element declarations. Or more likely a base class that represents any 
 * annotated object that can have DFDL properties on it.
 * <p>
 * The getPropertyOption routine will end up looking something like this:
 * <pre>
 *   def getPropertyOption(pname: String): String = {
 *     // prior to this being called, or as lazy val attributes, we need
 *     // list of short form (which are local) and long form annotations.
 *     // ref chains (dfdl:ref)
 *     // schema default properties
 *     // all goes into this one map dfdlCombinedProperties
 *     val maybeProp = dfdlCombinedProperties.get(pname)
 *     maybeProp
 *    }
 *   }
 * </pre>
 * The magic here is the dfdlCombinedProperties member which is a big map of all the 
 * properties (that's what has to take the scoping rules into account, and get properties locally 
 * or from other places)
 */
trait PropertyMixin {
  
  /**
   * Properties will push their toString function onto this list
   * 
   * Each object that has any properties mixed in must implement this
   * value. 
   */
  var toStringFunctionList : List[()=>String] = Nil
  
  /**
   * prints every property in scope. 
   * 
   * This is likely way more than the number locally defined.... dozens 
   * of properties are likely defined in most formats. 
   */
//  override def toString = {
//    val props = toStringFunctionList.map{f=>f.apply()}.foldLeft("PropertyMixin(")(_+_)
//    val suffix=")"
//    props + suffix
//  }
  
  def registerToStringFunction(f : (() => String)) {
    toStringFunctionList = toStringFunctionList :+ f
  }
  /**
   * Call this to get a property. 
   * 
   * Property values are non-optional in DFDL. If they're not
   * there but a format requires them, then it's always an error.
   * 
   * Note also that DFDL doesn't have default values for properties. That means
   * that most use of properties is unconditional. Get the property value, and 
   * it must be there, or its an error. There are very few exceptions to this
   * rule.
   */
  def getProperty(pname: String): String = {
    val propOpt = getPropertyOption(pname) 
    propOpt match {
      case Some(prop) => prop
      case None => Assert.schemaDefinitionError("Property " + pname + " is not defined.")
    }
  }
  
  /**
   * For testing, debug, printing, and other times when we really do need
   * to know whether or not there is a definition of a particular property.
   */
  def getPropertyOption(pname : String) : Option[String]

  /**
   * Convert a property string value to a Boolean
   * 
   * Note: no error checking is required, as we assume the schema has already been 
   * validated, so we only have to deal with the valid formats for Boolean. 
   */
  def convertToBoolean(pv: String) = {
    pv match {
      case "true" => true
      case "false" => false
      case "TRUE" => true
      case "FALSE" => false
    }
  }

  /**
   * Convert a property string value to a Int
   * 
   * Note: no error checking is required, as we assume the schema has already been 
   * validated, so we only have to deal with the valid formats for Int. 
   */
  def convertToInt(pv: String) = {
    pv.toInt
  }
  
  def convertToDouble(pv: String) = {
    pv.toDouble
  }
  
  def convertToFloat(pv: String) = {
    pv.toFloat
  }

  /**
   * There's no conversion to do here, but to eliminate a special case in the code generator
   * we always generate a call to a convertToTYPE function.
   */
  def convertToString(pv: String) = {
    // TODO DFDL String Literal processing to deal with 
    // entities and raw bytes
    pv
  }
  
  def convertToQName(pv: String) : String = {
    // remember: schema validation already checked format of QName for us.
    pv
  }
  
  def convertToNCName(pv: String) : String = {
  // remember: schema validation already checked format of NCName for us.
    pv
  }

} // end trait


