package daffodil

import daffodil.schema.annotation.AttributeValue
import daffodil.xml.XMLUtil

object ImplicitsForTesting {
 /**
   * makes tests easier to cope with. can write processor.setTerminator(";") instead of having
   * to convert.
   */
 implicit def string2AttributeValue(value:String): AttributeValue = XMLUtil getListFromValue(value)

}