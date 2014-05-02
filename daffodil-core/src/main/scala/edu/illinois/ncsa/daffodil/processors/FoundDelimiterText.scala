package edu.illinois.ncsa.daffodil.processors

class FoundDelimiterText(val foundText: String, val originalRepresentation: String) {

  override def toString(): String = "<FoundText text='%s' lookingFor='%s'/>".format(foundText, originalRepresentation)
}