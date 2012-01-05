
Introduction
============

    
The tests in this package represent a sampling of some of the basic functionality of a DFDL processor implementation.  
They are not any guarantee of compliance. There are two test files:

    dpaext1.tdml
    dpaext2.tdml     
     
These XML files each contain a number of tests. Each test specifies the data to parse, the expected resultant infoset, and a pointer 
to the DFDL schema. The following XML tags are used:

    parserTestCase - this tag represents one test. It has attributes: 
     
	name  - (must be unique within the full xml file)
	root - the root element of the DFDL schema from which to start processing
	model - the location of the DFDL schema to use
	description - a short summary of the purpose of the tests, including relevant DFDL spec section.
     
    Within the parserTestCase are the document and infoset tags.
     
	The document tag is the input document to be parsed.  In can be in readable text format, or within a documentpart with attribute of type=
	"byte" to provide the value in the hex equivalent form, as this is clearer in showing the input values for things such as binary numbers.
     
	The infoset tag encloses the expected output of the parser, with the tags of the corresponding elements that are parsed using the DFDL
	schema under test.  For string elements, the xsi:type is omitted; for other types it is explicitly provided
   
   
The tests are in order relative to the content of the DFDL specification.  Not all of sections of the specification are covered.  In some cases,
the sample schemas given in the specification are used (made into complete schemas as necessary).
      
    
The rest of this readme is a overall summary of the tests included in this set.  For further detail, see the description attribute
for each test, or review the DFDL schema directly.


Tests
=====


The first two tests pick up the samples in Section 1.2.1 of the DFDL specification.  They are here to show
the basic demonstration of DFDL with binary and text number formats  
	
The next set of tests show a number of schema types that can be used, as per described in Section 5.  For these examples,
an explicit length and a simple number pattern is used.   It is not comprehensive, and further tests are provided 
later for more variation in both length and patterns. 

There are no error tests provided here to demonstrate failure if a DFDL schema uses any of the reserved constructs noted
in Section 5.1 

Facets, default and fixed, though covered in Section 5, are optional, and so no tests here for those. The attributes minOccurs and maxOccurs 
will be covered with Section 16, arrays and optional elements. 
 
 
The next tests show examples of using DFDL entities in string literals as described in section 6.3 of the DFDL specification, to easily represent
values that aren't normally representable in XML

 
The next tests cover some examples from the Syntax of Annotation Elements in Section 7, showing attribute and element form,  the ability
to have CDATA with element form, and the short form.    
	
	
Section 8 covers property scoping. The tests include a simple unnamed format that is used for defaulting DFDL properties and another for
combining properties from a named format. Also shown is combining properties from references - both user defined type
and element references. Finally, importing schema and how scoping applies to properties in the imported schema. 
			
Section 11 covers properties common to content and framing, so the following test will cover various encodings and byteOrder.
	 
Section 12 covers length properties and framing properties (alignment and markup such as initiators and terminators).
	 
Alignment tests cover general alignment, leading and trailing skip bytes, and implicit alignment. 
	 
Delimiter tests here cover a simple initiator and terminator on a string field, use of ignoreCase property, and
use of a space separated list to indicate multiple values for the initiator may be used in the data. Also, a DFDL enitity NL is used. 
	 
A key part of representation, specification of length has been used in all of the previous tests, so to provide another example
here more directly related to this functionality, two tests will demonstrate the use of lengthUnits=bytes vs lengthUnits=characters when
multibyte character sets are used for explicit lengths. 
	
	
With lengthKind=delimited, tests show various ways to in indicate the end of an element's content - with terminator, with
parent separator, with parent of specified length, with end of input. Delimited length can also be used with BCD fields, which
is shown here.	 Also, an element of explicit length can be a component of a delimited sequence. 
	
Finally, a couple of valid logical types are shown with lengthKind="implicit". 	
	
Section 13 covers simple types, and the properties applicable to these.
    
First, are some tests for padding/trimming, with properties common to simple types with Text representation,
and the string/number specific properties related to padding/trimming. Tests include trimming include trimming in multibyte encodings, 
different justification (right, center, left)
     
     
Section 13.2.1 escapeScheme, and Section 13.3 Bidirectional support, are  optional features, so no tests are included.
  
To demonstrate some of the functionality of Section 13.5, properties specific to number with Text representation,
we have tests with decimal logical types, so show decimal and grouping separators, and a virtual decimal place.
With a logical double type, provide example use of zero representation, not a number representation, infinity representation, 
and exponent character. Both decimal and double tests make use of the numberPattern property to indicate representation of 
required digits and location of decimal place and grouping separator. 

All the above use base 10 for numbers, so another test is included with a logical short to show use of the textStandardBase
property to indicate number bases of 2,8, and 16.
     
Tests for Section 13.7, Properties specific to Number with Binary Representation, include use of the properties
binaryNumberRep, binaryDecimalVirtualPoint,  decimalSigned,  binaryNumberCheckPolicy. The first, with a BCD number contrasts
a logical decimal type with a virtual decimal point, and an integer type. The second test is a simple IEEE float number. 
	
	
Sections 13.9 and 13.10 of the DFDL specification cover properties related to logical boolean values, text and binary. 
For boolean text, the first test has an explicit length for the boolean element, so a entity is used to pad the short
boolean true representation value to the same length as false.  Note this is treated  as part of the boolean representation in this case
and not a pad character.  The second test shows the use of text numbers (1,0) for the boolean representations.
The third test shows the use of lengthKind implicit with varying length of boolean representation, and the fourth, the use
of a logical boolean in a binary  representation 
     
	
Section 13.10, 13.12, and 13.13 cover the properties specific to calendar objects, text and binary. 
Tests are included to cover padding and trimming with calendar fields, (properties textCalendarJustification, textCalendarPadCharacter)
calendarPattern and PatternKind(including implicit), and celendarFirstDayOfWeeK calendardDaysInFirstWeek (to convert a date value
from input provide in a form that indicates the Monday of the first week of 2010.)  
With the calendar property calendarStrictChecking, we show 56th week  is acceptable if checking is lax.
If the calendar presentation does not include time zone or has implied century, another test shows how calendarTimeZone, calendarCenturyStart
properties can be used to indicate this information in the calendar object in the infoset.
Finally, two tests show the basic use of binarySeconds and binaryMilliseconds, for logical calendar object represented in seconds or
milliseconds after a specified Epoch value.
	

Section 13.15, 13.16, 13.17 cover nil and default processing, which are optional, so no tests are included.
   
Section 14 of the DFDL specification covers Sequence groups. As unordered groups and groups with floating components are optional, 
only tests with ordered groups are included.  Tests show the various separator positions (infix, postfix, and prefix) and separator policies, including
scenarios where optional elements of the group are missing. The first tests use delimited length fields, but there are two
tests that show a sequence with explicit length fields. 
   
  	
Section 15 covers Choice Groups.  The tests for this take a single global element that has nested choices, and 
demonstrates the input and expected infoset values for various choice branches taken. In these tests, the lengthKind is delimited.
	 
Section 16 covers Arrays and Optional elements. In the sequence groups tests above, we showed the use
of optional elements in a sequence.   The test here has a sequence that include 3 different elements that all repeat.
The first element has occursCountKind=fixed, so must have exactly 2 occurrences. The Other two elements show 
the use of occursCountKind=implicit.
	 


