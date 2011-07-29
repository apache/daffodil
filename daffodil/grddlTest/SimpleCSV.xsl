<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE xsl:stylesheet [
         <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
	 <!ENTITY rdfs 'http://www.w3.org/2000/01/rdf-schema#'>
         <!ENTITY dfdl 'http://www.example.org/example1/'>

         <!-- RDF doesn't allow local URIs -->
         <!ENTITY absdfdl 'http://www.example.org/example1/'>
]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:rdf="&rdf;" xmlns:rdfs="&rdfs;" 
                xmlns:dfdl="&dfdl;" xmlns:absdfdl="&absdfdl;" 
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                version="1.0">

  <xsl:output method="xml" version="1.0" encoding="utf-8" indent="yes"/>


  <xsl:template match="/dfdl:table">
    <rdf:RDF>
      <!-- With XPATH 2.0 we could create a URI for the resource based
           on the document location. That is less than ideal and xpath 2
           support is scarce --> 
      <rdf:Description>
        <rdf:type rdf:resource="&absdfdl;table"/>
        <dc:creator><xsl:value-of 
                       select="dfdl:hdrblock/dfdl:Creator"/></dc:creator>
        <dc:date><xsl:value-of 
                    select="dfdl:hdrblock/dfdl:Date"/></dc:date>
      </rdf:Description>
    </rdf:RDF>
  </xsl:template>

</xsl:stylesheet>
