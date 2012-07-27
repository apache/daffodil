<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE dfdl-canonicalization [
  <!ENTITY dfdl-source "http://www.ogf.org/dfdl/">
  <!ENTITY dfdl-ns "http://www.ogf.org/dfdl/dfdl-1.0/">
  <!ENTITY xs-ns "http://www.w3.org/2001/XMLSchema">
]>
<!--
	Copyright (c) 2011 NCSA.  All rights reserved.
	Developed by: NCSA Cyberenvironments and Technologies
	University of Illinois at Urbana-Champaign
	http://cet.ncsa.uiuc.edu/
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to
	deal with the Software without restriction, including without limitation the
	rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
	sell copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	1. Redistributions of source code must retain the above copyright notice,
	this list of conditions and the following disclaimers.
	2. Redistributions in binary form must reproduce the above copyright
	notice, this list of conditions and the following disclaimers in the
	documentation and/or other materials provided with the distribution.
	3. Neither the names of NCSA, University of Illinois, nor the names of its
	contributors may be used to endorse or promote products derived from this
	Software without specific prior written permission.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
	CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
	WITH THE SOFTWARE.
-->
<!--
	DFDL Canonicalization transformation.
	Converts all DFDL annotations in attribute or short form into element form, and leaves
	the rest of the DFDL schema as is.
	The exception to converting everything to element form are the four annotations that may
	not be represented in that form, as described in section 7.1.3 of the DFDL specification.
	Those are canonicalized to attribute form.
	by Joe Futrelle 12/2010
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:dfdl="&dfdl-ns;" xmlns:xs="&xs-ns;">

	<xsl:output indent="yes"/>

	<!-- suppress all extraneous output -->
	<xsl:template match="*|@*|text()"/>

	<xsl:template
		match="xs:schema | xs:choice | xs:element | xs:sequence | xs:simpleType | xs:complexType | xs:group">
		<!-- copy the node -->
		<xsl:element name="xs:{local-name()}">
			<!-- force "xs:" namespace prefix -->
			<!-- copy the non-dfdl attributes -->
			<xsl:for-each select="@*[not(namespace-uri()='&dfdl-ns;')]">
				<xsl:call-template name="copy-attribute"/>
			</xsl:for-each>
			<!-- canonicalize dfdl annotations in short form, if any -->
			<xsl:choose>
				<xsl:when test="@*[namespace-uri()='&dfdl-ns;']">
					<xsl:element name="xs:annotation" namespace="&xs-ns;">
						<xsl:element name="xs:appinfo" namespace="&xs-ns;">
							<xsl:attribute name="source">&dfdl-source;</xsl:attribute>
							<!-- determine what to call the dfdl annotation element corresponding to the annotation point -->
							<xsl:variable name="annotation-name">
								<xsl:call-template name="dfdl-name-for">
									<xsl:with-param name="xs-name" select="local-name(.)"/>
								</xsl:call-template>
							</xsl:variable>
							<!-- emit the annotation element -->
							<xsl:element name="{$annotation-name}" namespace="&dfdl-ns;">
								<!-- certain short form annotations cannot be represented in element form; convert them to attribute form -->
								<xsl:for-each
									select="@dfdl:ref | @dfdl:escapeSchemeRef | @dfdl:hiddenGroupRef | @dfdl:prefixLengthType">
									<xsl:attribute name="{name()}">
										<xsl:value-of select="."/>
									</xsl:attribute>
								</xsl:for-each>
								<!-- convert any remaining dfdl short form annotations to element form -->
								<xsl:apply-templates select="@*[namespace-uri()='&dfdl-ns;']"/>
							</xsl:element>
						</xsl:element>
					</xsl:element>
				</xsl:when>
			</xsl:choose>
			<!-- canonicalize other annotations, and descend -->
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>

	<!-- match an existing xs:annotation -->
	<xsl:template match="xs:annotation">
		<xs:annotation>
			<xsl:apply-templates select="xs:appinfo"/>
		</xs:annotation>
	</xsl:template>

	<!-- canonicalize dfdl appinfo sections -->
	<xsl:template match="xs:appinfo[@source='&dfdl-source;']">
		<!-- copy it -->
		<xs:appinfo source="&dfdl-source;">
			<!-- handle child dfdl annotations -->
			<xsl:apply-templates select="*[self::dfdl:*]"/>
		</xs:appinfo>
	</xsl:template>

	<!-- leave non-dfdl appinfo sections as is -->
	<xsl:template match="xs:appinfo[not(@source='&dfdl-source;')]">
		<xsl:copy-of select="."/>
	</xsl:template>

	<!-- canonicalize a dfdl annotation corresponding to an annotation point, in element or attribute form -->
	<xsl:template
		match="dfdl:choice | dfdl:element | dfdl:group | dfdl:format | dfdl:sequence | dfdl:simpleType"
		priority="2">
		<xsl:element name="dfdl:{local-name()}">
			<!-- some attribute form annotations may not be represented in element form; leave them in attribute form -->
			<xsl:for-each select="@ref | @escapeSchemeRef | @hiddenGroupRef | @prefixLengthType">
				<xsl:attribute name="{name()}">
					<xsl:value-of select="."/>
				</xsl:attribute>
			</xsl:for-each>
			<!-- convert remaining attribute form annotations to element form -->
			<xsl:for-each select="@*">
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			<!-- retain existing element-form annotations -->
			<xsl:for-each select="dfdl:property">
				<xsl:copy-of select="."/>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<!-- canonicalize attribute-form annotations by converting into property form -->
	<xsl:template match="@*[not(namespace-uri()='&dfdl-ns;')]" priority="1">
		<dfdl:property name="{name()}">
			<xsl:value-of select="."/>
		</dfdl:property>
	</xsl:template>

	<!-- except for the ones that cannot be represented in property
	form; suppress output, as those have been handled earlier -->
	<xsl:template match="@ref | @escapeSchemeRef | @hiddenGroupRef | @prefixLengthType" priority="2"/>

	<!-- copy any dfdl annotation not corresponding to an annotation point (e.g., dfdl:setVariable) -->
	<xsl:template match="*[self::dfdl:*]" priority="1">
		<xsl:element name="dfdl:{local-name()}">
			<!-- copy all attributes -->
			<xsl:for-each select="@*">
				<xsl:call-template name="copy-attribute"/>
			</xsl:for-each>
			<!-- descend to copy children -->
			<xsl:apply-templates/>
			<!-- TODO what about any text nodes? -->
		</xsl:element>
	</xsl:template>

	<!-- canonicalize dfdl annotations in short form by converting them to property form -->
	<xsl:template match="@*[namespace-uri()='&dfdl-ns;']" priority="1">
		<dfdl:property>
			<xsl:attribute name="name">
				<xsl:value-of select="local-name(.)"/>
			</xsl:attribute>
			<xsl:value-of select="."/>
		</dfdl:property>
	</xsl:template>

	<!-- except for ones that cannot be represented in element form (see section 7.1.3);
		suppress output, as they have already been handled -->
	<xsl:template
		match="@dfdl:ref | @dfdl:escapeSchemeRef | @dfdl:hiddenGroupRef | @dfdl:prefixLengthType"
		priority="2"/>

	<!-- this is used to copy non-dfdl attributes, and correct namespace prefixes
		on QNames occuring in "type" attributes -->
	<xsl:template name="copy-attribute">
		<!-- for xs:element/@type -->
		<xsl:choose>
			<!-- AFAIK "type" is the only attribute that needs this transformation;
				XML schema also has "memberType" and "itemType"	attributes bearing QNames,
				but I don't think those occur in DFDL -->
			<xsl:when test="name()='type'">
				<xsl:variable name="type-ns-prefix" select="substring-before(.,':')"/>
				<xsl:choose>
					<!-- not namespace-prefixed, ignore -->
					<xsl:when test="$type-ns-prefix=''">
						<xsl:attribute name="type">
							<xsl:value-of select="."/>
						</xsl:attribute>
					</xsl:when>
					<!-- namespace-prefixed -->
					<xsl:otherwise>
						<xsl:variable name="type-ns-uri"
							select="string(../namespace::*[name()=$type-ns-prefix])"/>
						<xsl:choose>
							<!-- XML schema type: canonicalize prefix -->
							<xsl:when test="$type-ns-uri='&xs-ns;'">
								<xsl:attribute name="type">
									<xsl:text>xs:</xsl:text>
									<xsl:value-of
										select="substring-after(substring-after(.,$type-ns-prefix),':')"
									/>
								</xsl:attribute>
							</xsl:when>
							<!-- otherwise we need to declare the prefix -->
							<xsl:otherwise>
								<!-- copy the type -->
								<xsl:copy/>
								<!-- now declare the namespace prefix. XSLT 1.0 provides limited control over prefix
									declarations. as a result this approach may not work in some processors. however, this case, a non-
									XSD datatype with a non-local name, is unlikely to occur in a DFDL schema. -->
								<xsl:variable name="ns-decl">
									<xsl:element name="{$type-ns-prefix}:ignore"
										namespace="{$type-ns-uri}"/>
								</xsl:variable>
								<xsl:copy-of select="exsl:node-set($ns-decl)//namespace::*"
									xmlns:exsl="http://exslt.org/common"/>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- correspondence between schema components and DFDL annotation names (table 7 in the spec) -->
	<xsl:template name="dfdl-name-for">
		<xsl:param name="xs-name"/>
		<xsl:choose>
			<xsl:when test="$xs-name='choice'">dfdl:choice</xsl:when>
			<xsl:when test="$xs-name='element'">dfdl:element</xsl:when>
			<xsl:when test="$xs-name='group'">dfdl:group</xsl:when>
			<xsl:when test="$xs-name='schema'">dfdl:format</xsl:when>
			<xsl:when test="$xs-name='sequence'">dfdl:sequence</xsl:when>
			<xsl:when test="$xs-name='simpleType'">dfdl:simpleType</xsl:when>
		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>
