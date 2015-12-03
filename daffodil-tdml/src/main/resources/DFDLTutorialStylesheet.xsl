<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData">

	<xsl:output method="html" />

	<xsl:template match="tdml:testSuite/tdml:tutorial">
		<p>
			<xsl:copy-of select="." />  <!-- Needs to be copy-of to preserve the html -->
		</p>
	</xsl:template>


	<xsl:template match="tdml:testSuite/tdml:parserTestCase/tdml:tutorial">
		<p>
			<xsl:copy-of select="." />  <!-- Needs to be copy-of to preserve the html -->
		</p>
	</xsl:template>
	
	<xsl:template match="tdml:testSuite/tdml:parserTestCase/tdml:document">
	
		<b>Data Stream:</b>
		<table>
			<tr>
				<td>
					<pre class="prettyprint linenums">
						<xsl:copy-of select="." />
					</pre>
				</td>
			</tr>
		</table>
	
	</xsl:template>
	
	<xsl:template match="tdml:testSuite/tdml:parserTestCase/tdml:infoset">
		<xsl:variable name="nodestring">
			<xsl:apply-templates select="." mode="serialize" />
		</xsl:variable>
		<b>Infoset:</b>
		<table>
			<tr>
				<td>
					<pre class="prettyprint linenums">
						<xsl:value-of select="$nodestring" />
					</pre>
				</td>
			</tr>
		</table>
	
	</xsl:template>

	<xsl:template match="*" mode="serialize">
		<xsl:text>&lt;</xsl:text>
		<xsl:value-of select="name()" />
		<xsl:apply-templates select="@*" mode="serialize" />
		<xsl:choose>
			<xsl:when test="node()">
				<xsl:text>&gt;</xsl:text>
				<xsl:apply-templates mode="serialize" />
				<xsl:text>&lt;/</xsl:text>
				<xsl:value-of select="name()" />
				<xsl:text>&gt;</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text> /&gt;</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="@*" mode="serialize">
		<xsl:text> </xsl:text>
		<xsl:value-of select="name()" />
		<xsl:text>="</xsl:text>
		<xsl:value-of select="." />
		<xsl:text>"</xsl:text>
	</xsl:template>

	<xsl:template match="text()" mode="serialize">
		<xsl:value-of select="." />
	</xsl:template>

	<xsl:template match="tdml:testSuite/tdml:defineSchema">
		<xsl:variable name="nodestring">
			<xsl:apply-templates select="." mode="serialize" />
		</xsl:variable>
		<b>DFDL Schema:</b>
		<table>
			<tr>
				<td>
					<pre class="prettyprint linenums">
						<xsl:value-of select="$nodestring" />
					</pre>
				</td>
			</tr>
		</table>
		<br />
	</xsl:template>


	<xsl:template match="/">
		<html>
			<head>
				<style>
					li.L0, li.L1, li.L2, li.L3,
					li.L5, li.L6, li.L7, li.L8
					{ list-style-type: decimal !important }
					li.L0,
					li.L2,
					li.L4,
					li.L6,
					li.L8 { background: #f5f5f5 }
				</style>
				<title>
					<xsl:value-of select="tdml:testSuite/@description" />
				</title>
			</head>
			<body>
				<h2>
					<xsl:value-of select="tdml:testSuite/@description" />
				</h2>
				<xsl:apply-templates />


			</body>

			<script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>

		</html>
	</xsl:template>

</xsl:stylesheet>