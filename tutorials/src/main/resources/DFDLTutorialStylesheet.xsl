<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" 
  xmlns="http://www.w3.org/1999/xhtml">



  <xsl:output method="html" doctype-system="about:legacy-compat"/>
      
  <xsl:template match="tdml:tutorial">
    <p>
      <xsl:copy-of select="."/>  <!-- Needs to be copy-of to preserve the html -->
    </p>
  </xsl:template>
  
 <xsl:template match="tdml:testSuite/tdml:parserTestCase[@tdml:tutorialInclude='no']"/>
  <xsl:template match="tdml:testSuite/tdml:parserTestCase[not(@tdml:tutorialInclude)]">
       <h2>Parse Test: <xsl:value-of select="@name"/></h2>
    <p><xsl:value-of select="@description"/></p>
    <xsl:apply-templates select="*"/>
  </xsl:template>
  
  <xsl:template match="tdml:testSuite/tdml:unparserTestCase[@tdml:tutorialInclude='no']"/>
  <xsl:template match="tdml:testSuite/tdml:unparserTestCase[not(@tdml:tutorialInclude)]">
    <h2>Unparse Test: <xsl:value-of select="@name"/></h2>
    <p><xsl:value-of select="@description"/></p>
    <xsl:apply-templates select="*"/>
  </xsl:template>
  
  <xsl:template match="tdml:document[@tdml:tutorialInclude='no']"/>
  <xsl:template match="tdml:document[not(@tdml:tutorialInclude)]">
    <xsl:variable name="nodestring">
      <xsl:apply-templates select="../tdml:document" mode="serialize"/>
    </xsl:variable>
    <b>Data Stream:</b>
    <table>
      <tr>
        <td>
        <pre class="prettyprint linenums">
          <xsl:value-of select="$nodestring"/>
         </pre>
        </td>
      </tr>
    </table>
  </xsl:template>
  
  <xsl:template match="tdml:infoset[@tdml:tutorialInclude='no']"/>
  <xsl:template match="tdml:infoset[not(@tdml:tutorialInclude)]">
    <xsl:variable name="nodestring">
      <xsl:apply-templates select="./tdml:dfdlInfoset/*" mode="serialize"/>
    </xsl:variable>
    <b>Infoset:</b>
    <table>
      <tr>
        <td>
          <pre class="prettyprint linenums">
            <xsl:value-of select="$nodestring"/>
          </pre>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="*" mode="serialize">
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="name()"/>
    <xsl:apply-templates select="@*" mode="serialize"/>
    <xsl:choose>
      <xsl:when test="node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:apply-templates mode="serialize"/>
        <xsl:text>&lt;/</xsl:text>
        <xsl:value-of select="name()"/>
        <xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> /&gt;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="@*" mode="serialize">
    <xsl:text> </xsl:text>
    <xsl:value-of select="name()"/>
    <xsl:text>="</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>"</xsl:text>
  </xsl:template>

  <xsl:template match="text()" mode="serialize">
    <xsl:value-of select="."/>
  </xsl:template>

<!-- These match nodes containing the don't include in tutorial indicator, and 
     also, all-whitespace nodes that precede them. -->
     
 <xsl:template mode="serialize"  match="text()[string-length(translate(., ' &#9;&#xA;&#xD;', '')) = 0 and
    following-sibling::node()[1][@tdml:tutorialInclude='no']]"/>

  <xsl:template mode="serialize"  match="node()[@tdml:tutorialInclude='no']"/>
  
  <xsl:template match="tdml:testSuite/tdml:defineSchema[@tdml:tutorialInclude='no']"/>
 
  <xsl:template match="tdml:testSuite/tdml:defineSchema[not(@tdml:tutorialInclude)]">
    <xsl:variable name="nodestring">
      <xsl:apply-templates select="node()" mode="serialize"/>
    </xsl:variable>
    <b>DFDL Schema:</b>
    <table>
      <tr>
        <td>
          <pre class="prettyprint linenums">
            <xsl:value-of select="$nodestring"/>
          </pre>
        </td>
      </tr>
    </table>
    <br/>
  </xsl:template>
  

  <xsl:template match="/">
    <html>
      <head>
        <link rel="stylesheet" href="DFDLTutorialStylesheet.css" media="all"/>
        <link rel="shortcut icon" href="http://www.tresys.com/favicon.ico?v=2"/>
                <!-- This li.L0, li.L1 etc. is about turning on line numbering for all lines. 
                     The default was to put a line number only every 5 lines. -->
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
          <xsl:value-of select="tdml:testSuite/@suiteName"/>
        </title>
      </head>
      <body id="main">
        <header>
          <a id="logo" href="http://www.tresys.com">
            <img src="tresys.png"/>
          </a>
          <div id="headerText">
            <hr/>
            <h1>
              <xsl:value-of select="tdml:testSuite/@suiteName"/>
            </h1>
            <p>
              For quick reference here is the
              <a href="https://opensource.ncsa.illinois.edu/projects/artifacts/DFDL/latest/documentation/Manual/">DFDL Specification (HTML)</a>
            </p>
            <hr/>
          </div>
        </header>
        <section>
          <xsl:apply-templates select="*"/>
        </section>
        <footer class="pt-footer">
          <div>
            <hr/>
            <h4>About this DFDL Tutorial</h4>
            <p>
              This tutorial page is written in Test Data Markup Language (TDML), which is an XML-based language providing a way of creating a single-file, fully self contained test (or set of tests) for a DFDL processor. A TDML file contains the DFDL schema directly (if it is small enough for that
              to make sense), the test data, and the expected results for one or more tests. A tutorial can be created by inserting prose into the TDML file in special elements that contain
              <a href="http://www.w3.org/TR/html5/the-xhtml-syntax.html">XHTML5</a>
              markup for formatting.
            </p>
            <p>The result is that the TDML file can be used as a test of a DFDL processor, or it can be used to create a tutorial web page. The ability to execute the same file insures that the examples contained in the tutorial actually work as described.
            </p>
            <p>A TDML file is the ideal way to discuss how a particular feature of DFDL works or is supposed to work. Once a feature has been understood, examples and prose can be added that turn the TDML into a tutorial that can save others in the DFDL community a great deal of time and effort.
            </p>
            <p>TDML is also perfect for bug reports where the DFDL processor is not behaving as expected as they contain everything necessary to reproduce the bug.
            </p>
            <h4>About Putting Graphics In Tutorials</h4>
            <p>An important characteristic of TDML-based tutorials is that they are self-contained. By using a few HTML techniques and some free tools that run directly in the web browser, we can insure that the images and diagrams needed for a tutorial can be embedded directly in the TDML file.
            </p>
            <ul>
              <li>
                <a href="https://svg-edit.github.io/svgedit/releases/svg-edit-2.8.1/svg-editor.html">SVG-Edit</a>
                - lets you draw/edit SVG format - which is really the preferred thing for embedding in these tutorials since SVG is part of HTML. Once you have the SVG for your diagram, you can copy it into the HTML tutorial content of the HTML file.
              </li>
              <li>
                <a href='http://freeonlinetools24.com/base64-image'>Base64 image encoder decoder</a>
                can be used to embed images directly into the TDML file, so you can take images, or draw diagrams with your favorite drawing tool, and embed them in the TDML file while still keeping the TDML file self-contained. This tool can also convert the base64 encoding back into an editable
                file if you need to edit the image.
              </li>
              <li>
                This
                <a href="http://www.online-image-editor.com/">Online graphics editor</a>
                can edit PNGs JPG, etc.
              </li>
            </ul>
            <hr/>
            <br/>
            <p style="text-align: left;">
              © 2016 Tresys Technology LLC. All Rights Reserved
              <br/>
            </p>
          </div>
        </footer>
      </body>

      <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>

    </html>
  </xsl:template>

</xsl:stylesheet>
