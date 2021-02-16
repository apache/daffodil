<?xml version="1.0" encoding="UTF-8"?>
<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->
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
    <meta charset="utf-8"/>
    <title><xsl:value-of select="tdml:testSuite/@suiteName"/></title>
    <!-- Enable responsive viewport -->
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <link href="https://daffodil.apache.org/assets/themes/apache/bootstrap/css/bootstrap.css" rel="stylesheet"/>
    <link href="https://daffodil.apache.org/assets/themes/apache/css/style.css?body=1" rel="stylesheet" type="text/css"/>
    <link href="https://daffodil.apache.org/assets/themes/apache/css/syntax.css" rel="stylesheet"  type="text/css" media="screen" />
    <!-- 
      This li.L0, li.L1 etc. is about turning on line numbering for all lines. 
      The default was to put a line number only every 5 lines. 
      -->
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
    </head>
    <body id="main">
   <div class="navbar navbar-inverse" role="navigation">
      <div class="container">
        <div class="navbar-header"><a class="navbar-brand" href="/"><img src="https://daffodil.apache.org/assets/themes/apache/img/apache-daffodil-logo.png" alt="Apache Daffodil"/></a></div>
        <nav role="navigation">
          <ul class="nav navbar-nav navbar-right">
            <li><a href="https://daffodil.apache.org/releases">Releases</a></li>
            <li id="documentation">
              <a href="#" data-toggle="dropdown" class="dropdown-toggle">Docs<b class="caret"></b></a>
              <ul class="dropdown-menu dropdown-left">
                <li><a href="https://daffodil.apache.org/getting-started/">Getting Started</a></li>
                <li><a href="https://daffodil.apache.org/examples/">Examples</a></li>
                <li><a href="https://daffodil.apache.org/docs/latest/javadoc/">Java API</a></li>
                <li><a href="https://daffodil.apache.org/docs/latest/scaladoc/">Scala API</a></li>
                <li><a href="https://daffodil.apache.org/docs/dfdl/">DFDL Specification</a></li>
                <li><a href="https://daffodil.apache.org/unsupported/">Unsupported Features</a></li>
                <li><a href="https://daffodil.apache.org/faq/">Frequently Asked Questions</a></li>
              </ul>
            </li>
            <li id="community">
              <a href="#" data-toggle="dropdown" class="dropdown-toggle">Community<b class="caret"></b></a>
              <ul class="dropdown-menu dropdown-left">
                <li><a href="https://daffodil.apache.org/community">Get Involved</a></li>
                <li><a href="https://daffodil.apache.org/people">People</a></li>
              </ul>
            </li>
            <li id="development">
              <a href="#" data-toggle="dropdown" class="dropdown-toggle">Development<b class="caret"></b></a>
              <ul class="dropdown-menu dropdown-left">
                <li><a class="external" href="https://cwiki.apache.org/confluence/display/DAFFODIL/">Wiki</a></li>
                <li><a class="external" href="https://github.com/search?q=repo%3Aapache%2Fdaffodil+repo%3Aapache%2Fdaffodil-site&amp;type=Repositories">GitHub</a></li>
                <li><a class="external" href="https://issues.apache.org/jira/projects/DAFFODIL/">JIRA</a></li>
              </ul>
            </li>
            <li id="apache">
              <a href="#" data-toggle="dropdown" class="dropdown-toggle">Apache<b class="caret"></b></a>
              <ul class="dropdown-menu">
                <li><a class="external" href="https://www.apache.org/">Apache Software Foundation</a></li>
                <li><a class="external" href="https://www.apache.org/licenses/">License</a></li>
                <li><a class="external" href="https://www.apache.org/security">Security</a></li>
                <li><a class="external" href="https://www.apache.org/foundation/sponsorship.html">Sponsorship</a></li>
                <li><a class="external" href="https://www.apache.org/foundation/thanks.html">Thanks</a></li>
              </ul>
            </li>
          </ul>
        </nav>
      </div>
    </div>
        <div class="title">
          <div class="container">
            <h2><xsl:value-of select="tdml:testSuite/@suiteName"/></h2>
          </div>
        </div>
        <div class="container">
          <div class="row">
            <div class="col-md-12">
              <section>
                <xsl:apply-templates select="*"/>
              </section>
            </div>
          </div>
          <footer class="pt-footer">
   <footer class="site-footer">
    <div class="wrapper">
        <div class="footer-col-wrapper" style="font-size: .85em;">
            <hr/>
            <div class="container">
              <div class="col-xs-9">
                <div class="alert alert-warning">
                  This tutorial is runnable as a TDML Test file. 
                  Learn more about 
                  <a href="https://daffodil.apache.org/assets/tutorials/aboutTDMLTutorials">TDML Tutorials here</a>
                </div>
              </div>
            </div>
        </div>
    </div>
</footer>
  <footer class="site-footer">
    <div class="wrapper">
        <div class="footer-col-wrapper" style="font-size: .85em;">
            <hr/>
            <div>
                <div style="text-align: center;">
                    Copyright &#169; 2021 <a href="https://www.apache.org">The Apache Software Foundation</a>.
                    Licensed under the <a href="https://www.apache.org/licenses/LICENSE-2.0">Apache License, Version
                    2.0</a>.
                    <br/>
                    Apache, Apache Daffodil, Daffodil, and the Apache Daffodil logo
                    are trademarks of The Apache Software Foundation.
                </div>
            </div>
        </div>
    </div>
</footer>
          </footer>
        </div>
        <script src="https://daffodil.apache.org/assets/themes/apache/jquery/jquery-2.1.1.min.js"></script>
        <script src="https://daffodil.apache.org/assets/themes/apache/bootstrap/js/bootstrap.min.js"></script>
      </body>
      <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
    </html>
  </xsl:template>

</xsl:stylesheet>
