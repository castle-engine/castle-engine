<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ***************************************************************
     VampyreDoc Tool
          
     XSL stylesheet which transforms VampyreDoc contents XML file to
     XHTML page.

     **************************************************************** -->

<xsl:output method="xml" encoding="utf-8" indent="yes"
  doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" 
  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
  
<xsl:template match="toc">
  <html>
  <!-- <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"> 
       this adds ugly empty xmlns="" attribute to many elements
       when transforming with Saxon -->
    <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <meta name="generator" content="VampyreDoc"/>
      <link href="../common/doc.css" type="text/css" rel="stylesheet"/> 
      <link href="../../common/doc.css" type="text/css" rel="stylesheet"/> 
      <title>
        <xsl:choose>
          <xsl:when test="title!=''">
            <xsl:value-of select="title"/>        
          </xsl:when>
          <xsl:otherwise>
            Contents
          </xsl:otherwise>
        </xsl:choose>
      </title>
    </head>  
    <body>
      <div class="tocbody">
        <xsl:call-template name="title"/>
        <xsl:apply-templates select="itemlist"/>
      </div>
    </body>
  </html>
</xsl:template>

<!-- *********************************** 
               TOC Elements
     *********************************** -->

<xsl:template name="title">
  <xsl:if test="title!=''">
    <span class="toctitle">
      <xsl:value-of select="title"/>        
    </span>  
  </xsl:if>
</xsl:template> 

<xsl:template match="itemlist">
  <ul class="toc">
    <xsl:apply-templates select="item"/>
  </ul>
</xsl:template>

<xsl:template match="item">
  <li class="toc">
    <a target="docview">
      <xsl:attribute name="href">
        <xsl:value-of select="@url"/>
      </xsl:attribute>
      <xsl:value-of select="@name"/>
    </a>
    <xsl:apply-templates select="itemlist"/>
  </li>
</xsl:template>

</xsl:stylesheet> 

