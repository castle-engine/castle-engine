<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ***************************************************************
     VampyreDoc Tool
          
     XSL stylesheet which transforms VampyreDoc contents XML file to
     HTMLHelp hhc file.

     **************************************************************** -->

<xsl:output method="html" encoding="utf-8" indent="yes" doctype-public="-//IETF//DTD HTML//EN"/>
  
<xsl:template match="toc">
  <html>
    <head>
      <meta name="generator" content="VampyreDoc"/>
    </head>  
    <body>
      <xsl:apply-templates select="itemlist"/>
    </body>
  </html>
</xsl:template>

<!-- *********************************** 
               TOC Elements
     *********************************** -->

<xsl:template match="itemlist">
  <ul>
    <xsl:apply-templates select="item"/>
  </ul>
</xsl:template>

<xsl:template match="item">
  <li>
    <object type="text/sitemap">
      <param name="Name">
        <xsl:attribute name="value">
          <xsl:value-of select="@name"/>
        </xsl:attribute>
      </param>
      <param name="Local">
        <xsl:attribute name="value">
          <xsl:value-of select="@url"/>
        </xsl:attribute>
      </param>
    </object>
    <xsl:apply-templates select="itemlist"/>
  </li>
</xsl:template>

</xsl:stylesheet> 

