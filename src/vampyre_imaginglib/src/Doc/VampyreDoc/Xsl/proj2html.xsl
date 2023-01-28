<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ***************************************************************
     VampyreDoc Tool
          
     XSL stylesheet which transforms VampyreDoc project XML file to
     XHTML page.

     **************************************************************** -->

<xsl:output method="xml" encoding="utf-8" indent="yes"
  doctype-public="-//W3C//DTD XHTML 1.0 Frameset//EN"
  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"/>
  
<xsl:template match="vampyredoc">
  <html>
  <!-- <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"> 
       this adds ugly empty xmlns="" attribute to many elements
       when transforming with Saxon -->
    <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <meta name="generator" content="VampyreDoc"/>
      <title>
        <xsl:choose>
          <xsl:when test="title!=''">
            <xsl:value-of select="title"/>        
          </xsl:when>
          <xsl:otherwise>
            VampyreDoc Project
          </xsl:otherwise>
        </xsl:choose>
      </title>
    </head> 
    <frameset cols="250,*" rows="1" border="0" toolbar="no">
      <frame frameborder="0" name="toc" scrolling="auto" noresize="noresize">
        <xsl:attribute name="src">
          <xsl:value-of select="toc"/>
        </xsl:attribute>
      </frame>
      <frame frameborder="0" name="docview" scrolling="auto" noresize="noresize">
        <xsl:attribute name="src">
          <xsl:value-of select="root"/>
        </xsl:attribute>
      </frame>
    </frameset> 
  </html>
</xsl:template>

</xsl:stylesheet> 

