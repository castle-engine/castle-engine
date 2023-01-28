<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ***************************************************************
     VampyreDoc Tool
          
     XSL stylesheet which transforms VampyreDoc project XML file to
     HTMLHelp hhp project.

     **************************************************************** -->

<xsl:output method="text" indent="yes"/> 

<xsl:template match="vampyredoc">
  <xsl:call-template name="options"/>
  <xsl:call-template name="files"/>
</xsl:template>

<xsl:template name="options">
[OPTIONS]
Title=<xsl:value-of select="title"/>  
Compiled file=..\<xsl:value-of select="output"/>.chm
Contents file=<xsl:value-of select="toc"/>
Default topic=<xsl:value-of select="root"/>
Compatibility=1.1 or later
Display compile progress=Yes
Full-text search=Yes
Auto Index=Yes
Flat=True
Binary Index=No
Binary TOC=No
</xsl:template>

<xsl:template name="files">
[FILES]
<xsl:value-of select="root"/><xsl:text>
</xsl:text>
<xsl:apply-templates select="producer/file"/>
</xsl:template>

<xsl:template match="producer/file">
<xsl:value-of select="."/><xsl:text>
</xsl:text>
</xsl:template>

</xsl:stylesheet> 

