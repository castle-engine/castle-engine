<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- ***************************************************************
     VampyreDoc Tool
          
     Documentation XSL stylesheet which transforms VampyreDoc to
     XHTML page (in Borland Dev Studio help style).

     Note: these <xsl:text> </xsl:text> are at the ends of inline 
     elements are here because of MS IE. It does not put spaces
     between e.g. <icode>umajo</icode> <b>boldly umajo</b>
     even there is space or linebreak in xml file.

     **************************************************************** -->

<xsl:output method="xml" encoding="utf-8" indent="no" 
  doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>  

<xsl:template match="doc">
  <html>
   <!-- <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"> 
       this adds ugly empty xmlns="" attribute to many elements
       when transforming with Saxon -->
    <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8" />
      <meta name="generator" content="VampyreDoc" />
      <link href="../common/doc.css" type="text/css" rel="stylesheet" /> 
      <link href="../../common/doc.css" type="text/css" rel="stylesheet" /> 
      <link href="../../../common/doc.css" type="text/css" rel="stylesheet" /> 
      <link href="doc.css" type="text/css" rel="stylesheet" />  
      <xsl:call-template name="doctitle" />
    </head>  
    <body>
      <xsl:apply-templates select="chapter" />
      <br />
      <xsl:call-template name="pagefooter" />
      <br />
    </body>
  </html>
</xsl:template>

<!-- *********************************** 
                  Specials
     *********************************** -->

<xsl:template match="chapter">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="section">
  <xsl:apply-templates />
</xsl:template>

<!-- *********************************** 
             Headers & Captions
     *********************************** -->

<xsl:template name="doctitle">
  <title>
    <xsl:choose>
      <xsl:when test="title!=''">
        <xsl:value-of select="title"/>        
      </xsl:when>
      <xsl:otherwise>
        Documentation
      </xsl:otherwise>
    </xsl:choose>
  </title>
</xsl:template>

<xsl:template match="title">
  <span class="title">
    <xsl:value-of select="."/>
  </span>
</xsl:template>

<xsl:template match="lcap|largecap">
  <span class="subtopic1">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="mcap|mediumcap">
  <span class="subtopic2">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<xsl:template match="scap|smallcap">
  <span class="caption">
    <xsl:apply-templates/>
  </span>
</xsl:template>

<!-- *********************************** 
                   Lists
     *********************************** -->

<xsl:template match="list">
  <ul class="list">
    <xsl:apply-templates select="li" />
  </ul>
</xsl:template>

<xsl:template match="linklist|lslist">
  <div class="linklist">
    <div class="s">
      <xsl:value-of select="title"/>
    </div>
    <xsl:apply-templates select="listedlink|lslink" mode="inlist"/>   
  </div>
</xsl:template>

<xsl:template match="bulletlist|blist">
  <ul class="bullet">
    <xsl:apply-templates select="li" />
  </ul>
</xsl:template>

<xsl:template match="orderedlist|olist">
  <ol class="orderedlist">
    <xsl:apply-templates select="li"/>
  </ol>
</xsl:template>

<xsl:template match="li">
  <li class="li">
    <xsl:apply-templates />
  </li>
</xsl:template>

<!-- *********************************** 
              Text Formatting
     *********************************** -->

<xsl:template match="b|bold">
  <b>
    <xsl:apply-templates />
  </b>
</xsl:template>

<xsl:template match="u|under">
  <u>
    <xsl:apply-templates />
  </u>
</xsl:template>

<xsl:template match="i|italic">
  <i>
    <xsl:apply-templates />
  </i>
</xsl:template>

<xsl:template match="center">
  <center>
    <xsl:apply-templates />
  </center>
</xsl:template>

<xsl:template match="linebreak|br">
  <br />
</xsl:template>

<xsl:template match="par|para">
  <p class="para">
    <xsl:apply-templates />
  </p>
</xsl:template>

<xsl:template match="code">
  <pre class="syntax">
    <xsl:apply-templates />
  </pre>
</xsl:template>

<xsl:template match="icode">
  <code class="codeinline">
    <xsl:apply-templates />  
  </code>
</xsl:template>

<xsl:template match="keyword">
  <code class="keyword">
    <xsl:apply-templates />
  </code>
</xsl:template>

<xsl:template match="tip">
  <div class="tip">
    Tip:
    <span class="ntwpara">
      <xsl:apply-templates />
    </span>  
  </div>
</xsl:template>

<xsl:template match="warning|warn">
  <div class="warning">
    Warning:
    <span class="ntwpara">
      <xsl:apply-templates />
    </span>  
  </div>
</xsl:template>

<xsl:template match="note">
  <div class="note">
    Note:
    <span class="ntwpara">
      <xsl:apply-templates />
    </span>
  </div>
</xsl:template>

<xsl:template match="menuitem|mitem">
  <span class="menuitem">
    <xsl:apply-templates />
  </span>
</xsl:template>

<!-- *********************************** 
                 Links
     *********************************** -->

<xsl:template match="link">
  <span class="link">
    <a>
      <xsl:attribute name="href">
        <xsl:if test="@url!=''">
          <xsl:value-of select="@url"/>
        </xsl:if>
      </xsl:attribute>
      <xsl:apply-templates />
    </a>
  </span>
</xsl:template>

<xsl:template match="listedlink|lslink">
  <span class="listedlink">
    <a>
      <xsl:attribute name="href">
        <xsl:if test="@url!=''">
          <xsl:value-of select="@url"/>
        </xsl:if>
      </xsl:attribute>
      <xsl:apply-templates />
    </a>
  </span>
</xsl:template>

<xsl:template match="listedlink|lslink" mode="inlist">
  <div class="linklistitem">
    <xsl:apply-templates select="."/>  
  </div>    
</xsl:template>

<xsl:template match="anchor">
  <a>
    <xsl:attribute name="id">
       <xsl:value-of select="@name"/>
    </xsl:attribute>
    <xsl:apply-templates />
  </a>    
</xsl:template>

<xsl:template match="ref">
  <span class="codeinline">
    <span class="link">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="@url"/>
        </xsl:attribute>
        <xsl:apply-templates />
      </a>
    </span>  
  </span>
</xsl:template>

<!-- ***********************************
                 Images
     *********************************** -->

<xsl:template match="image|img">
  <img class="imageblock">
    <xsl:attribute name="src">
      <xsl:value-of select="@url"/>
    </xsl:attribute>
    <xsl:attribute name="alt">
      <xsl:value-of select="@desc"/>
    </xsl:attribute>
  </img>
</xsl:template>

<xsl:template match="inlineimage|inimg">
  <img class="imageinline">
    <xsl:attribute name="src">
      <xsl:value-of select="@url"/>
    </xsl:attribute>
     <xsl:attribute name="alt">
      <xsl:value-of select="@desc"/>
    </xsl:attribute>
  </img>
</xsl:template>

<!-- ***********************************
                 Tables
     *********************************** -->

<xsl:template match="table">
  <xsl:if test="title!=''">
    <span class="tabletitle">
      <xsl:value-of select="title"/>
    </span>
  </xsl:if>    
  <table class="table">
    <xsl:apply-templates select="row|tr"/>
  </table>
</xsl:template>

<xsl:template match="row|tr">
  <tr class="tr">
    <xsl:apply-templates select="cell|th|header|td"/>
  </tr>
</xsl:template>

<xsl:template match="cell|td">
  <td class="td">
    <xsl:if test="@colspan!=''">
      <xsl:attribute name="colspan">
        <xsl:value-of select="@colspan"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@rowspan!=''">
      <xsl:attribute name="rowspan">
        <xsl:value-of select="@rowspan"/>
      </xsl:attribute>
    </xsl:if>
    <span class="tablepara">
      <xsl:apply-templates />
    </span>
  </td>
</xsl:template>

<xsl:template match="header|th">
  <th class="th">
    <xsl:apply-templates />
  </th>
</xsl:template>

<!-- ***********************************
                 Page Footer
     *********************************** -->

<xsl:template name="pagefooter">
  <br /><br />
  <div class="footer">
    <xsl:text>
      Vampyre Imaging Library (Documentation for version 0.26.4)
    </xsl:text>
    <br />
    <a href="http://imaginglib.sourceforge.net" target="_blank">
    http://imaginglib.sourceforge.net</a>
    <br />
  </div>
</xsl:template>

</xsl:stylesheet>
