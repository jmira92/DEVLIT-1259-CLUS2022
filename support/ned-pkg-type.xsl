<?xml version="1.0"?>
<!-- Return the package type based on content in
     the package meta data file
-->
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:pkg="http://tail-f.com/ns/ncs-packages">
  <xsl:output method='text'/>

  <xsl:template match="/pkg:ncs-package">
    <xsl:for-each select="pkg:component/pkg:ned">
      <xsl:if test="pkg:netconf">
        <xsl:text>netconf</xsl:text>
      </xsl:if>
      <xsl:if test="pkg:cli">
        <xsl:text>cli</xsl:text>
      </xsl:if>
      <xsl:if test="pkg:generic">
        <xsl:text>generic</xsl:text>
      </xsl:if>
      <xsl:if test="pkg:snmp">
        <xsl:text>snmp</xsl:text>
      </xsl:if>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
