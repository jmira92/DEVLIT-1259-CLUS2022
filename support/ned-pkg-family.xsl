<?xml version="1.0"?>
<!-- Return the package familyname based on content in
     the package meta data in file
-->
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:pkg="http://tail-f.com/ns/ncs-packages">
  <xsl:output method='text'/>

  <xsl:param name="VERSION" select="/pkg:ncs-package/pkg:package-version"/>

  <xsl:template match="/pkg:ncs-package">
    <xsl:value-of select="pkg:name"/>
    <xsl:text>-</xsl:text>
    <xsl:if test="pkg:component/pkg:ned/pkg:netconf">
      <xsl:text>nc</xsl:text>
    </xsl:if>
    <xsl:if test="pkg:component/pkg:ned/pkg:cli">
      <xsl:text>cli</xsl:text>
    </xsl:if>
    <xsl:if test="pkg:component/pkg:ned/pkg:generic">
      <xsl:text>gen</xsl:text>
    </xsl:if>
    <xsl:if test="pkg:component/pkg:ned/pkg:snmp">
      <xsl:text>snmp</xsl:text>
    </xsl:if>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
