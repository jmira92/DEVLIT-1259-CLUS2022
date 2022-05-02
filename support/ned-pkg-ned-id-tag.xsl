<?xml version="1.0"?>
<!-- Return the package type based on content in
     the package meta data file
-->
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:pkg="http://tail-f.com/ns/ncs-packages">
  <xsl:output
      omit-xml-declaration="yes"
      method='xml'
      indent='no'
      />

  <xsl:template match="/pkg:ncs-package">
    <xsl:for-each select="pkg:component/pkg:ned/*/pkg:ned-id">
      <xsl:copy-of select="."/>
      <xsl:text>&#10;</xsl:text>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
