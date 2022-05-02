<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:pkg="http://tail-f.com/ns/ncs-packages"
                xmlns:id="http://tail-f.com/ns/ned-id"
                xmlns:ext="http://exslt.org/common"
                exclude-result-prefixes="ext">
  <xsl:strip-space  elements="*"/>
  <xsl:output indent="yes"/>

  <xsl:param name="pUrl"
             select="concat('http://tail-f.com/ns/ned-id/',$NED_ID)"/>

  <xsl:variable name="vrtfDummy">
    <xsl:element name="id:dummy" namespace="{$pUrl}"/>
  </xsl:variable>

  <xsl:variable name="NED_ID_NS"
                select="ext:node-set($vrtfDummy)/*/namespace::id"/>

  <xsl:param name="VERSION" select="/pkg:ncs-package/pkg:package-version"/>

  <xsl:variable name="NED_ID">
    <xsl:element name="dummy" namespace="http://fake">
      <xsl:value-of select="/pkg:ncs-package/pkg:name"/>
      <xsl:text>-</xsl:text>

      <xsl:if test="/pkg:ncs-package/pkg:component/pkg:ned/pkg:netconf">
        <xsl:text>nc</xsl:text>
      </xsl:if>
      <xsl:if test="/pkg:ncs-package/pkg:component/pkg:ned/pkg:cli">
        <xsl:text>cli</xsl:text>
      </xsl:if>
      <xsl:if test="/pkg:ncs-package/pkg:component/pkg:ned/pkg:generic">
        <xsl:text>gen</xsl:text>
      </xsl:if>
      <xsl:if test="/pkg:ncs-package/pkg:component/pkg:ned/pkg:snmp">
        <xsl:text>snmp</xsl:text>
      </xsl:if>
      <xsl:text>-</xsl:text>

      <xsl:value-of select="substring-before($VERSION, '.')"/>
      <xsl:text>.</xsl:text>
      <xsl:value-of
          select=
          "substring-before(concat(substring-after($VERSION,'.'),'.'),'.')"/>
    </xsl:element>
  </xsl:variable>

  <xsl:template match="@* | node()">
    <xsl:if test="local-name()='ncs-package'">
      <xsl:comment> This file has been generated. Do NOT edit it.
     For changes edit the original instead.
</xsl:comment>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/pkg:ncs-package/pkg:name">
    <xsl:copy>
      <xsl:value-of select="$NED_ID"/>
    </xsl:copy>
  </xsl:template>

  <!-- Before CDM there was no ned-id for netconf -->
  <xsl:template
      match="/pkg:ncs-package/pkg:component/pkg:ned/pkg:netconf">
    <xsl:copy>
      <xsl:element
          name="ned-id"
          namespace="http://tail-f.com/ns/ncs-packages">
        <xsl:copy-of select="$NED_ID_NS"/>
        <xsl:text>id:</xsl:text>
        <xsl:value-of select="$NED_ID"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>

  <!-- Before CDM there was no ned-id for snmp -->
  <xsl:template
      match="/pkg:ncs-package/pkg:component/pkg:ned/pkg:snmp">
    <xsl:copy>
      <xsl:element
          name="ned-id"
          namespace="http://tail-f.com/ns/ncs-packages">
        <xsl:copy-of select="$NED_ID_NS"/>
        <xsl:text>id:</xsl:text>
        <xsl:value-of select="$NED_ID"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>

  <xsl:template
      match="/pkg:ncs-package/pkg:component/pkg:ned/*/pkg:ned-id">
    <xsl:element
        name="ned-id"
        namespace="http://tail-f.com/ns/ncs-packages">
      <xsl:copy-of select="$NED_ID_NS"/>
      <xsl:text>id:</xsl:text>
      <xsl:value-of select="$NED_ID"/>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
