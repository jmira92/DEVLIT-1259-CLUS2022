<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:cfg="http://tail-f.com/ns/config/1.0"
                xmlns:ncs="http://tail-f.com/ns/ncs">

  <xsl:output method="xml" omit-xml-declaration="no" indent="yes"/>
  <xsl:strip-space elements="*"/>

  <xsl:variable name="ncs" select="'http://tail-f.com/ns/ncs'"/>

  <xsl:template match="/">
    <!-- Wrap everything in a config node -->
    <cfg:config>
      <xsl:apply-templates select="*|comment()"/>
    </cfg:config>
  </xsl:template>

  <xsl:template match="ncs:ncs">
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>

  <xsl:template match="cfg:config">
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>


  <!-- /ncs/sm -> /services -->
  <xsl:template match="ncs:sm[parent::ncs:ncs]">
    <ncs:services>
      <xsl:apply-templates select="comment()|*[not(local-name() = 'java-vm-startup')]"/>
    </ncs:services>
    <xsl:apply-templates select="ncs:java-vm-startup"/>
  </xsl:template>

  <!-- /ncs/sm/cust-service -> /services/customer-service -->
  <xsl:template match="ncs:cust-service">
    <ncs:customer-service>
      <xsl:apply-templates select="*|comment()"/>
    </ncs:customer-service>
  </xsl:template>

  <!-- /ncs/sm/java-vm-startup /java-vm -->
  <xsl:template match="ncs:java-vm-startup[../../ncs:sm/../../ncs:ncs]">
    <ncs:java-vm>
      <xsl:apply-templates select="*|comment()"/>
    </ncs:java-vm>
  </xsl:template>

  <!-- /ncs/customers -> /customers -->
  <xsl:template match="ncs:customers[parent::ncs:ncs]">
    <ncs:customers>
      <xsl:apply-templates select="*|comment()"/>
    </ncs:customers>
  </xsl:template>

  <!--  /ncs/snmp-notification-receiver -> /snmp-notification-receiver -->
  <xsl:template match="ncs:snmp-notification-receiver[parent::ncs:ncs]">
    <ncs:snmp-notification-receiver>
      <xsl:apply-templates select="*|comment()"/>
    </ncs:snmp-notification-receiver>
  </xsl:template>
 

 <!-- /ncs/managed-device -> /devices/device -->
  <xsl:template match="ncs:managed-device[parent::ncs:ncs]">
    <xsl:element name="devices" namespace="{$ncs}">
      <xsl:element name="device" namespace="{$ncs}">
        <xsl:apply-templates select="*|comment()"/>
      </xsl:element>
    </xsl:element>

<!--
    <ncs:devices>
      <ncs:device>
        <xsl:apply-templates select="*|comment()"/>
      </ncs:device>
    </ncs:devices>
-->
  </xsl:template>

  <xsl:template match="ncs:*[../../ncs:ncs]">
    <ncs:devices>
      <xsl:element name="{local-name()}" namespace="{namespace-uri()}">
        <xsl:apply-templates select="*|comment()"/>
      </xsl:element>
    </ncs:devices>
  </xsl:template>


  <xsl:template match="*">
    <xsl:copy-of select="."/>
    <xsl:apply-templates select="*|comment()"/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>


  <xsl:template match="comment()">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>


</xsl:stylesheet>
                
