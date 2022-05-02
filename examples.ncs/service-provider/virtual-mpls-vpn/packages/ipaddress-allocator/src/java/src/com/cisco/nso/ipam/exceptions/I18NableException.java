package com.cisco.nso.ipam.exceptions;

public interface I18NableException {

        void setEtype(String etype);

        String getEtype();

        void setEparms(String [] parms);

        String [] getEparms();

        void setOriginalMessage(String msg);

        String getOriginalMessage();
}
