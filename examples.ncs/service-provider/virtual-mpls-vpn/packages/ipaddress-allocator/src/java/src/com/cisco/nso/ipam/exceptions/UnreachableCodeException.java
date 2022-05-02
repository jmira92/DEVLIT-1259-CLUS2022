package com.cisco.nso.ipam.exceptions;

/**
 * Thrown when control falls into an area that was supposed to be
 * unreachable.
 */
public class UnreachableCodeException extends RuntimeException {
    private static final long serialVersionUID = -6922687511826244341L;

    public UnreachableCodeException(String why) {
        fillInStackTrace();
    }

    public UnreachableCodeException() {
        fillInStackTrace();
    }
}
