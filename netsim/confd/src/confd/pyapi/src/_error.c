/*
 * Copyright 2013 Tail-F Systems AB
 */

// include first, order is significant to get defines correct
#include "confdpy_config.h"

#include <confd.h>

#include "_error.h"


PyObject *ConfdError; /* pointer to confd.Error exception object */
PyObject *ConfdEOF;   /* pointer to confd.EOF exception object */

PyObject* confdPyConfdError(void)
{
    // Delete (potentially) old attributes, ignore error

    PyObject_DelAttrString(ConfdError, "confd_lasterr");
    PyObject_DelAttrString(ConfdError, "confd_errno");
    PyObject_DelAttrString(ConfdError, "errno");
    PyObject_DelAttrString(ConfdError, "confd_strerror");

    const char* cfd_lasterr = confd_lasterr();
    const char* cfd_strerror = confd_strerror(confd_errno);

    PyObject_SetAttrString(ConfdError, "confd_lasterr",
                           PyString_FromString(cfd_lasterr));

    PyObject_SetAttrString(ConfdError, "confd_errno",
                           PyInt_FromLong(confd_errno));

    if (confd_errno == CONFD_ERR_OS) {
        PyObject_SetAttrString(ConfdError, "errno",
                               PyInt_FromLong(errno));
    }

    PyObject_SetAttrString(ConfdError, "confd_strerror",
                           PyString_FromString(cfd_strerror));

    return PyErr_Format(ConfdError, "%s (%d): %s",
                        cfd_strerror, confd_errno, cfd_lasterr);
 }

PyObject* confdPyEofError(void)
{
    return PyErr_Format(ConfdEOF,
                        "%s closed connection", CONFD_PY_PRODUCT);
}


PyObject* confdPyNotImplementedError(void)
{
    return PyErr_Format(ConfdError, "Function not yet implemented");
}

PyObject* confdPyDeprecatedFunctionError(void)
{
    return PyErr_Format(ConfdError, "Function deprecated");
}


#include "../doc/src/error_doc.c"

#define MODULE CONFD_PY_MODULE ".error"

static struct PyModuleDef moduledef = {
        PyModuleDef_HEAD_INIT,
        MODULE,
        ERROR_MODULE_DOCSTR(CONFD_PY_PRODUCT),
        0,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL
};

PyObject* init__error_module(void)
{
    PyObject *m = NULL;

    if ((m = PyModule_Create(&moduledef)) == NULL) {
        goto error;
    }

    if ((ConfdError = PyErr_NewExceptionWithDoc(CONFD_PY_MODULE ".error.Error",
                    _error_Error__doc__, NULL, NULL)) == NULL) goto error;
    PyModule_AddObject(m, "Error", ConfdError);


    if ((ConfdEOF = PyErr_NewExceptionWithDoc(CONFD_PY_MODULE ".error.EOF",
                    _error_EOF__doc__, NULL, NULL)) == NULL) goto error;
    PyModule_AddObject(m, "EOF", ConfdEOF);

error:
    if (PyErr_Occurred()) {
        PyErr_SetString(PyExc_ImportError, MODULE ": init failed");
        return NULL;
    } else {
        return m;
    }
}


/**
 *
 * Check if a format string is 'python valid', i.e. safe to use
 * togheter with printf-like functions that expects parameters.
 *
 * If not safe, i.e. doesn't contain %-variants NULL is returned and
 * a corresponding Python TypeError is set.
 *
 */
const char *PyConfd_NormalizeFormatString(const char *fmt)
{
    static const char *invalid[] = {"%d", "%u", "%s", "%x", "%h", "%*x",
                                    "%ip4", "%ip6", NULL};
    const char *tmp, **p;

    if ((tmp = strchr(fmt, '%')) != NULL) {
        for (p = invalid; *p != NULL; ++p) {
            if (strstr(tmp, *p)) {
                PyErr_Format(PyExc_ValueError,
                            "Path can't contain a %s character", *p);
                return NULL;
            }
        }
    }

    return fmt;
}
