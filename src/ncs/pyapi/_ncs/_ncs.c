/*
 * Copyright 2013 Tail-F Systems AB
 */

#include "confdpy_config.h"
#include "types.h"
#include "_cdb.h"
#include "_maapi.h"
#include "_dp.h"
#include "_events.h"
#include "_ha.h"
#include "_lib.h"
#include "_error.h"


#include <confd.h>


#include "confdpy_config.h"


static PyMethodDef ncs_Methods[] = {
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef moduledef = {
        PyModuleDef_HEAD_INIT,
        "_ncs_py3",
        NULL,
        0,
        ncs_Methods,
        NULL,
        NULL,
        NULL,
        NULL
};

static void add_ncs_submodule(PyObject *ncs, PyObject *module, char name[])
{
    if (module != NULL) {
        Py_INCREF(module);
        PyModule_AddObject(ncs, name, module);
    } else {
        PyErr_Format(PyExc_ImportError, "Failed to load _ncs.%s module",
                name);
    }
}


PyMODINIT_FUNC PyInit__ncs_py3(void)
{
    PyObject *m = NULL;

    if ((m = PyModule_Create(&moduledef)) == NULL) {
        goto error;
    }

    confd_init("_ncs_py3", fopen("/dev/null", "w"), CONFD_SILENT);

    /* add_ncs_submodule(m, init_types_module(), "types"); */
    add_ncs_submodule(m, init__cdb_module(), "cdb");
    add_ncs_submodule(m, init__maapi_module(), "maapi");
    add_ncs_submodule(m, init__dp_module(), "dp");
    add_ncs_submodule(m, init__events_module(), "events");
    add_ncs_submodule(m, init__ha_module(), "ha");
    add_ncs_submodule(m, init__lib_module(), "lib");
    add_ncs_submodule(m, init__error_module(), "error");

error:
    if (PyErr_Occurred()) {
        PyErr_SetString(PyExc_ImportError, "_ncs : Initialization failed");
    }

    return m;
}
