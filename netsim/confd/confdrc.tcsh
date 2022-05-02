# tcsh/csh environment setup for ConfD confd-7.7.1
setenv CONFD_DIR /Users/jomira/DEVV/netsim/confd
setenv PATH $CONFD_DIR/bin:${PATH}
if ($?DYLD_LIBRARY_PATH) then
    setenv DYLD_LIBRARY_PATH $CONFD_DIR/lib:${DYLD_LIBRARY_PATH}
else
    setenv DYLD_LIBRARY_PATH $CONFD_DIR/lib
endif
if ($?PYTHONPATH) then
    setenv PYTHONPATH $CONFD_DIR/src/confd/pyapi:${PYTHONPATH}
else
    setenv PYTHONPATH $CONFD_DIR/src/confd/pyapi
endif
