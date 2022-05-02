# tcsh/csh environment setup for NSO ncs-5.7.1
setenv NCS_DIR /Users/jomira/DEVV
setenv PATH $NCS_DIR/bin:${PATH}
if ($?DYLD_LIBRARY_PATH) then
    setenv DYLD_LIBRARY_PATH $NCS_DIR/lib:${DYLD_LIBRARY_PATH}
else
    setenv DYLD_LIBRARY_PATH $NCS_DIR/lib
endif
if ($?PYTHONPATH) then
    setenv PYTHONPATH $NCS_DIR/src/ncs/pyapi:${PYTHONPATH}
else
    setenv PYTHONPATH $NCS_DIR/src/ncs/pyapi
endif
