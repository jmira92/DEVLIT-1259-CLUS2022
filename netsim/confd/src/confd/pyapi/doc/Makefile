CONFD_MODULES = \
		_confd \
		_confd.cdb \
		_confd.dp \
		_confd.error \
		_confd.events \
		_confd.ha \
		_confd.maapi \
		confd \
		confd.cdb \
		confd.dp \
		confd.events \
		confd.error \
		confd.experimental \
		confd.ha \
		confd.log \
		confd.maagic \
		confd.maapi

NCS_MODULES = \
		_ncs \
		_ncs.cdb \
		_ncs.dp \
		_ncs.error \
		_ncs.events \
		_ncs.ha \
		_ncs.maapi \
		ncs \
		ncs.application \
		ncs.cdb \
		ncs.dp \
		ncs.events \
		ncs.error \
		ncs.experimental \
		ncs.ha \
		ncs.log \
		ncs.maagic \
		ncs.maapi \
		ncs.template

ifeq ($(PYTHON),)
	PYTHON = $(shell which python3)
endif
ifeq ($(shell $(PYTHON) -c 'import pdoc' 2>/dev/null || echo false),false)
	USE_VENV = true
	PDOC_PYTHON = build-venv/bin/python
else
	PDOC_PYTHON = $(PYTHON)
endif

.PHONY: confd ncs confd-pdoc ncs-pdoc

ifeq ($(WITHOUT_PDOC),)
all: confd-pdoc ncs-pdoc
else
all: confd ncs
endif

confd:
	@rm -rf $@
	@mkdir -p $@
	@cd $@ && for m in $(CONFD_MODULES); do $(PYTHON) -m pydoc -w $$m; done
	@$(MAKE) confd/index.html

confd/index.html:
	@echo "creating $@"
	@rm -f $@
	@mkdir -p `dirname $@`
	@sh ./index.sh ConfD $(CONFD_MODULES) > $@

ncs:
	@rm -rf $@
	@mkdir -p $@
	@cd $@ && for m in $(NCS_MODULES); do $(PYTHON) -m pydoc -w $$m; done
	@$(MAKE) ncs/index.html

ncs/index.html:
	@echo "creating $@"
	@rm -f $@
	@mkdir -p `dirname $@`
	@sh ./index.sh NCS $(NCS_MODULES) > $@

confd-pdoc: custom_pdoc/__init__.py
	@rm -rf confd
	$(PDOC_PYTHON) generate.py -- confd _confd

ncs-pdoc: custom_pdoc/__init__.py
	@rm -rf ncs
	$(PDOC_PYTHON) generate.py -- ncs _ncs

custom_pdoc/__init__.py: custom_pdoc/init.patch
ifeq ($(USE_VENV),true)
	@echo "Python pdoc3 module not found, installing it in build-venv/"
	$(PYTHON) -m venv build-venv
	build-venv/bin/pip install -r custom_pdoc/requirements.txt
endif
	cp -f `$(PDOC_PYTHON) -c 'import pdoc; print(pdoc.__file__)'` \
		custom_pdoc/
	patch custom_pdoc/__init__.py < custom_pdoc/init.patch


clean:
	@rm -rf confd ncs custom_pdoc/__init__.py build-venv
