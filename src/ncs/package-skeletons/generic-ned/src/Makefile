all: javac
.PHONY: all

# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk

JAVA_PACKAGE = %JAVAPACKAGE%
JDIR = $(shell echo $(JAVA_PACKAGE) | sed 's/\./\//g')

NS  = namespaces

JFLAGS = --java-disable-prefix \
         --exclude-enums \
         --fail-on-warnings \
         --java-package $(JAVA_PACKAGE).$(NS) \
         --emit-java java/src/$(JDIR)/namespaces

SRC = $(wildcard yang/*.yang)
NFXS = ncsc-out/modules/fxs
FXS = $(SRC:yang/%.yang=$(NFXS)/%.fxs)
DIRS = ncsc-out ../load-dir ../shared-jar ../private-jar \
       java/src/$(JDIR)/$(NS)
NED_ID_ARG = $(shell [ -x ${NCS_DIR}/support/ned-ncs-ned-id-arg ] && \
               ${NCS_DIR}/support/ned-ncs-ned-id-arg package-meta-data.xml.in)

fxs: $(DIRS) ../package-meta-data.xml $(FXS)
.PHONY: fxs

$(DIRS):
	mkdir -p $@

../package-meta-data.xml: package-meta-data.xml.in
	rm -rf $@
	if [ -x ${NCS_DIR}/support/ned-make-package-meta-data ]; then \
	    ${NCS_DIR}/support/ned-make-package-meta-data $<;         \
	else                                                          \
	    cp package-meta-data.xml.in ../package-meta-data.xml;     \
	fi
	chmod -w $@

$(NFXS)/%YANGNAME%.fxs: yang/%YANGNAME%.yang
	$(NCSC) --ncs-compile-module $<       \
		--ncs-device-dir ncsc-out     \
		--ncs-skip-statistics         \
		--ncs-device-type generic-ned \
		--fail-on-warnings \
		$(NED_ID_ARG) && cp $@ ../load-dir
	$(NCSC) $(JFLAGS)/%NAME%.java $@

$(NFXS)/%YANGNAME%-stats.fxs: yang/%YANGNAME%-stats.yang
	$(NCSC) --ncs-compile-module $<       \
		--ncs-device-dir ncsc-out     \
		--ncs-skip-template           \
		--ncs-device-type generic-ned \
		--fail-on-warnings \
		$(NED_ID_ARG) && cp $@ ../load-dir
	$(NCSC) $(JFLAGS)/%NAME%Stats.java $@

../load-dir/%.fxs: %.yang
	$(NCSC) -c $< -o $@

javac: fxs
	cd java && ant -q all
.PHONY: javac

clean:
	rm -rf $(DIRS)
	rm -rf ../package-meta-data.xml
	cd java && ant -q clean || true
.PHONY: clean
