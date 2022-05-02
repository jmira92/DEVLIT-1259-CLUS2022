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
DIRS = ncsc-out ../load-dir ../shared-jar ../private-jar \
       java/src/$(JDIR)/$(NS)
FXS = $(SRC:yang/%.yang=../load-dir/%.fxs)

fxs: $(DIRS) $(FXS)
.PHONY: fxs

$(DIRS):
	mkdir -p $@

../load-dir/%.fxs: yang/%.yang
	$(NCSC) `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
		--fail-on-warnings \
		$(YANGPATH) \
		-c -o $@ $<
	$(NCSC) $(JFLAGS)/$*.java $@

javac: fxs
	cd java && ant -q all
.PHONY: javac

clean:
	rm -rf $(DIRS)
	cd java && ant -q clean || true
.PHONY: clean
