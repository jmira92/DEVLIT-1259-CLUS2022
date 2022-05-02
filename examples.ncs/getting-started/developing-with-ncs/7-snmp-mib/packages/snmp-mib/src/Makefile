

JAVA_PACKAGE = com.example.mymib
JDIR = $(shell echo $(JAVA_PACKAGE) | sed 's/\./\//g')

NS  = namespaces
NCS_FXS_DIR = $(NCS_DIR)/etc/ncs

JFLAGS = --java-disable-prefix \
         --exclude-enums \
         --fail-on-warnings \
         --java-package $(JAVA_PACKAGE).$(NS) \
         --emit-java java/src/$(JDIR)/namespaces

all:	fxs ../load-dir/TAIL-F-TEST-MIB.bin javac

# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk

%.yang: %.mib
	$(NCSC) --mib2yang $< > $@

../load-dir/TAIL-F-TEST-MIB.bin: TAIL-F-TEST-MIB.mib TAIL-F-TEST-MIB.miba
	$(NCSC) -c TAIL-F-TEST-MIB.mib -f $(NCS_FXS_DIR) \
          --mib-annotation TAIL-F-TEST-MIB.miba ../load-dir/simple.fxs && \
 	mv TAIL-F-TEST-MIB.bin ../load-dir

SRC = $(wildcard *.yang)
FXS = $(SRC:%.yang=../load-dir/%.fxs)

fxs:	$(FXS)

../load-dir/%.fxs:	%.yang
	$(NCSC)  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
             $(YANGPATH) -c -o $@ $<
	$(NCSC) $(JFLAGS)/$*.java $@ 

javac:
	cd java && ant -q all

clean:
	rm -rf  *.fxs *.bin ../load-dir/*
	rm -f ../*jar/*.jar
	cd java && ant -q clean || true
	rm -f java/src/$(JDIR)/$(NS)/*.java

