
all: tailf-aaa.fxs

old_aaa: aaa_cdb@2011-09-22.fxs

tailf-aaa.fxs: tailf-aaa.yang
	ncsc -c -o $@ $<

aaa_cdb@2011-09-22.fxs: tailf-aaa@2011-09-22.yang
	ncsc -c -o $@ $<

clean:
	-rm *.fxs
