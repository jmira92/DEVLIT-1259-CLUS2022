all:
	mkdir nso-lab-rundir
	ncs-setup --dest ./nso-lab-rundir

clean:
	-rm -rf nso-lab-rundir

stop:
	-ncs --stop

showcase:
	./showcase.sh

showcase-rc:
	python3 showcase_rc.py
