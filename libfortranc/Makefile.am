lib_LTLIBRARIES = libfortranc.la
libfortranc_la_SOURCES = fortranc.F90
nodist_include_HEADERS = fortranc.mod

if VERSIONINFO
libfortranc_la_LDFLAGS = -version-info @VERSIONLIB@
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
