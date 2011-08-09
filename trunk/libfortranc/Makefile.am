lib_LTLIBRARIES = libfortranc.la
libfortranc_la_SOURCES = fortranc.F90
nodist_include_HEADERS = fortranc.mod

if VERSIONINFO
libfortranc_la_LDFLAGS = -version-info @VERSIONLIB@
endif

TESTS = $(check_PROGRAMS)
check_PROGRAMS = fortranc_test

fortranc_test_SOURCES = fortranc_test.f90 return_charpp.c
fortranc_test_LDADD = libfortranc.la


mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
