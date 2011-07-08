lib_LTLIBRARIES = libfortrangis.la
libfortrangis_la_SOURCES = 
libfortrangis_la_LIBADD =
nodist_include_HEADERS = 

check_PROGRAMS =
TESTS = $(check_PROGRAMS)

if SHP
libfortrangis_la_SOURCES += shapelib.F90 shapelibint.c
libfortrangis_la_LIBADD += $(SHP_LIBS)
nodist_include_HEADERS += shapelib.mod

check_PROGRAMS += shapelib_test
shapelib_test_SOURCES = shapelib_test.f90
shapelib_test_LDADD = libfortrangis.la
endif

if PROJ
libfortrangis_la_SOURCES += proj.F90
libfortrangis_la_LIBADD += $(PROJ_LIBS)
nodist_include_HEADERS += proj.mod
endif

if GDAL
libfortrangis_la_SOURCES += gdal.F90 
libfortrangis_la_LIBADD += $(GDAL_LIBS)
nodist_include_HEADERS += gdal.mod
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
