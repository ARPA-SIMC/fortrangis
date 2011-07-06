lib_LTLIBRARIES = libfortrangis.la

libfortrangis_la_SOURCES = 

if SHP
libfortrangis_la_SOURCES += shapelib.F90
endif

if PROJ
libfortrangis_la_SOURCES += proj.F90
endif

if GDAL
libfortrangis_la_SOURCES += gdal.F90 
endif

nodist_include_HEADERS = 

if SHP
nodist_include_HEADERS += shapelib.mod
endif

if PROJ
nodist_include_HEADERS += proj.mod
endif

if GDAL
nodist_include_HEADERS += gdal.mod
endif

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
