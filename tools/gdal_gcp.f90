MODULE gdal_gcp_int
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

!INCLUDE "gdaltypes.f90"

TYPE,BIND(C) :: gdal_gcp
  TYPE(c_ptr) :: pszid, pszinfo
  REAL(kind=c_double) :: dfGCPPixel, dfGCPLine, dfGCPX, dfGCPY, dfGCPZ
END TYPE gdal_gcp

INTERFACE
  FUNCTION gdalgcpstogeotransform(ngcpcount, pasgcps, padfgeotransform, bapproxok) BIND(C,name='GDALGCPsToGeoTransform')
  IMPORT
  INTEGER(kind=c_int),VALUE :: ngcpcount
  TYPE(gdal_gcp),INTENT(in) :: pasgcps
  REAL(kind=c_double) :: padfgeotransform(*)
  INTEGER(kind=c_int),VALUE :: bapproxok
  INTEGER(kind=c_int) :: gdalgcpstogeotransform
  END FUNCTION gdalgcpstogeotransform
END INTERFACE

CONTAINS

FUNCTION gdalgcpstogeotransform_f(pasgcps, padfgeotransform, bapproxok)
TYPE(gdal_gcp),INTENT(in) :: pasgcps(:)
REAL(kind=c_double),INTENT(out) :: padfgeotransform(6)
INTEGER(kind=c_int),VALUE :: bapproxok
INTEGER(kind=c_int) :: gdalgcpstogeotransform_f

gdalgcpstogeotransform_f = gdalgcpstogeotransform(SIZE(pasgcps), pasgcps(1), padfgeotransform, bapproxok)

END FUNCTION gdalgcpstogeotransform_f

END MODULE gdal_gcp_int
