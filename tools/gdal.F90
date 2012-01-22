!    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>
!
!    This file is part of FortranGIS.
!
!    FortranGIS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation, either version 3 of the
!    License, or (at your option) any later version.
!
!    FortranGIS is distributed in the hope that it will be useful, but
!    WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public
!    License along with FortranGIS.  If not, see
!    <http://www.gnu.org/licenses/>.

!> Fortran 2003 interface to the gdal http://www.gdal.org/ library.
!! This module includes the interface to most of the public gdal C
!! API plus a more "Fortarnic" version of some functions.
!!
!! \ingroup libfortrangis
MODULE gdal
USE,INTRINSIC :: iso_c_binding
IMPLICIT NONE

! Hand made type definitions strictly reflecting C definitions
TYPE,BIND(C) :: gdal_gcp
  TYPE(c_ptr) :: pszid, pszinfo
  REAL(kind=c_double) :: dfGCPPixel, dfGCPLine, dfGCPX, dfGCPY, dfGCPZ
END TYPE gdal_gcp

TYPE,BIND(C) :: gdalrpcinfo
  REAL(kind=c_double) :: dfline_off, dfsamp_off, dflat_off, dflong_off, dfheight_off
  REAL(kind=c_double) :: dfline_scale, dfsamp_scale, dflat_scale, dflong_scale, dfheight_scale
  REAL(kind=c_double) :: adfline_num_coeff(20), adfline_den_coeff(20), &
   adfsamp_num_coeff(20), adfsamp_den_coeff(20)
  REAL(kind=c_double) :: dfmin_long, dfmin_lat, dfmax_long, dfmax_lat
END TYPE gdalrpcinfo

TYPE,BIND(C) :: gdalcolorentry
  INTEGER(kind=c_short) :: c1, c2, c3, c4
END TYPE gdalcolorentry

! Machine made type definitions
INCLUDE 'gdalproto_type.f90'

! Hand made interface definitions
INTERFACE
  FUNCTION gdalgcpstogeotransform(ngcpcount, pasgcps, padfgeotransform, bapproxok) &
   BIND(C,name='GDALGCPsToGeoTransform')
  IMPORT
  INTEGER(kind=c_int),VALUE :: ngcpcount
  TYPE(gdal_gcp),INTENT(in) :: pasgcps
  REAL(kind=c_double) :: padfgeotransform(*)
  INTEGER(kind=c_int),VALUE :: bapproxok
  INTEGER(kind=c_int) :: gdalgcpstogeotransform
  END FUNCTION gdalgcpstogeotransform
END INTERFACE

! Machine made interface definitions
INCLUDE 'gdalproto_interf.f90'

CONTAINS

! Fortran specific version of some functions
FUNCTION gdalgcpstogeotransform_f(pasgcps, padfgeotransform, bapproxok)
TYPE(gdal_gcp),INTENT(in) :: pasgcps(:)
REAL(kind=c_double),INTENT(out) :: padfgeotransform(6)
INTEGER(kind=c_int),VALUE :: bapproxok
INTEGER(kind=c_int) :: gdalgcpstogeotransform_f

gdalgcpstogeotransform_f = gdalgcpstogeotransform(SIZE(pasgcps), pasgcps(1), padfgeotransform, bapproxok)

END FUNCTION gdalgcpstogeotransform_f

END MODULE gdal
