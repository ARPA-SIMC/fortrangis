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

! Hand made symbolic constant definitions
! GDALDataType
INTEGER(kind=c_int),PARAMETER :: GDT_Unknown = 0 !< constant defining the native data type of a dataset data: unknown
INTEGER(kind=c_int),PARAMETER :: GDT_Byte = 1 !< byte, in Fortran it can be declared as \a INTEGER(kind=C_INT_8_T)
INTEGER(kind=c_int),PARAMETER :: GDT_UInt16 = 2 !< unsigned 16 bit integer, it should be avoided in Fortran and translated into a signed type
INTEGER(kind=c_int),PARAMETER :: GDT_Int16 = 3 !< signed 16 bit integer, in Fortran it can be declared as \a INTEGER(kind=C_INT_16_T)
INTEGER(kind=c_int),PARAMETER :: GDT_UInt32 = 4 !< unsigned 32 bit integer, it should be avoided in Fortran and translated into a signed type
INTEGER(kind=c_int),PARAMETER :: GDT_Int32 = 5  !< signed 32 bit integer, in Fortran it can be declared as \a INTEGER(kind=C_INT)
INTEGER(kind=c_int),PARAMETER :: GDT_Float32 = 6 !< 32 bit floating point real, in Fortran it can be declared as \a REAL(kind=C_FLOAT)
INTEGER(kind=c_int),PARAMETER :: GDT_Float64 = 7 !< 64 bit floating point real, in Fortran it can be declared as \a REAL(kind=C_DOUBLE)
INTEGER(kind=c_int),PARAMETER :: GDT_CInt16 = 8 !< 16 bit integer complex, it should be avoided in Fortran and translated into a floating point type
INTEGER(kind=c_int),PARAMETER :: GDT_CInt32 = 9 !< 32 bit integer complex, it should be avoided in Fortran and translated into a floating point type
INTEGER(kind=c_int),PARAMETER :: GDT_CFloat32 = 10 !< 32 bit (*2) floating point complex, in Fortran it can be declared as \a COMPLEX(kind=C_FLOAT_COMPLEX)
INTEGER(kind=c_int),PARAMETER :: GDT_CFloat64 = 11 !< 64 bit (*2) floating point complex, in Fortran it can be declared as \a COMPLEX(kind=C_DOUBLE_COMPLEX)
INTEGER(kind=c_int),PARAMETER :: GDT_TypeCount = 12

! GDALAccess
INTEGER(kind=c_int),PARAMETER :: GA_ReadOnly = 0 !< access type for opening a file: read only
INTEGER(kind=c_int),PARAMETER :: GA_Update = 1 !< update, currently unsupported in Fortran

! GDALRWFlag
INTEGER(kind=c_int),PARAMETER :: GF_Read = 0 !< operation to be performed on a dataset: read
INTEGER(kind=c_int),PARAMETER :: GF_Write = 1 !< write, currently unsupported in Fortran

INTEGER(kind=c_int),PARAMETER :: & ! GDALColorInterp
 GCI_Undefined = 0, GCI_GrayIndex = 1, GCI_PaletteIndex = 2, &
 GCI_RedBand = 3, GCI_GreenBand = 4, GCI_BlueBand = 5, &
 GCI_AlphaBand = 6, GCI_HueBand = 7, GCI_SaturationBand = 8, &
 GCI_LightnessBand = 9, GCI_CyanBand = 10, GCI_MagentaBand = 11, &
 GCI_YellowBand = 12, GCI_BlackBand = 13, GCI_YCbCr_YBand = 14, &
 GCI_YCbCr_CbBand = 15,GCI_YCbCr_CrBand = 16, GCI_Max = 16

INTEGER(kind=c_int),PARAMETER :: & ! GDALPaletteInterp
 GPI_Gray = 0, GPI_RGB = 1, GPI_CMYK = 2, GPI_HLS = 3

INTEGER(kind=c_int),PARAMETER :: & ! GDALRATFieldType
 GFT_Integer = 0, GFT_Real = 1, GFT_String = 2

INTEGER(kind=c_int),PARAMETER :: & ! GDALRATFieldUsage
 GFU_Generic = 0, GFU_PixelCount = 1, GFU_Name = 2, GFU_Min = 3, &
 GFU_Max = 4, GFU_MinMax = 5, GFU_Red = 6, GFU_Green = 7, &
 GFU_Blue = 8, GFU_Alpha = 9, GFU_RedMin = 10, GFU_GreenMin = 11, &
 GFU_BlueMin = 12, GFU_AlphaMin = 13, GFU_RedMax = 14, &
 GFU_GreenMax = 15, GFU_BlueMax = 16, GFU_AlphaMax = 17, GFU_MaxCount = 18

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

! Fortran style interfaces
!> Simplified Fortran generic interface to the gdalrasterio C function.
INTERFACE gdalrasterio_f
  MODULE PROCEDURE gdalrasterio_int8, gdalrasterio_int16, gdalrasterio_int32, &
   gdalrasterio_float, gdalrasterio_double, &
   gdalrasterio_float_cmplx, gdalrasterio_double_cmplx
END INTERFACE
PRIVATE gdalrasterio_int8, gdalrasterio_int16, gdalrasterio_int32, &
 gdalrasterio_float, gdalrasterio_double, &
 gdalrasterio_float_cmplx, gdalrasterio_double_cmplx

! internal interface
INTERFACE gdalrasterio_loc
  MODULE PROCEDURE gdalrasterio_int8_loc, gdalrasterio_int16_loc, gdalrasterio_int32_loc, &
   gdalrasterio_float_loc, gdalrasterio_double_loc, &
   gdalrasterio_float_cmplx_loc, gdalrasterio_double_cmplx_loc
END INTERFACE
PRIVATE gdalrasterio_loc
PRIVATE gdalrasterio_int8_loc, gdalrasterio_int16_loc, gdalrasterio_int32_loc, &
 gdalrasterio_float_loc, gdalrasterio_double_loc, &
 gdalrasterio_float_cmplx_loc, gdalrasterio_double_cmplx_loc

CONTAINS

! Fortran specific version of some functions
FUNCTION gdalgcpstogeotransform_f(pasgcps, padfgeotransform, bapproxok)
TYPE(gdal_gcp),INTENT(in) :: pasgcps(:)
REAL(kind=c_double),INTENT(out) :: padfgeotransform(6)
INTEGER(kind=c_int),VALUE :: bapproxok
INTEGER(kind=c_int) :: gdalgcpstogeotransform_f

gdalgcpstogeotransform_f = gdalgcpstogeotransform(SIZE(pasgcps), pasgcps(1), padfgeotransform, bapproxok)

END FUNCTION gdalgcpstogeotransform_f


FUNCTION gdalrasterio_int8(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int8_t),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_int8


FUNCTION gdalrasterio_int8_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int8_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_Byte, 0, 0)

END FUNCTION gdalrasterio_int8_loc


FUNCTION gdalrasterio_int16(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int16_t),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_int16


FUNCTION gdalrasterio_int16_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int16_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_Int16, 0, 0)

END FUNCTION gdalrasterio_int16_loc


FUNCTION gdalrasterio_int32(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int32_t),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_int32


FUNCTION gdalrasterio_int32_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int32_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_Int32, 0, 0)

END FUNCTION gdalrasterio_int32_loc


FUNCTION gdalrasterio_float(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
REAL(kind=c_float),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_float


FUNCTION gdalrasterio_float_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
REAL(kind=c_float),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_Float32, 0, 0)

END FUNCTION gdalrasterio_float_loc


FUNCTION gdalrasterio_double(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
REAL(kind=c_double),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_double


FUNCTION gdalrasterio_double_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
REAL(kind=c_double),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_Float64, 0, 0)

END FUNCTION gdalrasterio_double_loc


FUNCTION gdalrasterio_float_cmplx(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
COMPLEX(kind=c_float),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_float_cmplx


FUNCTION gdalrasterio_float_cmplx_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
COMPLEX(kind=c_float_complex),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_CFloat32, 0, 0)

END FUNCTION gdalrasterio_float_cmplx_loc


FUNCTION gdalrasterio_double_cmplx(hband, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
COMPLEX(kind=c_double_complex),INTENT(inout) :: pbuffer(:,:)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio_loc(hband, erwflag, ndsxoff, ndsyoff, &
   SIZE(pbuffer,1), SIZE(pbuffer,2), pbuffer)

END FUNCTION gdalrasterio_double_cmplx


FUNCTION gdalrasterio_double_cmplx_loc(hband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer) RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
COMPLEX(kind=c_double_complex),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdalrasterio(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1)), &
 ndsxsize, ndsysize, GDT_CFloat64, 0, 0)

END FUNCTION gdalrasterio_double_cmplx_loc


END MODULE gdal
