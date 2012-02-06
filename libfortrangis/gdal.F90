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
!! API plus a more "Fortranic" version of some functions.
!!
!! The following functions are directly interfaced to their
!! corresponding C version, so they are undocumented here, please
!! refer to the original gdal C API documentation, e.g. at the address
!! http://www.gdal.org/gdal_8h.html , for their use:
#include "gdalproto_doxy.f90"
!!
!! Other Fortran-style subroutines, functions and procedure interfaces
!! are documented explicitely here.
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
INTEGER(kind=c_int),PARAMETER :: GA_Update = 1 !< update

! GDALRWFlag
INTEGER(kind=c_int),PARAMETER :: GF_Read = 0 !< operation to be performed on a dataset: read
INTEGER(kind=c_int),PARAMETER :: GF_Write = 1 !< write

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
  SUBROUTINE gdalapplygeotransform(padfgeotransform, dfpixel, dfline, pdfgeox, pdfgeoy) BIND(C,name='GDALApplyGeoTransform')
  IMPORT
  REAL(kind=c_double) :: padfgeotransform(*)
  REAL(kind=c_double),VALUE :: dfpixel
  REAL(kind=c_double),VALUE :: dfline
  REAL(kind=c_double) :: pdfgeox
  REAL(kind=c_double) :: pdfgeoy
  END SUBROUTINE gdalapplygeotransform
END INTERFACE

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

!> Interface to a Fortran version of gdalapplygeotransform
!! working on scalars, 1-d, 2-d and 3-d arrays.  This is a Fortran
!! reimplementation of \a gdalapplygeotransform, giving the same
!! results but acting also on arrays of data with up to 3 dimensions
!! in a single call.
!!
!! SUBROUTINE gdalapplygeotransform_f(padfgeotransform, dfpixel, dfline, pdfgeox, pdfgeoy)
!! \param REAL(kind=c_double),INTENT(in)::padfgeotransform(6) the affine transformation
!! \param REAL(kind=c_double),INTENT(in)::dfpixel a scalar or an array with up to 3 dimensions
!! \param REAL(kind=c_double),INTENT(in)::dfline a scalar or an array with up to 3 dimensions
!! \param REAL(kind=c_double),INTENT(out)::pdfgeox a scalar or an array with up to 3 dimensions
!! \param REAL(kind=c_double),INTENT(out)::pdfgeoy a scalar or an array with up to 3 dimensions
INTERFACE gdalapplygeotransform_f
  MODULE PROCEDURE gdalapplygeotransform_f_0d, gdalapplygeotransform_f_1d, &
   gdalapplygeotransform_f_2d, gdalapplygeotransform_f_3d
END INTERFACE

PRIVATE gdalapplygeotransform_f_0d, gdalapplygeotransform_f_1d, &
 gdalapplygeotransform_f_2d, gdalapplygeotransform_f_3d


!> Simplified Fortran generic interface to the gdaldatasetrasterio C function.
!! This interface manages calls to \a gdaldatasetrasterio for
!! different Fortran data types. The size of the array provided
!! determines also automatically the size of the area and the number
!! of raster bands which are read or written from/to the dataset, so
!! that the arguments \a ndsxsize, \a ndsysize, \a nbxsize, \a
!! nbysize, \a nbandcount of the C interface are not needed and
!! inferred from the shape of \a pbuffer, while \a panbandcount, \a
!! npixelspace, \a nlinespace and \a nbandspace are set to default
!! values, thus the number of requested raster bands is read starting
!! from the first. The remaining arguments have the same meaning as
!! in the original \a gdaldatasetrasterio function which is still
!! available to Fortran under the \c gdaldatasetrasterio name.
!!
!! INTEGER FUNCTION gdaldatasetrasterio_f(hds, erwflag, ndsxoff, ndsyoff, pbuffer)
!! \param TYPE(gdadataseth),VALUE::hds dataset object to read or write
!! \param INTEGER(kind=c_int),INTENT(in)::erwflag \a GF_Read or \a GF_Write
!! \param INTEGER(kind=c_int),INTENT(in)::ndsxoff offset from x origin
!! \param INTEGER(kind=c_int),INTENT(in)::ndsyoff offset from y origin
!! \param INTEGER|REAL|DOUBLEPRECISION|COMPLEX,INTENT(inout)::pbuffer(:,:,:) data buffer, can be integer. real, or complex of different kinds
INTERFACE gdaldatasetrasterio_f
  MODULE PROCEDURE gdaldatasetrasterio_int8, gdaldatasetrasterio_int16, &
   gdaldatasetrasterio_int32, &
   gdaldatasetrasterio_float, gdaldatasetrasterio_double, &
   gdaldatasetrasterio_float_cmplx, gdaldatasetrasterio_double_cmplx
END INTERFACE

PRIVATE gdaldatasetrasterio_int8, gdaldatasetrasterio_int16, &
 gdaldatasetrasterio_int32, &
 gdaldatasetrasterio_float, gdaldatasetrasterio_double, &
 gdaldatasetrasterio_float_cmplx, gdaldatasetrasterio_double_cmplx

!> Simplified Fortran generic interface to the gdalrasterio C function.
!! This interface manages calls to \a gdalrasterio for different
!! Fortran data types. The size of the array provided determines also
!! automatically the size of the area which is read or written from/to
!! the raster band, so that the arguments \a ndsxsize, \a ndsysize, \a
!! nbxsize, \a nbysize of the C interface are not needed and inferred
!! from the shape of \a pbuffer, while \a npixelspace and \a
!! nlinespace are set to default values. The remaining arguments have
!! the same meaning as in the original \a gdalrasterio function which
!! is still available to Fortran under the \c gdalrasterio name.
!!
!! INTEGER FUNCTION gdalrasterio_f(hband, erwflag, ndsxoff, ndsyoff, pbuffer)
!! \param TYPE(gdalrasterbandh),VALUE::hband raster band object to read or write
!! \param INTEGER(kind=c_int),INTENT(in)::erwflag \a GF_Read or \a GF_Write
!! \param INTEGER(kind=c_int),INTENT(in)::ndsxoff offset from x origin
!! \param INTEGER(kind=c_int),INTENT(in)::ndsyoff offset from y origin
!! \param INTEGER|REAL|DOUBLEPRECISION|COMPLEX,INTENT(inout)::pbuffer(:,:) data buffer, can be integer. real, or complex of different kinds
INTERFACE gdalrasterio_f
  MODULE PROCEDURE gdalrasterio_int8, gdalrasterio_int16, &
   gdalrasterio_int32, &
   gdalrasterio_float, gdalrasterio_double, &
   gdalrasterio_float_cmplx, gdalrasterio_double_cmplx
END INTERFACE

PRIVATE gdalrasterio_int8, gdalrasterio_int16, &
 gdalrasterio_int32, &
 gdalrasterio_float, gdalrasterio_double, &
 gdalrasterio_float_cmplx, gdalrasterio_double_cmplx

! internal interfaces
INTERFACE gdaldatasetrasterio_loc
  MODULE PROCEDURE gdaldatasetrasterio_int8_loc, gdaldatasetrasterio_int16_loc, &
   gdaldatasetrasterio_int32_loc, &
   gdaldatasetrasterio_float_loc, gdaldatasetrasterio_double_loc, &
   gdaldatasetrasterio_float_cmplx_loc, gdaldatasetrasterio_double_cmplx_loc
END INTERFACE
PRIVATE gdaldatasetrasterio_loc
PRIVATE gdaldatasetrasterio_int8_loc, gdaldatasetrasterio_int16_loc, &
 gdaldatasetrasterio_int32_loc, &
 gdaldatasetrasterio_float_loc, gdaldatasetrasterio_double_loc, &
 gdaldatasetrasterio_float_cmplx_loc, gdaldatasetrasterio_double_cmplx_loc

INTERFACE gdalrasterio_loc
  MODULE PROCEDURE gdalrasterio_int8_loc, gdalrasterio_int16_loc, &
   gdalrasterio_int32_loc, &
   gdalrasterio_float_loc, gdalrasterio_double_loc, &
   gdalrasterio_float_cmplx_loc, gdalrasterio_double_cmplx_loc
END INTERFACE
PRIVATE gdalrasterio_loc
PRIVATE gdalrasterio_int8_loc, gdalrasterio_int16_loc, &
 gdalrasterio_int32_loc, &
 gdalrasterio_float_loc, gdalrasterio_double_loc, &
 gdalrasterio_float_cmplx_loc, gdalrasterio_double_cmplx_loc

CONTAINS

! Machine made procedure definitions
INCLUDE 'gdalproto_proc.f90'

! Fortran specific version of some functions
FUNCTION gdalgcpstogeotransform_f(pasgcps, padfgeotransform, bapproxok)
TYPE(gdal_gcp),INTENT(in) :: pasgcps(:)
REAL(kind=c_double),INTENT(out) :: padfgeotransform(6)
INTEGER(kind=c_int),VALUE :: bapproxok
INTEGER(kind=c_int) :: gdalgcpstogeotransform_f

gdalgcpstogeotransform_f = gdalgcpstogeotransform(SIZE(pasgcps), pasgcps(1), padfgeotransform, bapproxok)

END FUNCTION gdalgcpstogeotransform_f


! ========================================
! gdalapplygeotransform
! ========================================
! this unfortunately does not work as ELEMENTAL, padfgeotransform
! should be a scalar derived type
SUBROUTINE gdalapplygeotransform_f_0d(padfgeotransform, &
 dfpixel, dfline, pdfgeox, pdfgeoy)
REAL(kind=c_double),INTENT(in) :: padfgeotransform(6)
REAL(kind=c_double),INTENT(in) :: dfpixel
REAL(kind=c_double),INTENT(in) :: dfline
REAL(kind=c_double),INTENT(out) :: pdfgeox
REAL(kind=c_double),INTENT(out) :: pdfgeoy

pdfGeoX = padfGeoTransform(1) + &
 dfPixel * padfGeoTransform(2) + dfLine * padfGeoTransform(3)
pdfGeoY = padfGeoTransform(4) + &
 dfPixel * padfGeoTransform(5) + dfLine * padfGeoTransform(6)

END SUBROUTINE gdalapplygeotransform_f_0d

SUBROUTINE gdalapplygeotransform_f_1d(padfgeotransform, &
 dfpixel, dfline, pdfgeox, pdfgeoy)
REAL(kind=c_double),INTENT(in) :: padfgeotransform(6)
REAL(kind=c_double),INTENT(in) :: dfpixel(:)
REAL(kind=c_double),INTENT(in) :: dfline(:)
REAL(kind=c_double),INTENT(out) :: pdfgeox(:)
REAL(kind=c_double),INTENT(out) :: pdfgeoy(:)

pdfGeoX = padfGeoTransform(1) + &
 dfPixel * padfGeoTransform(2) + dfLine * padfGeoTransform(3)
pdfGeoY = padfGeoTransform(4) + &
 dfPixel * padfGeoTransform(5) + dfLine * padfGeoTransform(6)

END SUBROUTINE gdalapplygeotransform_f_1d

SUBROUTINE gdalapplygeotransform_f_2d(padfgeotransform, &
 dfpixel, dfline, pdfgeox, pdfgeoy)
REAL(kind=c_double),INTENT(in) :: padfgeotransform(6)
REAL(kind=c_double),INTENT(in) :: dfpixel(:,:)
REAL(kind=c_double),INTENT(in) :: dfline(:,:)
REAL(kind=c_double),INTENT(out) :: pdfgeox(:,:)
REAL(kind=c_double),INTENT(out) :: pdfgeoy(:,:)

pdfGeoX = padfGeoTransform(1) + &
 dfPixel * padfGeoTransform(2) + dfLine * padfGeoTransform(3)
pdfGeoY = padfGeoTransform(4) + &
 dfPixel * padfGeoTransform(5) + dfLine * padfGeoTransform(6)

END SUBROUTINE gdalapplygeotransform_f_2d

SUBROUTINE gdalapplygeotransform_f_3d(padfgeotransform, &
 dfpixel, dfline, pdfgeox, pdfgeoy)
REAL(kind=c_double),INTENT(in) :: padfgeotransform(6)
REAL(kind=c_double),INTENT(in) :: dfpixel(:,:,:)
REAL(kind=c_double),INTENT(in) :: dfline(:,:,:)
REAL(kind=c_double),INTENT(out) :: pdfgeox(:,:,:)
REAL(kind=c_double),INTENT(out) :: pdfgeoy(:,:,:)

pdfGeoX = padfGeoTransform(1) + &
 dfPixel * padfGeoTransform(2) + dfLine * padfGeoTransform(3)
pdfGeoY = padfGeoTransform(4) + &
 dfPixel * padfGeoTransform(5) + dfLine * padfGeoTransform(6)

END SUBROUTINE gdalapplygeotransform_f_3d


! ========================================
! gdaldatasetrasterio
! ========================================
FUNCTION gdaldatasetrasterio_int8(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int8_t),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_int8

FUNCTION gdaldatasetrasterio_int8_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int8_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_Byte, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_int8_loc


FUNCTION gdaldatasetrasterio_int16(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int16_t),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_int16

FUNCTION gdaldatasetrasterio_int16_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int16_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_Int16, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_int16_loc


FUNCTION gdaldatasetrasterio_int32(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int32_t),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_int32

FUNCTION gdaldatasetrasterio_int32_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int32_t),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_Int32, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_int32_loc


FUNCTION gdaldatasetrasterio_float(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
REAL(kind=c_float),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_float

FUNCTION gdaldatasetrasterio_float_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
REAL(kind=c_float),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_Float32, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_float_loc


FUNCTION gdaldatasetrasterio_double(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
REAL(kind=c_double),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_double

FUNCTION gdaldatasetrasterio_double_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
REAL(kind=c_double),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_Float64, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_double_loc


FUNCTION gdaldatasetrasterio_float_cmplx(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
COMPLEX(kind=c_float_complex),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_float_cmplx

FUNCTION gdaldatasetrasterio_float_cmplx_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
COMPLEX(kind=c_float_complex),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_CFloat32, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_float_cmplx_loc


FUNCTION gdaldatasetrasterio_double_cmplx(hds, erwflag, ndsxoff, ndsyoff, pbuffer) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
COMPLEX(kind=c_double_complex),INTENT(inout) :: pbuffer(:,:,:)
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: i

err = gdaldatasetrasterio_loc(hds, erwflag, ndsxoff, ndsyoff, &
 SIZE(pbuffer,1), SIZE(pbuffer,2), SIZE(pbuffer,3), pbuffer, &
 (/(i,i=1,SIZE(pbuffer,3))/))

END FUNCTION gdaldatasetrasterio_double_cmplx

FUNCTION gdaldatasetrasterio_double_cmplx_loc(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, nbandcount, pbuffer, panbandcount) RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize
INTEGER(kind=c_int),INTENT(in) :: ndsysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
COMPLEX(kind=c_double_complex),TARGET,INTENT(inout) :: pbuffer(ndsxsize,ndsysize,nbandcount)
INTEGER(kind=c_int),INTENT(in) :: panbandcount(*)
INTEGER(kind=c_int) :: err ! CPLErr

err = gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, C_LOC(pbuffer(1,1,1)), &
 ndsxsize, ndsysize, GDT_CFloat64, nbandcount, panbandcount, 0, 0, 0)

END FUNCTION gdaldatasetrasterio_double_cmplx_loc


! ========================================
! gdaldatasetrasterio
! ========================================
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
COMPLEX(kind=c_float_complex),INTENT(inout) :: pbuffer(:,:)
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
