!> Fortran 2003 interface to the gdal http://www.gdal.org/ library.
!! This module includes the interface to a very basic subset of gdal C
!! API functions, enough to read data of any kind from a file format
!! supported by gdal.
!!
!! \ingroup libfortrangis
MODULE gdal
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

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

!> Derived type defining a gdal dataset. No operations should be
!! performed on it except checking the null state with \a
!! C_ASSOCIATED(hds%ptr).
TYPE, BIND(C) :: gdaldataseth
  TYPE(c_ptr) :: ptr
END TYPE gdaldataseth

!> Derived type defining a gdal raster band. No operations should be
!! performed on it except checking the null state with \a
!! C_ASSOCIATED(hband%ptr).
TYPE, BIND(C) :: gdalrasterbandh
  TYPE(c_ptr) :: ptr
END TYPE gdalrasterbandh


!> Direct interface to the corresponding gdal C functions.
INTERFACE
!> Equivalent of the C function GDALAllRegister().
  SUBROUTINE gdalallregister() BIND(C,name="GDALAllRegister")
  END SUBROUTINE gdalallregister
END INTERFACE

INTERFACE
!> Equivalent of the C function GDALOpen().
!! The filename must be trimmed and null-terminated, e.g.  hds =
!! gdalopen(TRIM(filename)//C_NULL_CHAR, GA_ReadOnly).  The result
!! must be declared as \a TYPE(gdaldataseth) . In case of failure it
!! returns a NULL pointer, this condition has to be checked with the
!! gdalassociated() function.
  FUNCTION gdalopen_c(pszfilename, eaccess) BIND(C,name="GDALOpen")
  IMPORT
  CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*) !< filename, must be explicitly null-terminated
  INTEGER(kind=c_int),VALUE :: eaccess !< one of \a GA_ReadOnly, \a GA_Update
!  TYPE(gdaldataseth) :: gdalopen
  TYPE(c_ptr) :: gdalopen_c
  END FUNCTION gdalopen_c
END INTERFACE

INTERFACE
!> Equivalent of the C function GDALOpenShared().
!! The filename must be trimmed and null-terminated, e.g.  hds =
!! gdalopen(TRIM(filename)//C_NULL_CHAR, GA_ReadOnly) The result must
!! be declared as \a TYPE(gdaldataseth) . In case of failure it
!! returns a NULL pointer, this condition has to be checked with the
!! gdalassociated() function.
  FUNCTION gdalopenshared(pszfilename, eaccess) BIND(C,name="GDALOpenShared")
  IMPORT
  CHARACTER(kind=c_char) :: pszfilename(*) !< filename, must be explicitly null-terminated
  INTEGER(kind=c_int),VALUE :: eaccess !< one of \a GA_ReadOnly, \a GA_Update
  TYPE(gdaldataseth) :: gdalopenshared
  END FUNCTION gdalopenshared

!> Equivalent of the C function GDALGetGeoTransform().
!! The output argument \a padfgeotransform is a 6-element array
!! specifying the affine transformation from dataset array index space
!! to the projection coordinate space. See also
!! gdalapplygeotransform().  The result is 0 for success or 1 for
!! failure (in these cases an identity transformation is returned
!! anyway).
  FUNCTION gdalgetgeotransform(hds, padfgeotransform) &
   BIND(C,name="GDALGetGeoTransform")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< a gdal dataset object, returned by, e.g., gdalopen()
  REAL(kind=c_double),INTENT(out) :: padfgeotransform(6) !< the output affine transformation associated with the dataset
  INTEGER(kind=c_int) :: gdalgetgeotransform ! CPLErr
  END FUNCTION gdalgetgeotransform

!> Equivalent of the C function GDALInvGeoTransform().
!! The arguments are 6-element arrays specifying the affine
!! transformation from dataset array index space to the projection
!! coordinate space and vice-versa. See also gdalapplygeotransform().
!! The result is 1 for success or 0 when the transformation cannot be
!! inverted.
  FUNCTION gdalinvgeotransform(padfgeotransformin, padfinvgeotransformout) &
   BIND(C,name="GDALInvGeoTransform")
  IMPORT
  REAL(kind=c_double),INTENT(in) :: padfgeotransformin(6) !< input affine transformation
  REAL(kind=c_double),INTENT(out) :: padfinvgeotransformout(6) !< output inverted affine transformation
  INTEGER(kind=c_int) :: gdalinvgeotransform
  END FUNCTION gdalinvgeotransform

!> Equivalent of the C function GDALApplyGeoTransform().
!! Applies the specified transformation to (\a dfpixel, \a dfline).
  SUBROUTINE gdalapplygeotransform(padfgeotransform, dfpixel, dfline, &
   pdfgeox, pdfgeoy) BIND(C,name="GDALApplyGeoTransform")
  IMPORT
  REAL(kind=c_double),INTENT(in) :: padfgeotransform(6) !< affine transformation
  REAL(kind=c_double),VALUE :: dfpixel !< input x coordinate, typically an array index
  REAL(kind=c_double),VALUE :: dfline !< input y coordinate, typically an array index
  REAL(kind=c_double),INTENT(out) :: pdfgeox !< output x coordinate, typically a georeferenced longitude/easting
  REAL(kind=c_double),INTENT(out) :: pdfgeoy !< output x coordinate, typically a georeferenced latitude/northing
  END SUBROUTINE gdalapplygeotransform

!> Equivalent of the C function GDALClose().
!! Closes the specified dataset and associated files.
  SUBROUTINE gdalclose(hds) BIND(C,name="GDALClose")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< dataset
  END SUBROUTINE gdalclose

!> Equivalent of the C function GDALGetRasterXSize().
!! It returns the number of pixels along X axis in a dataset.
  FUNCTION gdalgetrasterxsize(hds) BIND(C,name="GDALGetRasterXSize")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< dataset
  INTEGER(kind=c_int) :: gdalgetrasterxsize
  END FUNCTION gdalgetrasterxsize

!> Equivalent of the C function GDALGetRasterYSize().
!! It returns the number of lines along Y axis in a dataset.
  FUNCTION gdalgetrasterysize(hds) BIND(C,name="GDALGetRasterYSize")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< dataset
  INTEGER(kind=c_int) :: gdalgetrasterysize
  END FUNCTION gdalgetrasterysize

!> Equivalent of the C function GDALGetRasterCount().
!! It returns the number of bands in a dataset.
  FUNCTION gdalgetrastercount(hds) BIND(C,name="GDALGetRasterCount")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< dataset
  INTEGER(kind=c_int) :: gdalgetrastercount
  END FUNCTION gdalgetrastercount

!> Equivalent of the C function GDALGetRasterBand().
!! It returns a band object from a dataset.  The result must be
!! declared as \a TYPE(gdalrasterbandh) . In case of failure it
!! returns a NULL pointer, this condition has to be checked with the
!! gdalassociated() function.
  FUNCTION gdalgetrasterband(hds, nbandid) BIND(C,name="GDALGetRasterBand")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds !< dataset
  INTEGER(kind=c_int),VALUE :: nbandid !< number of band
!  TYPE(gdalrasterbandh) :: gdalgetrasterband
  TYPE(c_ptr) :: gdalgetrasterband
  END FUNCTION gdalgetrasterband

!> Equivalent of the C function GDALGetRasterDataType().
  FUNCTION gdalgetrasterdatatype(hband) BIND(C,name="GDALGetRasterDataType")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterdatatype
  END FUNCTION gdalgetrasterdatatype

!> Equivalent of the C function GDALGetRasterBandXSize().
  FUNCTION gdalgetrasterbandxsize(hband) BIND(C,name="GDALGetRasterBandXSize")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterbandxsize
  END FUNCTION gdalgetrasterbandxsize

!> Equivalent of the C function GDALGetRasterBandYSize().
  FUNCTION gdalgetrasterbandysize(hband) BIND(C,name="GDALGetRasterBandYSize")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasterbandysize
  END FUNCTION gdalgetrasterbandysize

!> Equivalent of the C function GDALGetRasterAccess().
  FUNCTION gdalgetrasteraccess(hband) BIND(C,name="GDALGetRasterAccess")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetrasteraccess
  END FUNCTION gdalgetrasteraccess

!> Equivalent of the C function GDALGetBandNumber().
  FUNCTION gdalgetbandnumber(hband) BIND(C,name="GDALGetBandNumber")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int) :: gdalgetbandnumber
  END FUNCTION gdalgetbandnumber

!> Equivalent of the C function GDALGetBandDataset().
  FUNCTION gdalgetbanddataset(hband) BIND(C,name="GDALGetBandDataset")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
!  TYPE(gdaldataseth) :: gdalgetbanddataset
  TYPE(c_ptr) :: gdalgetbanddataset
  END FUNCTION gdalgetbanddataset

!> Equivalent of the C function GDALGetRasterNoDataValue().
  FUNCTION gdalgetrasternodatavalue(hband, pbsuccess) &
   BIND(C,name="GDALGetRasterNoDataValue")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(out) :: pbsuccess ! boolean indicating if a value is actually associated with this layer (!0 = true)
  REAL(kind=c_double) :: gdalgetrasternodatavalue
  END FUNCTION gdalgetrasternodatavalue

!> Equivalent of the C function GDALGetRasterMinimum().
  FUNCTION gdalgetrasterminimum(hband, pbsuccess) &
   BIND(C,name="GDALGetRasterMinimum")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(out) :: pbsuccess ! boolean indicating if a value is a tight minimum or not (!0 = true)
  REAL(kind=c_double) :: gdalgetrasterminimum
  END FUNCTION gdalgetrasterminimum

!> Equivalent of the C function GDALGetRasterMaximum().
  FUNCTION gdalgetrastermaximum(hband, pbsuccess) &
   BIND(C,name="GDALGetRasterMaximum")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(out) :: pbsuccess ! boolean indicating if a value is a tight maximum or not (!0 = true)
  REAL(kind=c_double) :: gdalgetrastermaximum
  END FUNCTION gdalgetrastermaximum

! Units value = (raw value * scale) + offset
!> Equivalent of the C function GDALGetRasterOffset().
  FUNCTION gdalgetrasteroffset(hband, pbsuccess) &
   BIND(C,name="GDALGetRasterOffset")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(out) :: pbsuccess ! boolean indicating if the returned value is meaningful or not
  REAL(kind=c_double) :: gdalgetrasteroffset
  END FUNCTION gdalgetrasteroffset

! Units value = (raw value * scale) + offset
!> Equivalent of the C function GDALGetRasterScale().
  FUNCTION gdalgetrasterscale(hband, pbsuccess) &
   BIND(C,name="GDALGetRasterScale")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),INTENT(out) :: pbsuccess ! boolean indicating if the returned value is meaningful or not
  REAL(kind=c_double) :: gdalgetrasterscale
  END FUNCTION gdalgetrasterscale

  FUNCTION gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
   nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
   BIND(C,name="GDALDatasetRasterIO")
  IMPORT
  TYPE(gdaldataseth),VALUE :: hds
  INTEGER(kind=c_int),VALUE :: erwflag
  INTEGER(kind=c_int),VALUE :: ndsxoff, ndsyoff, ndsxsize, ndsysize
  TYPE(c_ptr),VALUE :: pbuffer
  INTEGER(kind=c_int),VALUE :: nbxsize, nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype
  INTEGER(kind=c_int),VALUE :: nbandcount
  TYPE(c_ptr),VALUE :: panbandcount
  INTEGER(kind=c_int),VALUE :: npixelspace, nlinespace, nbandspace
  INTEGER(kind=c_int) :: gdaldatasetrasterio_c ! CPLErr
  END FUNCTION gdaldatasetrasterio_c

  FUNCTION gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
   npixelspace, nlinespace) BIND(C,name="GDALRasterIO")
  IMPORT
  TYPE(gdalrasterbandh),VALUE :: hband
  INTEGER(kind=c_int),VALUE :: erwflag
  INTEGER(kind=c_int),VALUE :: ndsxoff, ndsyoff, ndsxsize, ndsysize
  TYPE(c_ptr),VALUE :: pbuffer
  INTEGER(kind=c_int),VALUE :: nbxsize, nbysize
  INTEGER(kind=c_int),VALUE :: ebdatatype
  INTEGER(kind=c_int),VALUE :: npixelspace, nlinespace
  INTEGER(kind=c_int) :: gdalrasterio_c ! CPLErr
  END FUNCTION gdalrasterio_c

END INTERFACE

!> This function returns \a .TRUE. if the corresponding argument
!! has been correctly associated or \a .FALSE. if it is a NULL pointer.
INTERFACE gdalassociated
  MODULE PROCEDURE gdaldataseth_associated, gdalrasterbandh_associated
END INTERFACE

!> This subroutine can be used to set the corresponding argument to a
!! NULL pointer.
INTERFACE gdalnullify
  MODULE PROCEDURE gdaldataseth_nullify, gdalrasterbandh_nullify
END INTERFACE

INTERFACE gdaldatasetrasterio
  MODULE PROCEDURE gdaldatasetrasterio_byte, gdaldatasetrasterio_int32, &
   gdaldatasetrasterio_float32, gdaldatasetrasterio_float64, &
   gdaldatasetrasterio_cfloat32, gdaldatasetrasterio_cfloat64
END INTERFACE

INTERFACE gdalrasterio
  MODULE PROCEDURE gdalrasterio_byte, gdalrasterio_int32, &
   gdalrasterio_float32, gdalrasterio_float64, &
   gdalrasterio_cfloat32, gdalrasterio_cfloat64
END INTERFACE


CONTAINS


FUNCTION gdaldataseth_associated(hds1, hds2) RESULT(associated_)
TYPE(gdaldataseth),INTENT(in) :: hds1
TYPE(gdaldataseth),INTENT(in),OPTIONAL :: hds2
LOGICAL :: associated_
IF (PRESENT(hds2)) THEN
  associated_ = C_ASSOCIATED(hds1%ptr, hds2%ptr)
ELSE
  associated_ = C_ASSOCIATED(hds1%ptr)
ENDIF
END FUNCTION gdaldataseth_associated


FUNCTION gdalrasterbandh_associated(hband1, hband2) RESULT(associated_)
TYPE(gdalrasterbandh),INTENT(in) :: hband1
TYPE(gdalrasterbandh),INTENT(in),OPTIONAL :: hband2
LOGICAL :: associated_
IF(PRESENT(hband2)) THEN
  associated_ = C_ASSOCIATED(hband1%ptr, hband2%ptr)
ELSE
  associated_ = C_ASSOCIATED(hband1%ptr)
ENDIF
END FUNCTION gdalrasterbandh_associated


SUBROUTINE gdaldataseth_nullify(hds)
TYPE(gdaldataseth),INTENT(inout) :: hds
hds%ptr = C_NULL_PTR
END SUBROUTINE gdaldataseth_nullify


SUBROUTINE gdalrasterbandh_nullify(hband)
TYPE(gdalrasterbandh),INTENT(inout) :: hband
hband%ptr = C_NULL_PTR
END SUBROUTINE gdalrasterbandh_nullify


! trick to avoid bug in gfortran 4.3.2 with bind(c) on function
! returning a derived type
FUNCTION gdalopen(pszfilename, eaccess)
CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*) ! filename, must be explicitly null-terminated
INTEGER(kind=c_int) :: eaccess ! one of \a GA_ReadOnly, \a GA_Update
TYPE(gdaldataseth) :: gdalopen

gdalopen%ptr = gdalopen_c(pszfilename, eaccess)

END FUNCTION gdalopen


FUNCTION gdaldatasetrasterio_byte(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
INTEGER(kind=SELECTED_INT_KIND(1)),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Byte, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_byte


FUNCTION gdaldatasetrasterio_int32(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
INTEGER(kind=c_int),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Int32, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_int32


FUNCTION gdaldatasetrasterio_float32(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
REAL(kind=c_float),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Float32, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_float32


FUNCTION gdaldatasetrasterio_float64(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
REAL(kind=c_double),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Float64, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_float64


FUNCTION gdaldatasetrasterio_cfloat32(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
COMPLEX(kind=c_float_complex),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_CFloat32, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_cfloat32


FUNCTION gdaldatasetrasterio_cfloat64(hds, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) &
 RESULT(err)
TYPE(gdaldataseth),VALUE :: hds
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
COMPLEX(kind=c_double_complex),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in) :: nbandcount
INTEGER(kind=c_int),INTENT(in),OPTIONAL,TARGET :: panbandcount(*)
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace, nbandspace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace, lnbandspace
TYPE(c_ptr) :: lpanbandcount

IF (PRESENT(panbandcount)) THEN
  lpanbandcount = C_LOC(panbandcount)
ELSE
  lpanbandcount = C_NULL_PTR
ENDIF
IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF
IF (PRESENT(nbandspace)) THEN
  lnbandspace = nbandspace
ELSE
  lnbandspace = 0
ENDIF

err = gdaldatasetrasterio_c(hds, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_CFloat64, nbandcount, lpanbandcount, lnpixelspace, lnlinespace, lnbandspace)

END FUNCTION gdaldatasetrasterio_cfloat64


FUNCTION gdalrasterio_byte(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
INTEGER(kind=SELECTED_INT_KIND(1)),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Byte, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_byte


FUNCTION gdalrasterio_int32(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
INTEGER(kind=c_int),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Int32, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_int32


FUNCTION gdalrasterio_float32(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
REAL(kind=c_float),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Float32, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_float32


FUNCTION gdalrasterio_float64(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
REAL(kind=c_double),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_Float64, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_float64


FUNCTION gdalrasterio_cfloat32(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
COMPLEX(kind=c_float_complex),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_CFloat32, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_cfloat32


FUNCTION gdalrasterio_cfloat64(hband, erwflag, ndsxoff, ndsyoff, &
 ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, &
 npixelspace, nlinespace) &
 RESULT(err)
TYPE(gdalrasterbandh),VALUE :: hband
INTEGER(kind=c_int),INTENT(in) :: erwflag
INTEGER(kind=c_int),INTENT(in) :: ndsxoff, ndsyoff
INTEGER(kind=c_int),INTENT(in) :: ndsxsize, ndsysize
COMPLEX(kind=c_double_complex),TARGET,INTENT(inout) :: pbuffer(*)
INTEGER(kind=c_int),INTENT(in) :: nbxsize, nbysize
INTEGER(kind=c_int),INTENT(in),OPTIONAL :: npixelspace, nlinespace
INTEGER(kind=c_int) :: err ! CPLErr

INTEGER(kind=c_int) :: lnpixelspace, lnlinespace

IF (PRESENT(npixelspace)) THEN
  lnpixelspace = npixelspace
ELSE
  lnpixelspace = 0
ENDIF
IF (PRESENT(nlinespace)) THEN
  lnlinespace = nlinespace
ELSE
  lnlinespace = 0
ENDIF

err = gdalrasterio_c(hband, erwflag, ndsxoff, ndsyoff, &
   ndsxsize, ndsysize, C_LOC(pbuffer), nbxsize, nbysize, &
   GDT_CFloat64, lnpixelspace, lnlinespace)

END FUNCTION gdalrasterio_cfloat64


END MODULE gdal


!PROGRAM gdaltest
!USE gdal
!IMPLICIT none
!
!TYPE(gdaldataseth) :: ds
!TYPE(gdalrasterbandh) :: band
!CHARACTER(len=512) :: file
!REAL(kind=c_double) :: x1, y1, x2, y2, gt(6)
!INTEGER(kind=c_int) :: i1, j1, i2, j2, ierr
!REAL,ALLOCATABLE :: z(:,:)
!
!CALL getarg(1, file)
!
!CALL gdalallregister()
!ds = gdalopen(TRIM(file)//C_NULL_CHAR, GA_ReadOnly)
!IF (.NOT.C_ASSOCIATED(ds%ptr)) STOP
!PRINT*,'getgeotransform: ',gdalgetgeotransform(ds, gt)
!PRINT*,gt
!i1 = gdalgetrasterxsize(ds)
!j1 = gdalgetrasterxsize(ds)
!PRINT*,'size: ',i1, j1
!
!CALL gdalapplygeotransform(gt, 0.5_c_double, 0.5_c_double, x1, y1)
!CALL gdalapplygeotransform(gt, &
! REAL(i1, kind=c_double)-0.5_c_double, &
! REAL(j1, kind=c_double)-0.5_c_double, x2, y2)
!PRINT*,x1,y1,x2,y2
!ALLOCATE(z(i1,j1))
!
!band = gdalgetrasterband(ds, 1)
!IF (.NOT.C_ASSOCIATED(band%ptr)) STOP
!
!!ierr = gdalrasterio(band, GF_Read, 0, 0, i1, j1, z, i1, j1)
!PRINT*,'gdalrasterio: ',ierr
!PRINT*,z(1,1),z(2,1),z(1,2),z(i1,j1)
!
!DEALLOCATE(z)
!CALL gdalclose(ds)
!
!END PROGRAM gdaltest

