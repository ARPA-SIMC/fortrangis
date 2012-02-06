PROGRAM gdal_test
USE,INTRINSIC :: iso_c_binding
USE fortranc
USE gdal
IMPLICIT none

TYPE(gdaldriverh) :: driver
TYPE(gdaldataseth) :: ds
TYPE(gdalrasterbandh) :: band
!TYPE(c_ptr) :: ds, band
CHARACTER(len=512) :: file
REAL(kind=c_double) :: x1, y1, x2, y2, gt(6)
INTEGER(kind=c_int) :: i1, j1, i2, j2, ierr, i, j
REAL,ALLOCATABLE :: z(:,:), z3(:,:,:)


! register all available gdal drivers
CALL gdalallregister()
! file name to work on
file = 'gdal_test.tiff'

! ==== How to create a gdal dataset and export it to a file ====

! get a specific driver (see e.g. gdalinfo --formats)
PRINT*,'Getting GeoTIFF driver'
driver = gdalgetdriverbyname('GTiff'//CHAR(0))
IF (.NOT.gdalassociated(driver)) THEN
  PRINT*,'Error getting GeoTIFF driver from gdal'
  STOP 1
ENDIF

! create the dataset with i1xj1 points and 1 raster band of integer data
PRINT*,'Creating a GeoTIFF gdal dataset'
i1 = 120
j1 = 80
ds = gdalcreate(driver, TRIM(file)//CHAR(0), i1, j1, 1, gdt_int32, &
 c_ptr_ptr_getobject(c_ptr_ptr_new((/('',i=1,0)/))))
! (/('',i=1,0)/) is a trick to define a zero-length array no the fly,
! since we do not want to pass any specific option
IF (.NOT.gdalassociated(ds)) THEN
  PRINT*,'Error creating a GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

! create a dummy array of real data
ALLOCATE(z3(i1,j1,1))
DO j = 1, j1
  DO i = 1, i1
    z3(i,j,1) = i+2*j
  ENDDO
ENDDO

! write data to dataset with the simplified Fortran interface, one
! raster band is written starting from the upper(?) left corner
PRINT*,'Writing data to dataset'
ierr = gdaldatasetrasterio_f(ds, GF_Write, 0, 0, z3)
IF (ierr /= 0) THEN
  PRINT*,'Error writing data to GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

CALL gdalclose(ds)

! ==== How to read a gdal dataset from a file ====

PRINT*,'Opening a GeoTIFF gdal dataset for reading'
ds = gdalopen(TRIM(file)//CHAR(0), GA_ReadOnly)
IF (.NOT.gdalassociated(ds)) THEN
  PRINT*,'Error opening dataset on file ',TRIM(file)
  STOP 1
ENDIF

PRINT*,'Getting the affine transformation'
ierr = gdalgetgeotransform(ds, gt)
! an error is acceptable since no transformation had been defined
!IF (ierr /= 0) THEN
!  PRINT*,'Error getting the affine transformation from dataset on file ',TRIM(file)
!  STOP 1
!ENDIF
PRINT*,'The affine transformation matrix is: ',gt

PRINT*,'Getting the dataset size'
i2 = gdalgetrasterxsize(ds)
j2 = gdalgetrasterysize(ds)
IF (i1 /= i2 .OR. j1 /= j2) THEN
  PRINT*,'Error, wrong dataset x or y size ',i1,i2,j1,j2
  STOP 1
ENDIF
PRINT*,'The x/y size of the raster is: ',i2,j2

! apply the affine transformation to some coordinates
! original gdal version
CALL gdalapplygeotransform(gt, 0.5_c_double, 0.5_c_double, x1, y1)
! Fortran elemental version
CALL gdalapplygeotransform_f(gt, 0.5_c_double, 0.5_c_double, x2, y2)
IF (x1 /= x2 .OR. y1 /= y2) THEN ! this check should be relaxed
  PRINT*,'Error gdal and Fortran version of GDALApplyGeoTransform'
  PRINT*,'give different results'
  STOP 1
ENDIF

CALL gdalapplygeotransform_f(gt, 0.5_c_double, 0.5_c_double, x1, y1)
CALL gdalapplygeotransform_f(gt, &
 REAL(i1, kind=c_double)-0.5_c_double, &
 REAL(j1, kind=c_double)-0.5_c_double, x2, y2)
PRINT*,'The raster coordinates of the corners are: ',x1,y1,x2,y2

ALLOCATE(z(i2,j2))

! get the first raster band
PRINT*,'Getting a raster band'
band = gdalgetrasterband(ds, 1)
IF (.NOT.gdalassociated(band)) THEN
  PRINT*,'Error getting raster band from GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

! read data from raster band with the simplified Fortran interface,
! raster band is read starting from the upper(?) left corner
PRINT*,'Reading data from dataset'
ierr = gdalrasterio_f(band, GF_Read, 0, 0, z)
IF (ierr /= 0) THEN
  PRINT*,'Error reading data from GeoTIFF dataset on file ',TRIM(file)
  STOP 1
ENDIF

PRINT*,'Average error after write/read process: ',&
 SUM(ABS(z(:,:) - z3(:,:,1)))/(i2*j2)

CALL gdalclose(ds)

DEALLOCATE(z3,z)

END PROGRAM gdal_test

