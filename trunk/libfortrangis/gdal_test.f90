PROGRAM gdaltest
USE,INTRINSIC :: iso_c_binding
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

CALL gdalallregister()

PRINT*,'getting driver'
driver = gdalgetdriverbyname('GTiff'//CHAR(0))
IF (.NOT.gdalassociated(driver)) STOP

PRINT*,'creating dataset'
i1 = 120
j1 = 80
ds = gdalcreate(driver, 'gdal_test.tiff'//CHAR(0), i1, j1, 1, gdt_int32, &
 C_NULL_PTR)
ALLOCATE(z3(i1,j1,1))
DO j = 1, j1
  DO i = 1, i1
    z3(i,j,1) = i+2*j
  ENDDO
ENDDO
ierr = gdaldatasetrasterio_f(ds, GF_Write, 0, 0, z3)

CALL gdalclose(ds)

STOP

CALL getarg(1, file)

IF (file == '') STOP

ds = gdalopen(TRIM(file)//C_NULL_CHAR, GA_ReadOnly)
IF (.NOT.gdalassociated(ds)) STOP
PRINT*,'getgeotransform: ',gdalgetgeotransform(ds, gt)
PRINT*,gt
i1 = gdalgetrasterxsize(ds)
j1 = gdalgetrasterysize(ds)
PRINT*,'size: ',i1, j1

CALL gdalapplygeotransform_f(gt, 0.5_c_double, 0.5_c_double, x1, y1)
CALL gdalapplygeotransform_f(gt, &
 REAL(i1, kind=c_double)-0.5_c_double, &
 REAL(j1, kind=c_double)-0.5_c_double, x2, y2)
PRINT*,x1,y1,x2,y2
ALLOCATE(z(i1,j1))

band = gdalgetrasterband(ds, 1)
IF (.NOT.gdalassociated(band)) STOP

ierr = gdalrasterio_f(band, GF_Read, 0, 0, z)
PRINT*,'gdalrasterio: ',ierr
PRINT*,z(1,1),z(2,1),z(1,2),z(i1,j1)

DEALLOCATE(z)
CALL gdalclose(ds)

END PROGRAM gdaltest

