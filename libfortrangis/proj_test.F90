PROGRAM proj_test
USE,INTRINSIC :: iso_c_binding
!USE fortranc
USE proj
IMPLICIT none

! declare projection objects
TYPE(pj_object) :: pj, pjll
! declare coordinate objects
TYPE(pjuv_object) :: coordg, coordp, coordgc
REAL(kind=c_double) :: x(1), y(1), z(1)
INTEGER :: res

! initialize a projection object from a string, remember //char(0)!
pj = pj_init_plus('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'//CHAR(0))
PRINT*,'Associated:',pj_associated(pj)
IF (.NOT.pj_associated(pj)) CALL EXIT(1)

! build a corresponding latlon projection
pjll = pj_latlong_from_proj(pj)
IF (.NOT.pj_associated(pjll)) CALL EXIT(1)
PRINT*,'Associated:',pj_associated(pjll)

! define a coordinate set, it is latlong so convert to radians
coordg = pjuv_object(11.0D0*pj_deg_to_rad,45.0D0*pj_deg_to_rad)

! make a forward projection, result in meters
coordp = pj_fwd(coordg, pj)
! return back to polar coordinates in radians
coordgc = pj_inv(coordp, pj)
PRINT*,'Original:',coordg
PRINT*,'Projected:',coordp
PRINT*,'Returned:',coordgc

! check whether they are the same within 10-6 radians
IF (ABS(coordgc%u-coordg%u) > 1.0D-6 .OR. ABS(coordgc%v-coordg%v) > 1.0D-6) THEN
  CALL EXIT(1)
ENDIF

!x(1) = coordg%u
!y(1) = coordg%v
!z(1) = 0.0D0
!res = pj_transform(pjll, pj, x, y, z)
!PRINT*,x,y,res

END PROGRAM proj_test

