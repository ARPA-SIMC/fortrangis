PROGRAM proj_test6
USE,INTRINSIC :: iso_c_binding
USE fortranc
USE proj6
IMPLICIT none

! declare projection objects
TYPE(pj_object) :: pj, pjll
! declare coordinate objects
TYPE(pj_coord_object) :: coordg, coordp, coordgc
! declare area object
TYPE(pj_area_object) :: area

CHARACTER(len=512) :: proj_string

proj_string = &
 '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0'
! initialize a projection object from a string, remember //char(0)!
PRINT*,TRIM(proj_string)

pj = proj_create_crs_to_crs(pj_default_ctx, TRIM(proj_string)//CHAR(0), &
 'EPSG:4326'//CHAR(0), area)

pj = proj_destroy(pj)

END PROGRAM proj_test6
