!> Fortran 2003 interface to proj C library.
!! \ingroup libfortrangis
MODULE proj
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

INTERFACE
  FUNCTION pj_init_plus(name) BIND(C,name='pj_init_plus')
  IMPORT
  CHARACTER(kind=C_CHAR) :: name(*)
  TYPE(C_PTR) :: pj_init_plus
  END FUNCTION pj_init_plus

  FUNCTION pj_transform(src, dst, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_transform')
  IMPORT
  TYPE(C_PTR),VALUE :: src
  TYPE(C_PTR),VALUE :: dst
  INTEGER(kind=C_LONG),VALUE :: point_count
  INTEGER(kind=C_INT),VALUE :: point_offset
  REAL(kind=C_DOUBLE) :: x(*)
  REAL(kind=C_DOUBLE) :: y(*)
  REAL(kind=C_DOUBLE) :: z(*)
  INTEGER(kind=C_INT) :: pj_transform
  END FUNCTION pj_transform

  FUNCTION pj_datum_transform(src, dst, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_datum_transform')
  IMPORT
  TYPE(C_PTR),VALUE :: src
  TYPE(C_PTR),VALUE :: dst
  INTEGER(kind=C_LONG),VALUE :: point_count
  INTEGER(kind=C_INT),VALUE :: point_offset
  REAL(kind=C_DOUBLE) :: x(*)
  REAL(kind=C_DOUBLE) :: y(*)
  REAL(kind=C_DOUBLE) :: z(*)
  INTEGER(kind=C_INT) :: pj_datum_transform
  END FUNCTION pj_datum_transform

  FUNCTION pj_geocentric_to_geodetic(a, es, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_geocentric_to_geodetic')
  IMPORT
  REAL(kind=C_DOUBLE),VALUE :: a
  REAL(kind=C_DOUBLE),VALUE :: es
  INTEGER(kind=C_LONG),VALUE :: point_count
  INTEGER(kind=C_INT),VALUE :: point_offset
  REAL(kind=C_DOUBLE) :: x(*)
  REAL(kind=C_DOUBLE) :: y(*)
  REAL(kind=C_DOUBLE) :: z(*)
  INTEGER(kind=C_INT) :: pj_geocentric_to_geodetic
  END FUNCTION pj_geocentric_to_geodetic

  FUNCTION pj_geodetic_to_geocentric(a, es, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_geodetic_to_geocentric')
  IMPORT
  REAL(kind=C_DOUBLE),VALUE :: a
  REAL(kind=C_DOUBLE),VALUE :: es
  INTEGER(kind=C_LONG),VALUE :: point_count
  INTEGER(kind=C_INT),VALUE :: point_offset
  REAL(kind=C_DOUBLE) :: x(*)
  REAL(kind=C_DOUBLE) :: y(*)
  REAL(kind=C_DOUBLE) :: z(*)
  INTEGER(kind=C_INT) :: pj_geodetic_to_geocentric
  END FUNCTION pj_geodetic_to_geocentric

  FUNCTION pj_compare_datums(srcdefn, dstdefn) BIND(C,name='pj_compare_datums')
  IMPORT
  TYPE(C_PTR),VALUE :: srcdefn
  TYPE(C_PTR),VALUE :: dstdefn
  INTEGER(kind=C_INT) :: pj_compare_datums
  END FUNCTION pj_compare_datums

  FUNCTION pj_apply_gridshift(c, i, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_apply_gridshift')
  IMPORT
  CHARACTER(kind=C_CHAR) :: c(*)
  INTEGER(kind=C_INT),VALUE :: i
  INTEGER(kind=C_LONG),VALUE :: point_count
  INTEGER(kind=C_INT),VALUE :: point_offset
  REAL(kind=C_DOUBLE) :: x(*)
  REAL(kind=C_DOUBLE) :: y(*)
  REAL(kind=C_DOUBLE) :: z(*)
  INTEGER(kind=C_INT) :: pj_apply_gridshift
  END FUNCTION pj_apply_gridshift

  SUBROUTINE pj_deallocate_grids() BIND(C,name='pj_deallocate_grids')
  END SUBROUTINE pj_deallocate_grids

!int pj_is_latlong(projPJ);
!int pj_is_geocent(projPJ);
!void pj_pr_list(projPJ);
!void pj_free(projPJ);
!void pj_set_finder( const char *(*)(const char *) );
!void pj_set_searchpath ( int count, const char **path );
!projPJ pj_init(int, char **);
!char *pj_get_def(projPJ, int);
!projPJ pj_latlong_from_proj( projPJ );
!void *pj_malloc(size_t);
!void pj_dalloc(void *);
!char *pj_strerrno(int);
!int *pj_get_errno_ref(void);
!const char *pj_get_release(void);


END INTERFACE

END MODULE proj
