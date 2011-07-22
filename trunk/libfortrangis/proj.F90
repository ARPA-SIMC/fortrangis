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

!> Fortran 2003 interface to the proj.4 http://trac.osgeo.org/proj/ library.
!! \ingroup libfortrangis
MODULE proj
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

INTERFACE
  FUNCTION pj_init_plus(name) BIND(C,name='pj_init_plus')
  IMPORT
  CHARACTER(kind=c_char) :: name(*)
  TYPE(c_ptr) :: pj_init_plus
  END FUNCTION pj_init_plus
END INTERFACE

INTERFACE
  FUNCTION pj_transform(src, dst, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_transform')
  IMPORT
  TYPE(c_ptr),VALUE :: src
  TYPE(c_ptr),VALUE :: dst
  INTEGER(kind=c_long),VALUE :: point_count
  INTEGER(kind=c_int),VALUE :: point_offset
  REAL(kind=c_double) :: x(*)
  REAL(kind=c_double) :: y(*)
  REAL(kind=c_double) :: z(*)
  INTEGER(kind=c_int) :: pj_transform
  END FUNCTION pj_transform
END INTERFACE

INTERFACE
  FUNCTION pj_datum_transform(src, dst, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_datum_transform')
  IMPORT
  TYPE(c_ptr),VALUE :: src
  TYPE(c_ptr),VALUE :: dst
  INTEGER(kind=c_long),VALUE :: point_count
  INTEGER(kind=c_int),VALUE :: point_offset
  REAL(kind=c_double) :: x(*)
  REAL(kind=c_double) :: y(*)
  REAL(kind=c_double) :: z(*)
  INTEGER(kind=c_int) :: pj_datum_transform
  END FUNCTION pj_datum_transform
END INTERFACE

INTERFACE
  FUNCTION pj_geocentric_to_geodetic(a, es, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_geocentric_to_geodetic')
  IMPORT
  REAL(kind=c_double),VALUE :: a
  REAL(kind=c_double),VALUE :: es
  INTEGER(kind=c_long),VALUE :: point_count
  INTEGER(kind=c_int),VALUE :: point_offset
  REAL(kind=c_double) :: x(*)
  REAL(kind=c_double) :: y(*)
  REAL(kind=c_double) :: z(*)
  INTEGER(kind=c_int) :: pj_geocentric_to_geodetic
  END FUNCTION pj_geocentric_to_geodetic
END INTERFACE

INTERFACE
  FUNCTION pj_geodetic_to_geocentric(a, es, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_geodetic_to_geocentric')
  IMPORT
  REAL(kind=c_double),VALUE :: a
  REAL(kind=c_double),VALUE :: es
  INTEGER(kind=c_long),VALUE :: point_count
  INTEGER(kind=c_int),VALUE :: point_offset
  REAL(kind=c_double) :: x(*)
  REAL(kind=c_double) :: y(*)
  REAL(kind=c_double) :: z(*)
  INTEGER(kind=c_int) :: pj_geodetic_to_geocentric
  END FUNCTION pj_geodetic_to_geocentric
END INTERFACE

INTERFACE
  FUNCTION pj_compare_datums(srcdefn, dstdefn) BIND(C,name='pj_compare_datums')
  IMPORT
  TYPE(c_ptr),VALUE :: srcdefn
  TYPE(c_ptr),VALUE :: dstdefn
  INTEGER(kind=c_int) :: pj_compare_datums
  END FUNCTION pj_compare_datums
END INTERFACE

INTERFACE
  FUNCTION pj_apply_gridshift(c, i, point_count, point_offset, x, y, z) &
   BIND(C,name='pj_apply_gridshift')
  IMPORT
  CHARACTER(kind=c_char) :: c(*)
  INTEGER(kind=c_int),VALUE :: i
  INTEGER(kind=c_long),VALUE :: point_count
  INTEGER(kind=c_int),VALUE :: point_offset
  REAL(kind=c_double) :: x(*)
  REAL(kind=c_double) :: y(*)
  REAL(kind=c_double) :: z(*)
  INTEGER(kind=c_int) :: pj_apply_gridshift
  END FUNCTION pj_apply_gridshift
END INTERFACE

INTERFACE
  SUBROUTINE pj_deallocate_grids() BIND(C,name='pj_deallocate_grids')
  END SUBROUTINE pj_deallocate_grids
END INTERFACE

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



END MODULE proj
