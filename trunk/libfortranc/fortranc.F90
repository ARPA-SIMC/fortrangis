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

!> Utility module for supporting Fortran 2003 C language interface module.
!! This module contains various utilties for simplifying the exchange
!! of character variables between Fortran and C when using the \a
!! ISO_C_BINDING intrinsic module of Fortran 2003.
!!
!! For an example of application of the \a fortranc module, have a
!! look at the following test program, which decodes the output of a C
!! function returning a char** result:
!! \include fortranc_test.F90
!!
!! \ingroup libfortranc
MODULE fortranc
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE


!> Fortran derived type for handling \a char** C objects.
!! This object allows Fortran programs to extract the data pointed by
!! every single pointer in a C \a char** object (or in principle a C
!! array of pointers to any kind of data), provided that the array of
!! pointers is terminated by a NULL pointer.
TYPE charpp
  PRIVATE
  TYPE(c_ptr),POINTER :: elem(:) => NULL()
END TYPE charpp

!> Equivalent of the strlen C function.
!!
!! \param string null-terminated C-style string to test
INTERFACE strlen
  MODULE PROCEDURE strlen_char, strlen_chararr, strlen_intarr, &
   strlen_ptr
END INTERFACE

!> Convert a null-terminated C string into a Fortran \a CHARACTER
!! variable of the proper length.  The input can be provided as a
!! Fortran \a CHARACTER scalar of any length, as a Fortran array of \a
!! CHARACTER of length one, as an array of 1-byte integers or as a C
!! pointer to char (\a char \a *).
!!
!! It is typically used for:
!!
!!  - converting a string created/modified by a C function and passed
!!    as a \n char \n * argument, interfaced as \n
!!    CHARACTER(kind=c_char,len=*) \n :: \n fchar for its subsequent
!!    use in Fortran
!!
!!  - (more frequently) converting a string returned by a C function
!!    declared as \n char \n * , interfaced as \n TYPE(c_ptr) for its
!!    subsequent use in Fortran
!!
!! \param string null-terminated C-style string to convert
INTERFACE strtofchar
  MODULE PROCEDURE strtofchar_char, strtofchar_chararr, strtofchar_intarr, &
   strtofchar_ptr
END INTERFACE

PRIVATE
PUBLIC strlen, strtofchar, fchartostr, fchartrimtostr
PUBLIC charpp, charpp_new, charpp_getsize, charpp_getptr

CONTAINS


PURE FUNCTION strlen_char(string) RESULT(strlen)
CHARACTER(kind=c_char,len=*),INTENT(in) :: string
INTEGER :: strlen

INTEGER :: i

DO i = 1, LEN(string)
  IF (string(i:i) == CHAR(0)) EXIT
ENDDO
strlen = i - 1

END FUNCTION strlen_char


PURE FUNCTION strlen_chararr(string) RESULT(strlen)
CHARACTER(kind=c_char,len=1),INTENT(in) :: string(:)
INTEGER :: strlen

INTEGER :: i

DO i = 1, SIZE(string)
  IF (string(i) == CHAR(0)) EXIT
ENDDO
strlen = i - 1

END FUNCTION strlen_chararr


PURE FUNCTION strlen_intarr(string) RESULT(strlen)
INTEGER(kind=c_signed_char),INTENT(in) :: string(:)
INTEGER :: strlen

INTEGER :: i

DO i = 1, SIZE(string)
  IF (string(i) == 0) EXIT
ENDDO
strlen = i - 1

END FUNCTION strlen_intarr


PURE FUNCTION strlen_ptr(string) RESULT(strlen)
TYPE(c_ptr),INTENT(in) :: string
INTEGER :: strlen

INTEGER(kind=c_signed_char),POINTER :: pstring(:)
INTEGER :: i

!IF (c_associated(string)) THEN ! conflicts with PURE
CALL C_F_POINTER(string, pstring, (/HUGE(i)/))

IF (ASSOCIATED(pstring)) THEN
  DO i = 1, SIZE(pstring)
    IF (pstring(i) == 0) EXIT
  ENDDO
  strlen = i - 1
ELSE
  strlen = 0
ENDIF

END FUNCTION strlen_ptr


FUNCTION strtofchar_char(string) RESULT(fchar)
CHARACTER(kind=c_char,len=*),INTENT(in) :: string
CHARACTER(len=strlen(string)) :: fchar

fchar(:) = string(1:LEN(fchar))

END FUNCTION strtofchar_char


FUNCTION strtofchar_chararr(string) RESULT(fchar)
CHARACTER(kind=c_char,len=1),INTENT(in) :: string(:)
CHARACTER(len=strlen(string)) :: fchar

INTEGER :: i

DO i = 1, LEN(fchar)
  fchar(i:i) = string(i)
ENDDO

END FUNCTION strtofchar_chararr


FUNCTION strtofchar_intarr(string) RESULT(fchar)
INTEGER(kind=c_signed_char),INTENT(in) :: string(:)
CHARACTER(len=strlen(string)) :: fchar

fchar(:) = TRANSFER(string(1:LEN(fchar)), fchar)

END FUNCTION strtofchar_intarr


FUNCTION strtofchar_ptr(string) RESULT(fchar)
TYPE(c_ptr) :: string
CHARACTER(len=strlen(string)) :: fchar

CHARACTER(len=strlen(string)),POINTER :: pfchar

IF (C_ASSOCIATED(string)) THEN
  CALL c_f_pointer(string, pfchar)
  fchar(:) = pfchar(:)
!ELSE
! silently return an empty string probably useless because
! strlen is zero in this case (to be tested)
!  fchar = ''
ENDIF

END FUNCTION strtofchar_ptr


!> Convert a Fortran \a CHARACTER variable into a null-terminated C
!! string.
FUNCTION fchartostr(fchar) RESULT(string)
CHARACTER(len=*),INTENT(in) :: fchar !< Fortran \a CHARACTER variable to convert
CHARACTER(kind=c_char,len=LEN(fchar)+1) :: string

string = fchar//CHAR(0)

END FUNCTION fchartostr


!> Trim trailing blanks and convert a Fortran \a CHARACTER variable
!! into a null-terminated C string.
FUNCTION fchartrimtostr(fchar) RESULT(string)
CHARACTER(len=*),INTENT(in) :: fchar !< Fortran \a CHARACTER variable to convert
CHARACTER(kind=c_char,len=LEN_TRIM(fchar)+1) :: string

string = TRIM(fchar)//CHAR(0)

END FUNCTION fchartrimtostr

!> Constructor for the a \a charpp object.
!! The argument, a generic C pointer, must be a C array of pointers
!! (char** charpp_c or char* charpp_c[n]), typically the result of a C
!! function.
FUNCTION charpp_new(charpp_c) RESULT(this)
TYPE(c_ptr),VALUE :: charpp_c
TYPE(charpp) :: this

INTEGER :: i
TYPE(c_ptr),POINTER :: charp(:)

IF (C_ASSOCIATED(charpp_c)) THEN
  ! HUGE() here is ugly, but we must set a finite size
  CALL C_F_POINTER(charpp_c, charp, (/HUGE(1)/))
  DO i = 1, SIZE(charp)
    IF (.NOT.C_ASSOCIATED(charp(i))) THEN
      CALL C_F_POINTER(charpp_c, this%elem, (/i-1/))
      RETURN
    ENDIF
  ENDDO
ENDIF
END FUNCTION charpp_new


!> Return the number of valid pointers in the array pointer \a this.
!! If the object has not been initialized or has been initialized with
!! errors, zero is returned.
FUNCTION charpp_getsize(this)
TYPE(charpp),INTENT(in) :: this
INTEGER :: charpp_getsize

IF (ASSOCIATED(this%elem)) THEN
  charpp_getsize = SIZE(this%elem)
ELSE
  charpp_getsize = 0
ENDIF

END FUNCTION charpp_getsize

!> Returns the nth pointer in the array pointer \a this.
!! If the object has not been initialized, or \a n is out of bounds, a
!! NULL pointer is returned, this condition can be checked by means of
!! the \a C_ASSOCIATED() function. If \a this is an array of pointers
!! to C null-terminated strings, the string can be returned as a
!! Fortran \a CHARACTER variable of the proper length by using the
!! strtofchar function, for example:
!!
!! \code
!! TYPE(charpp) :: envp
!! CHARACTER(len=256) :: str
!! ...
!! envp = charpp_new(charpp_c)
!! ...
!! str = 'hello, '//strtofchar(charpp_getptr(envp, 2))//'!'
!! \endcode
FUNCTION charpp_getptr(this, n)
TYPE(charpp),INTENT(in) :: this
INTEGER,INTENT(in) :: n
TYPE(c_ptr) :: charpp_getptr

charpp_getptr = C_NULL_PTR
IF (ASSOCIATED(this%elem)) THEN
  IF (n <= SIZE(this%elem)) THEN
    charpp_getptr = this%elem(n)
  ENDIF
ENDIF

END FUNCTION charpp_getptr

END MODULE fortranc
