!> Utility module for supporting Fortran 2003 C language interface module.
!! This module contains various utilties for simplifying the exchange
!! of character variables between Fortran and C when using the \a
!! ISO_C_BINDING intrinsic module of Fortran 2003.
!!
!! \ingroup libfortranc
MODULE fortranc
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

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
!! \param string null-terminated C-style string to convert
INTERFACE strtofchar
  MODULE PROCEDURE strtofchar_char, strtofchar_chararr, strtofchar_intarr, &
   strtofchar_ptr
END INTERFACE

PRIVATE
PUBLIC strlen, strtofchar, fchartostr, fchartrimtostr


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
TYPE(c_ptr),VALUE,INTENT(in) :: string
INTEGER :: strlen

INTEGER(kind=c_signed_char),POINTER :: pstring(:)
INTEGER :: i

!IF (c_associated(string)) THEN
  CALL c_f_pointer(string, pstring, (/HUGE(i)/))

  DO i = 1, SIZE(pstring)
    IF (pstring(i) == 0) EXIT
  ENDDO
  strlen = i - 1
!ELSE
!  strlen = 0 !-1?
!ENDIF

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
TYPE(c_ptr),VALUE :: string
CHARACTER(len=strlen(string)) :: fchar

CHARACTER(len=strlen(string)),POINTER :: pfchar

IF (c_associated(string)) THEN
  CALL c_f_pointer(string, pfchar)
  fchar(:) = pfchar(:)
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


END MODULE fortranc
