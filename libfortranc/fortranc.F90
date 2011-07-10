!> Utility module for supporting Fortran 2003 C language interface module.
!! This module contains various utilties for simplifying the exchange
!! of character variables between Fortran and C when using the \a
!! ISO_C_BINDING intrinsic module of Fortran 2003.
!!
!! \ingroup libfortranc
MODULE fortranc
USE,INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE

PRIVATE
PUBLIC strlen, strtofchar, fchartostr, fchartrimtostr

CONTAINS

!> Equivalent of the strlen C function.
PURE FUNCTION strlen(string)
CHARACTER(kind=c_char,len=*),INTENT(in) :: string !< null-terminated C-style string to test
INTEGER :: strlen

INTEGER :: i

DO i = 1, LEN(string)
  IF (string(i:i) == CHAR(0)) EXIT
ENDDO
strlen = i - 1

END FUNCTION strlen


!> Convert a null-terminated C string into a Fortran \a CHARACTER
!! variable of the proper length.
FUNCTION strtofchar(string) RESULT(fchar)
CHARACTER(kind=c_char,len=*),INTENT(in) :: string !< null-terminated C-style string to convert
CHARACTER(len=strlen(string)) :: fchar

fchar(:) = string(1:LEN(fchar))

END FUNCTION strtofchar


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
