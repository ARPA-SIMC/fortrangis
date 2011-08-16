PROGRAM fortranc_test
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE

INTERFACE
FUNCTION return_charpp() BIND(C,name="return_charpp")
IMPORT
TYPE(c_ptr) :: return_charpp
END FUNCTION return_charpp
END INTERFACE

TYPE(charpp) :: envp
INTEGER :: i

! get a char** object from C function return_charpp(), the function
! result has been declared as:
! char* var[4] = { "first", "segundo", "troisieme", NULL }
PRINT*,'Getting a charpp object from C'
envp = charpp_new(return_charpp())
!IF (.NOT.C_ASSOCIATED(envp)) THEN
!  PRINT*,'Error in charpp_new, got a NULL pointer'
!ENDIF

! get the number of valid pointers in envp
PRINT*,'The object has ',charpp_getsize(envp),' elements'
IF (charpp_getsize(envp) /= 3) THEN
  PRINT*,'Error in charpp_getsize:',3,charpp_getsize(envp)
  STOP 1
ENDIF

! get the content of selected pointers as a Fortran CHARACTER variable
! of the right length, count starts from 1
IF (strtofchar(charpp_getptr(envp, 1)) /= 'first') THEN
  PRINT*,'Error in charpp_getptr:',strtofchar(charpp_getptr(envp, 1)),':first'
  STOP 1
ENDIF
IF (strtofchar(charpp_getptr(envp, 2)) /= 'segundo') THEN
  PRINT*,'Error in charpp_getptr:',strtofchar(charpp_getptr(envp, 2)),':segundo'
  STOP 1
ENDIF
IF (strtofchar(charpp_getptr(envp, 3)) /= 'troisieme') THEN
  PRINT*,'Error in charpp_getptr:',strtofchar(charpp_getptr(envp, 3)),':troisieme'
  STOP 1
ENDIF

PRINT*,'The object contains the expected data'

END PROGRAM fortranc_test
