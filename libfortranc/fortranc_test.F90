PROGRAM fortranc_test
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE

! interface to functions in c_test.c
INTERFACE
FUNCTION return_null_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_null_charp
END FUNCTION return_null_charp

FUNCTION return_empty_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_empty_charp
END FUNCTION return_empty_charp

FUNCTION return_8_charp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_8_charp
END FUNCTION return_8_charp

FUNCTION return_charpp() BIND(C)
IMPORT
TYPE(c_ptr) :: return_charpp
END FUNCTION return_charpp
END INTERFACE

TYPE(charpp) :: envp
INTEGER :: i


! ==== How to use strlen for a char* null-terminated C string ====

PRINT*,'Testing strlen with C char* argument'
! A NULL pointer should return zero (not obvious, for safety)
IF (strlen(return_null_charp()) /= 0) THEN
  PRINT*,'Error in strlen: a NULL char* does not return zero, ', &
   strlen(return_null_charp())
  STOP 1
ENDIF
! A zero-len string should return zero (obvious)
IF (strlen(return_empty_charp()) /= 0) THEN
  PRINT*,'Error in strlen: a zero len char* does not return zero, ', &
   strlen(return_empty_charp())
  STOP 1
ENDIF
! A 8-char string should return 8 (obvious)
IF (strlen(return_8_charp()) /= 8) THEN
  PRINT*,'Error in strlen: a nonzero len char* does not return expected len (8), ', &
   strlen(return_8_charp())
  STOP 1
ENDIF

PRINT*,'Strlen returns the expected values'


! ==== How to use charpp ====

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
IF (strtofchar(charpp_getptr(envp, 4)) /= '') THEN
  PRINT*,'Error in charpp_getptr: out of bound request should return empty string:',strtofchar(charpp_getptr(envp, 4))
  STOP 1
ENDIF

PRINT*,'The object contains the expected data'

END PROGRAM fortranc_test
