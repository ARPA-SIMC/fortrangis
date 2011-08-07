PROGRAM fortranc_test
USE,INTRINSIC :: ISO_C_BINDING
USE fortranc
IMPLICIT NONE

TYPE(charpp) :: envp

INTERFACE
FUNCTION returnenv() BIND(C,name="returnenv")
IMPORT
TYPE(c_ptr) :: returnenv
END FUNCTION returnenv
END INTERFACE

!extern char **environ;
envp = charpp_new(returnenv())



END PROGRAM fortranc_test
