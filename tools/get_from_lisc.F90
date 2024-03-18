SUBROUTINE GET_FROM_LISC(filename,fieldname,cline_out)
! ------------------
! look for a variable name in the conversion list
! and return its (139 character) dexcription.
! ---------------------------------
! Original: Alex Deckmyn, Dec. 2007
! ---------------------------------

CHARACTER*(*) :: filename,fieldname,cline_out
CHARACTER*139 :: cline
INTEGER(KIND=4)       :: IFILE
LOGICAL       :: LOK

write(*,"(A)") 'Looking for '//TRIM(fieldname)//' in '//TRIM(filename)

LOK=.TRUE.
IFILE=52
OPEN(IFILE,FILE=filename)
DO WHILE (LOK)
      READ(IFILE,fmt='(a)',IOSTAT=IEOF) cline
        IF (IEOF<0) THEN
                LOK=.FALSE.
                WRITE(*,*) 'Could not find the requested field ',trim(fieldname),'!'
        ELSEIF(cline(5:17)==fieldname) THEN
                write(*,*) 'found ',fieldname
                cline_out=cline
                LOK=.FALSE.
        ENDIF
ENDDO
END SUBROUTINE                
