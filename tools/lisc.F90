SUBROUTINE lisc(kdiml,cdlisc,klisc,listfilename)
! read the full list of possible ddh fields from a file
IMPLICIT NONE

CHARACTER*139 :: cdlisc(*)
CHARACTER*(*) :: listfilename
CHARACTER*200 :: cline
INTEGER(KIND=4)       :: klisc,kdiml,IEOF,JJ,IFILE=15
LOGICAL       :: LOK
write(*,*) 'lisc:',TRIM(listfilename)

! only READ, and give an error if the file doesn't exist !!!
OPEN(UNIT=IFILE,FILE=listfilename,form='formatted',STATUS='OLD',ACTION='READ')

JJ=0
LOK=.TRUE.
DO WHILE (LOK)
        READ(IFILE,fmt='(a)',IOSTAT=IEOF) cline
        IF (IEOF<0) THEN
                LOK=.FALSE.
        ELSE
                IF (cline(1:1) /= "#" .and. cline /= ' ') THEN
                        JJ=JJ+1
                        cdlisc(JJ)=cline
                        IF (JJ > kdiml) STOP 'list is too long'
                ENDIF
        ENDIF
ENDDO
klisc=JJ
CLOSE(IFILE)
!WRITE(*,*) 'lisc: read ',klisc,' fields.'
END SUBROUTINE LISC
