PROGRAM ddhb_makelists
! parse the budget file
! write ddhi and ddhb lists
! prepare the list of fields for graphs
! ---------------------------------
! Original: Alex Deckmyn, Dec. 2007
! ---------------------------------
IMPLICIT NONE
INTEGER(KIND=4), PARAMETER :: MAXCOMP=50,MAXFIELDS=200
CHARACTER (LEN=200) FBLFILENAME
CHARACTER (LEN=200) FILEPREFIX
INTEGER(KIND=4) :: NARG
CHARACTER*13  :: fieldname(MAXFIELDS),listname(MAXCOMP)
CHARACTER     :: fieldsign(MAXFIELDS),mainsign(MAXCOMP)
CHARACTER (LEN=200) :: compname(MAXCOMP),asciifile(MAXCOMP)
CHARACTER (LEN=200) :: resname,tendname,sumname
INTEGER(KIND=4)       :: nfield(MAXCOMP)
CHARACTER*1   :: cl1
CHARACTER*2   :: mainvar,maincomp(MAXCOMP)
CHARACTER*4   :: cmain0,cmain1,cmainm
CHARACTER*13   ::cmainres
CHARACTER*30  :: budgetname
CHARACTER (LEN=3)  :: FLUXCONV
CHARACTER (LEN=200) :: ddhtfile,ddhifile,ddhilist,graphfile,mvfile
CHARACTER (LEN=200) :: liscfile
INTEGER(KIND=4)       :: JJ,KK,totfields,totcomp,totmain,jfield,jcomp
INTEGER(KIND=4)       :: IUNIT=21
CHARACTER (LEN=15)  :: cunit
REAL(KIND=8)          :: zcoef,mcoef
INTEGER(KIND=4)       :: iechlog
CHARACTER*139 :: cline
CHARACTER*300 :: clfile_name
CHARACTER*10  :: clconvft
LOGICAL       :: lunit,lcoef,lsum

! read command line

!NARG=IARGC()
!IF (NARG/=3) THEN
!        STOP('need 3 arguments: fblfile  fileprefix conversion_list')
!ENDIF

CALL GETARG(1,FBLFILENAME)
CALL GETARG(2,fileprefix)
CALL GETARG(3,liscfile)
CALL GETARG(4,clconvft) ! clconvft='flux' or 'tend'

! some defaults
resname='Residual'
tendname='Total tendency'
sumname=' '
LSUM=.FALSE.
CALL parse_fbl(trim(FBLFILENAME),trim(FILEPREFIX),budgetname,compname, &
    & fieldname,fieldsign,nfield,totfields,totcomp,mainvar,maincomp,mainsign, &
    & resname,tendname,sumname,totmain,lunit,cunit,lcoef,mcoef)

if(trim(clconvft) == 'tend') then
  cl1='T'
  cmain0='V'//mainvar//'0'
  cmain1='V'//mainvar//'1'
  cmainm='V'//mainvar//'M'
  cmainres='T'//mainvar//'RESIDUAL'
else
  cl1='F'
  cmain0='F'//mainvar//'0'
  cmain1='F'//mainvar//'1'
  cmainm='F'//mainvar//'M'
  cmainres='F'//mainvar//'RESIDUAL'
endif

! If the budget list included a SUM, then we also put the sum of contributions
! in the graph
IF (sumname/=' ') LSUM=.TRUE.

DO jcomp=1,totcomp
  WRITE(listname(jcomp),FMT="(A1,A2,A5,i2.2)") cl1,mainvar,'COMPL',jcomp
  clfile_name=TRIM(fileprefix)//TRIM(SQUASH_STRING(compname(jcomp)," .,:"))//'.dta'
  WRITE(asciifile(jcomp),FMT="(A)") trim(clfile_name)
ENDDO
IF (lsum) THEN
        listname(totcomp+1)='T'//mainvar//'COMPSUM'
        WRITE(asciifile(totcomp+1),FMT="(A)") &
        & TRIM(fileprefix)//TRIM(listname(totcomp+1))//'.dta'
ENDIF



ddhtfile='lc.ddht'
ddhifile='lc.ddhi'
ddhilist='ddhi_list.tmp'
graphfile='ddhb_graph'
mvfile='ddhb_rename'
! ------------------------
! First we write the DDHT :

OPEN(IUNIT,FILE=ddhtfile)
! We may have to cumulate for the main variable
WRITE(IUNIT,FMT="(A)")'0'
if(trim(clconvft) == 'flux') then
  
  !-------------------------------------------------
  ! Convert variables into fluxes.
  !-------------------------------------------------
  DO JJ=1,totmain
    WRITE(IUNIT,FMT='(A)') 'V'//maincomp(JJ)//'1 V'//maincomp(JJ)//'0 - V>F '
  ENDDO
  WRITE(IUNIT,FMT="(A)") 'ECR"F'//maincomp(totmain)//'M"'
else
  
  !-------------------------------------------------
  ! Variables keeped as such.
  !-------------------------------------------------
  DO JJ=1,totmain
    WRITE(IUNIT,FMT="(A)")'V'//maincomp(JJ)//'0 '//mainsign(JJ)
  ENDDO
  WRITE(IUNIT,FMT="(A)") 'ECR"'//cmain0//'"'
  
  WRITE(IUNIT,FMT="(A)")'0'
  DO JJ=1,totmain
    WRITE(IUNIT,FMT="(A)")'V'//maincomp(JJ)//'1 '//mainsign(JJ)
  ENDDO
  WRITE(IUNIT,FMT="(A)") 'ECR"'//cmain1//'"'
endif

jfield=0
! The different budgets are exported and cumulated
DO jcomp=1,totcomp
        WRITE(IUNIT,FMT="(A)") '0'
        DO JJ=1,nfield(jcomp)
                jfield=jfield+1
                IF(fieldname(jfield)(1:1)=='F' .and. trim(clconvft) == 'tend') THEN
                        FLUXCONV='F>V'
                ELSEIF((fieldname(jfield)(1:1)=='T' .or. fieldname(jfield)(1:1)=='V') .and. trim(clconvft) == 'flux') THEN
                        FLUXCONV='V>F'
                ELSE
                        FLUXCONV='   '
                ENDIF
                WRITE(IUNIT,FMT='(a13,1x,a3,1x,a1)') fieldname(jfield),FLUXCONV,fieldsign(jfield)
        ENDDO
        WRITE(IUNIT,FMT="(A)") 'ECR"'//TRIM(listname(jcomp))//'"'
        IF(jcomp>=2) WRITE(IUNIT,FMT="(A)") '+'
ENDDO
IF (lsum) WRITE(IUNIT,FMT="(A)") 'ECR"'//TRIM(listname(totcomp+1))//'"'
WRITE(IUNIT,FMT="(A)") 'COMPLEMENT'
WRITE(IUNIT,FMT="(A)") 'ECR"'//TRIM(cmainres)//'"'
CLOSE(IUNIT)

! --------------
! entry for DDHI
! --------------
OPEN(IUNIT,FILE=ddhifile)
DO jcomp=1,totcomp
        WRITE(IUNIT,FMT="(A)") TRIM(listname(jcomp))//'/'//TRIM(compname(jcomp))
ENDDO
IF (LSUM) WRITE(IUNIT,FMT="(A)") TRIM(listname(totcomp+1))//'/'//TRIM(sumname)
WRITE(IUNIT,FMT="(A4)") cmainm//'/'//TRIM(tendname)
WRITE(IUNIT,FMT="(A)") TRIM(cmainres)//'/'//TRIM(resname)
CLOSE(IUNIT)

! -----------------------
! entry for ddhi_list.tmp
! -----------------------
! We assume the main variable is defined in the standard list.
CALL GET_FROM_LISC(liscfile,cmainm,cline)
WRITE(*,*) 'found in LISC:',cline

!IF (cline(1:4) /= 'BUII') then
!  write(*,*) 'Can not deal with '//cline(1:4)
!  write(*,fmt=*) 'error/ddhb_makelists'
!  call exit(1)
!endif

READ(cline,FMT="(4x,13x,26x,60x,15x,f18.6,i3)") zcoef,iechlog
IF (lcoef) zcoef=zcoef*mcoef 
! We now have a possible problem: ZCOEF may become to big for f18.6
! which means we have to use iechlog.
DO WHILE (ZCOEF >= 1.0E+11)
        ZCOEF=ZCOEF/10.
        IECHLOG=IECHLOG+1
ENDDO
        
WRITE(*,*) 'COEF:',ZCOEF
!WRITE(*,FMT="(f18.6)"),ZCOEF
IF (.NOT. lunit) READ(cline,FMT="(103x,A15,21x)") cunit

OPEN(IUNIT,FILE=ddhilist)
! total tendency
if(trim(clconvft) == 'flux') then
  WRITE(IUNIT,FMT="(a4,a4,9x,a4,9x,a4,9x,a60,a15,f18.6,i3)") &
  & cline(1:4),cmainm,cmainm,'    ',tendname,cunit,zcoef,iechlog
else
  WRITE(IUNIT,FMT="(a4,a4,9x,a4,9x,a4,9x,a60,a15,f18.6,i3)") &
  & cline(1:4),cmainm,cmain1,cmain0,tendname,cunit,zcoef,iechlog
endif
! the budget components
! maybe safer to read TxxCOMPLEMENT from the list and use this as a basis?
!CALL GET_FROM_LISC(liscfile,'T'//mainvar//'COMPLEMENT',cline)
DO jcomp=1,totcomp
       WRITE(IUNIT,FMT="(a4,2a13,13x,a60,a15,f18.6,i3)") &
       & 'BIIP',listname(jcomp),listname(jcomp),compname(jcomp),cunit,zcoef,iechlog
ENDDO
! sum of contributions
IF (lsum) WRITE(IUNIT,FMT="(a4,2a13,13x,a60,a15,f18.6,i3)") &
     & 'BIIP',listname(totcomp+1),listname(totcomp+1),sumname,cunit,zcoef,iechlog
! residual
WRITE(IUNIT,FMT="(a4,2a13,13x,a60,a15,f18.6,i3)") &
     & 'BIIP',cmainres,cmainres,resname,cunit,zcoef,iechlog
CLOSE(IUNIT)

! ----------------------------------------
! a shell script to rename the ascii files
! ----------------------------------------
OPEN(IUNIT,FILE=mvfile)
DO jcomp=1,totcomp
     WRITE(IUNIT,"(A)") &
     &'mv tmp.'//TRIM(listname(jcomp))//'.dta '//trim(asciifile(jcomp))
ENDDO
IF (LSUM) WRITE(IUNIT,"(A)") &
    &'mv tmp.'//TRIM(listname(totcomp+1))//'.dta '//trim(asciifile(totcomp+1))
WRITE(IUNIT,"(A)") &
    &'mv tmp.'//cmainm//'.dta '//trim(fileprefix)//cmainm//'.dta'
WRITE(IUNIT,"(A)") &
    &'mv tmp.'//trim(cmainres)//'.dta '//trim(fileprefix)//trim(cmainres)//'.dta'
CLOSE(IUNIT)

! --------------------------------
! Now the final .doc for the graph
! --------------------------------
OPEN(UNIT=IUNIT,FILE=graphfile,form='FORMATTED')
WRITE(IUNIT,"(A)") '#TITRE='//TRIM(budgetname)
DO jcomp=1,totcomp
     WRITE(IUNIT,"(A)") '#CHAMP='//TRIM(compname(jcomp))
     WRITE(IUNIT,"(A)") '#FICHIER='//trim(asciifile(jcomp))
ENDDO
IF (lsum) THEN
        WRITE(IUNIT,"(A)") '#CHAMP='//TRIM(sumname)
        WRITE(IUNIT,"(A)") '#FICHIER='//TRIM(asciifile(totcomp+1))
ENDIF


WRITE(IUNIT,"(A)") '#CHAMP='//TRIM(resname)
WRITE(IUNIT,"(A)") '#FICHIER='//trim(fileprefix)//trim(cmainres)//'.dta'
WRITE(IUNIT,"(A)") '#CHAMP='//TRIM(tendname)
WRITE(IUNIT,"(A)") '#FICHIER='//trim(fileprefix)//cmainm//'.dta'

WRITE(IUNIT,"(A)") '#UNITE='//TRIM(ADJUSTL(cunit))
WRITE(IUNIT,"(A)") '#INV'
CLOSE(IUNIT)

CONTAINS

FUNCTION SQUASH_STRING(INSTRING,SCANSTRING)
CHARACTER (LEN=*) :: INSTRING,SCANSTRING
CHARACTER (LEN=LEN(INSTRING)) :: tempstring,SQUASH_STRING
INTEGER(KIND=4) :: JJ
LOGICAL :: LOK
LOK=.TRUE.
tempstring=INSTRING
DO WHILE (LOK)
      JJ=SCAN(TRIM(tempstring),SCANSTRING)
      IF (JJ==0) THEN
              LOK=.FALSE.
      ELSEIF (JJ==1) THEN
              tempstring=tempstring(2:LEN(tempstring))
      ELSE
              tempstring=tempstring(1:JJ-1)//tempstring(JJ+1:LEN(tempstring))
      ENDIF
ENDDO
SQUASH_STRING=tempstring
END FUNCTION

END PROGRAM
#include"parse_fbl.F90"
#include"get_from_lisc.F90"
