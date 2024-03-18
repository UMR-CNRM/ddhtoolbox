SUBROUTINE PARSE_FBL(fblfilename,fileprefix,budgetname,compname, &
    & fieldname,fieldsign,nfield,totfields,totcomp,mainvar,maincomp,mainsign, &
    & resname,tendname,sumname,totmain,lunit,cunit,lcoef,coef)
! -------------------------------------
! parse the "Budget package list"
! called by ddhb_makelists
! Original: Alex Deckmyn, December 2007
! -------------------------------------
IMPLICIT NONE

CHARACTER*(*) :: fblfilename
CHARACTER*(*) :: fileprefix
CHARACTER*200 :: cline
CHARACTER*(*)  :: mainvar, resname, tendname,sumname,cunit
CHARACTER*(*)  :: budgetname
CHARACTER(LEN=*)  :: fieldname(*)
CHARACTER  :: fieldsign(*),mainsign(*)
CHARACTER*(*)  :: compname(*),maincomp(*)
INTEGER(KIND=4)       :: nfield(*)
INTEGER(KIND=4)       :: IEOF,JJ,IFILE=15,jcomp,totfields,totcomp,totmain,jfield,jmain
LOGICAL       :: LOK,LMAIN,LUNIT,LCOEF
REAL(KIND=8)          :: COEF

OPEN(UNIT=IFILE,FILE=fblfilename,form='formatted')

jcomp=0
jmain=0
jfield=0
lcoef=.FALSE.
coef=1.
lunit=.FALSE.
cunit=' '

LOK=.TRUE.
LMAIN=.FALSE.
DO WHILE (LOK)
        READ(IFILE,fmt='(a)',IOSTAT=IEOF) cline
        IF (IEOF<0) THEN
                LOK=.FALSE.
        ELSE
                cline=ADJUSTL(cline)
        IF (cline(1:1) == "#" ) THEN
        ! it's a comment line, so do nothing
        ELSEIF (cline == " " ) THEN
        ! it's an empty line
        ELSE
                cline=ADJUSTL(cline)
                ! there may still be a comment at the end of the line
                JJ=index(cline,'#')
                IF (JJ>1) cline=cline(1:JJ-1)
                IF (cline(1:11)=='BUDGET LIST') THEN
                        budgetname=TRIM(ADJUSTL(cline(12:LEN(cline))))
                        WRITE(*,*) 'found fbl ',budgetname
                ELSEIF (cline(1:4)=='MAIN') THEN
                        LMAIN=.TRUE.
                        mainvar=trim(ADJUSTL(cline(5:LEN(cline))))
                        WRITE(*,*) 'Main variable:',mainvar
                ELSEIF (cline(1:11)=='BEGIN BLOCK') THEN
        ! a new list of budget components is started
        ! first we have to know it's name 
        ! (this is on the same line as the "BEGIN" statement)
                        LMAIN=.FALSE.
                        jcomp=jcomp+1
                        nfield(jcomp)=0
                        compname(jcomp)=trim(ADJUSTL(cline(12:LEN(cline))))
                        WRITE(*,*) 'Found budget component ',jcomp,': ',TRIM(compname(jcomp))
                 ELSEIF (cline(1:8)=='RESIDUAL') THEN
                        resname=trim(ADJUSTL(cline(9:LEN(cline))))
                 ELSEIF (cline(1:8)=='TENDENCY') THEN
                        tendname=trim(ADJUSTL(cline(9:LEN(cline))))
                 ELSEIF (cline(1:3)=='SUM') THEN
                        sumname=trim(ADJUSTL(cline(4:LEN(cline))))
                 ELSEIF (cline(1:4)=='UNIT') THEN
                        LUNIT=.TRUE.
                        cunit=trim(ADJUSTL(cline(5:LEN(cline))))
                        WRITE(*,*)'UNIT:',cunit
                 ELSEIF (cline(1:4)=='COEF') THEN
                        LCOEF=.TRUE.
                        READ(cline(5:LEN(cline)),*) coef
                        WRITE(*,*)'COEF:',coef
                 ELSEIF(LMAIN) THEN
                        ! it's a component for the main tendency
                        jmain=jmain+1
                        ! may be a sign, too
                        JJ=index(TRIM(cline)," ")
                        IF(JJ>0) THEN
                                maincomp(jmain)=cline(1:JJ-1)
                                mainsign(jmain)=ADJUSTL(cline(JJ+1:))
                        ELSE
                                maincomp(jmain)=cline
                                mainsign(jmain)='+'
                        ENDIF
                WRITE(*,*) '     FIELD (',jmain,') : '//mainsign(jmain)//' '//maincomp(jmain)
                ELSE
                        ! it's a budget component
                        nfield(jcomp)=nfield(jcomp)+1
                        jfield=jfield+1
                        ! there may be a sign too
                        JJ=index(TRIM(cline)," ")
                        IF(JJ>0) THEN
                                ! example: cline='FQVTURCONV -'
                                fieldname(jfield)=cline(1:JJ-1)
                                fieldsign(jfield)=ADJUSTL(cline(JJ+1:))
                        ELSE
                                ! example: cline='FQVTURCONV'
                                fieldname(jfield)=cline
                                fieldsign(jfield)='+'
                        ENDIF
                        WRITE(*,*) '     FIELD (',nfield(jcomp),') : '//fieldsign(jfield)//' '//fieldname(jfield)
                ENDIF
        ENDIF
        ENDIF
ENDDO
CLOSE(IFILE)
totfields=jfield
totmain=jmain
totcomp=jcomp
RETURN

END SUBROUTINE PARSE_FBL
