PROGRAM ddhb_bestlist
! find the most suitable budget list
! for a given input file and main variable
! Original: Alex Deckmyn, Dec. 2007
! --------------------------------
! Basically a simple routine:
! parse the budget file to get all required variables
! then check whether they are available in the input file
! If there is no perfect fit, you may take the best 
! but delete some lines.
! Attention: files of type TxxDELTAP are "pseudo-fields"
! that don't really exist but are calculated by DDHT and DDHI.
! ------------------------------------------------------------
! best to call parse_fbl , so if anything changes in the format
! only one routine is involved
IMPLICIT NONE
CHARACTER (LEN=60) :: lfafile
INTEGER(KIND=4), PARAMETER :: MAXCOMP=50,MAXFIELDS=200
CHARACTER (LEN=50) FBLFILENAME
INTEGER(KIND=4) :: NARG
CHARACTER*13  :: fieldname(MAXFIELDS)
CHARACTER     :: fieldsign(MAXFIELDS),mainsign(MAXCOMP)
CHARACTER (LEN=60) :: compname(MAXCOMP)
CHARACTER (LEN=60) :: resname,tendname,sumname
INTEGER(KIND=4)       :: nfield(MAXCOMP)
CHARACTER*2   :: mainvar,maincomp(MAXCOMP)
CHARACTER*4   :: cmain0,cmain1,cmainm
CHARACTER*13   ::cmainres
CHARACTER*30  :: budgetname
CHARACTER (LEN=3)  :: FLUXCONV
CHARACTER (LEN=30) :: ddhtfile,ddhifile,ddhilist,graphfile,mvfile
CHARACTER (LEN=60) :: liscfile
INTEGER(KIND=4)       :: JJ,KK,totfields,totcomp,totmain,jfield,jcomp
INTEGER(KIND=4)       :: IUNIT=21
CHARACTER (LEN=15)  :: cunit
REAL(KIND=8)          :: mcoef
INTEGER(KIND=4)       :: iechlog
CHARACTER*139 :: cline
LOGICAL       :: lunit,lcoef,lsum

! read from command line: the lfa file and the budget file

CALL parse_fbl(trim(FBLFILENAME),trim(FILEPREFIX),budgetname,compname, &
    & fieldname,fieldsign,nfield,totfields,totcomp,mainvar,maincomp,mainsign, &
    & resname,tendname,sumname,totmain,lunit,cunit,lcoef,mcoef)

CALL lfaouv( 

DO JJ=1,totmain
                
