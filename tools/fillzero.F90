subroutine fillzero(cdent,cdsor)
! --------------------------------------------------------------
! **** *fillzero* Remplacement des blancs de gauche par des zéros.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! Exemples:
! cdent='   -233.07'  ==> cdsor='-000233.07'
! cdent='    233.07'  ==> cdsor='0000233.07'
! cdent='    856   '  ==> cdsor='0000856   '
! cdent='   -856   '  ==> cdsor='-000856   '
! --------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: JCAR
character*(*) cdent,cdsor
cdsor=' '
do jcar=1,len_trim(cdent)
  if(cdent(jcar:jcar) == ' ') then
    cdsor(jcar:jcar)='0'
  elseif(cdent(jcar:jcar) == '-') then
    if(cdent(jcar-1:jcar-1) == ' ') then
      cdsor(jcar:jcar)='0'
      cdsor(1:1)='-'
    else
      cdsor(jcar:jcar)=cdent(jcar:jcar)
    endif
  else
    cdsor(jcar:jcar)=cdent(jcar:jcar)
  endif
enddo
end
