!OPTIONS NODOUBLE
subroutine lited(kul,kdom,cdopt,pdocd,krep)
! --------------------------------------------------------------------------
! **** **  
! --------------------------------------------------------------------------
! Sujet:    LIT ou Ecrit un article de Documentation de domaine: DOCD004, DOCD16324, etc.
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! Methode:
! --------
! Externes: /
! ---------
! Auteur:        2018-04-16, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entrée:
!     kul: unité logique du fichier DDH.
!     kdom: numéro du domaine.
!     cdopt: 'W' si on écrit l'article, 'R' si on le lit.
! En entrée (si cdopt='W') ou en sortie (si cdopt='R'):
!     pdocd: réels 
! En sortie:
!     krep=0 si OK, -1 si documentation de ce domaine inexistante.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
#include"ddhpar.h"
character(len=*), intent(in) :: cdopt
REAL(KIND=8) :: pdocd(jpdoc)
krep=0
!
!-------------------------------------------------
! On écrit le numéro d'article au format I3.3 ou * suivant la valeur de ce numéro.
!-------------------------------------------------
!
if(kdom < 1000) then
  write(clnoma,fmt='(a,i3.3)') 'DOCD',kdom
else
  write(clnum,fmt=*) kdom
  write(clnoma,fmt='(2a)') 'DOCD',trim(adjustl(clnum))
endif
!
!-------------------------------------------------
! Lecture ou écriture.
!-------------------------------------------------
!
if(trim(cdopt) == 'R') then
  !
  !-------------------------------------------------
  ! Lecture.
  !-------------------------------------------------
  !
  call lfacas(kul,clnoma,cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
  if(irep == 0) then
    call lfalecr(kul,clnoma,jpdoc,pdocd,ilong,ierr)
  else
    write(*,fmt=*) 'DDHI/LITED/WARNING: article ',trim(clnoma),' missing.'
    krep=-1
    pdocd=999.999
  endif
else
  !
  !-------------------------------------------------
  ! Ecriture.
  !-------------------------------------------------
  !
  call lfaecrr(kul,clnoma,pdocd,jpdoc)
endif
end
