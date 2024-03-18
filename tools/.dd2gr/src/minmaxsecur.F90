subroutine minmaxsecur
! --------------------------------------------------------------
! **** ** Garantit que le max soit supérieur au min.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2010-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
if(rcmin == 0.) then
  if(rcmax <= 0.) then
    rcmax=1.
    llmodif=.true.
  else
    llmodif=.false.
  endif
elseif(abs(rcmax/rcmin-1.) < 1.e-8 .or. rcmax <= rcmin) then
  if(rcmin > 0.) then
    rcmax=rcmin*1.01
    llmodif=.true.
  else
    rcmax=rcmin*0.99
    llmodif=.true.
  endif
else
  llmodif=.false.
endif
if(llmodif) then
  write(*,fmt=*) '  [(min et max trop voisins entre eux) ou (min >= max)] ==> valeur du max forcée à ',rcmax
endif
end
