subroutine ote_zeros_non_sign(cdin,cdout)
! --------------------------------------------------------------
! **** *ote_zeros_non_sign*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2020-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   cdin="0.500"
! En sortie:
!   cdout="0.5"
! --------------------------------------------------------------
! Exemple: 
! --------------------------------------------------------------
!
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
character*(*), intent(in) :: cdin
character*(*), intent(out) :: cdout
ipos=index(cdin,".")
if(ipos == 0) then
  cdout=cdin
else
  il=len_trim(cdin)
  do jcar=len_trim(cdin),1,-1
    if(cdin(jcar:jcar) == '0') then
      il=jcar-1
    else
      exit
    endif
  enddo
  cdout=cdin(1:il)
endif
end
