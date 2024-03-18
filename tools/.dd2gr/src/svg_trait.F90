subroutine svg_trait(kul,px1,px2,py1,py2,pwidth,cdcoul,cdpoin)
! --------------------------------------------------------------
! **** Ecriture d'un trait sur un fichier SVG.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
!
use parametres, only : cgindef
#include"implicit_r8i4.h"
character(len=*) :: cdcoul
character(len=*) :: cdpoin

write(clx1,fmt=*) px1 ; clx1=adjustl(adjustr(clx1))
write(clx2,fmt=*) px2 ; clx2=adjustl(adjustr(clx2))
write(cly1,fmt=*) py1 ; cly1=adjustl(adjustr(cly1))
write(cly2,fmt=*) py2 ; cly2=adjustl(adjustr(cly2))
!
!-------------------------------------------------
! Gestion des pointillés.
!-------------------------------------------------
!
if(trim(cdpoin) == trim(cgindef)) then
  !
  !-------------------------------------------------
  ! Pas de pointillé.
  !-------------------------------------------------
  !
  cldash=' '
else
  !
  !-------------------------------------------------
  ! Pontillé.
  !-------------------------------------------------
  !
  cldash='; stroke-dasharray: '//trim(cdpoin)//';'
endif

write(kul,fmt='(11a,f5.2,100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1),'" x2="',trim(clx2) &
& ,'" y2="',trim(cly2),'" style="stroke: ',trim(cdcoul) &
& ,'; stroke-width: ',pwidth,'; stroke-linecap: round',trim(cldash),'" />'
end
