subroutine ote_et_commercial(cde,cds)
! --------------------------------------------------------------
! **** ** Remplace les "&" d'une chaîne par des "+".
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2015-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   cde='Barbade & Floride'
! En sortie:
!   cds='Barbade + Floride'
! --------------------------------------------------------------
#include"implicit_r8i4.h"
character(len=*) :: cde
character(len=*) :: cds
!
!-------------------------------------------------
! On remplace les "&" par des "+" car le langage SVG interprète les "&": s'il y
! a des & dans les titres ou sous-titres, ou légendes, l'interprétation du SVG
! sera impossible..
!-------------------------------------------------
!
cds=cde
do while(index(cds,'&') /= 0)
  ipos=index(cds,'&')
  cds(ipos:ipos)='+'
enddo
end
