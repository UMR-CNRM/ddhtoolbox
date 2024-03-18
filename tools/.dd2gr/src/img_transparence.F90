subroutine img_transparence(krvb_fond,krvb_pp,popac,krvb)
! --------------------------------------------------------------
! **** *img_transparence* Surimposition d'une couleur en img_transparence sur une autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! krvb_fond: couleur du fond.
! krvb_pp: couleur du premier plan à surimposer.
! popac: opacité du premier plan: 0. si vitre parfaite, 1. si écran parfait.
! En sortie:
! krvb: couleur résultatnte.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
real(kind=8) :: popac,zopac
integer(kind=4) :: krvb_fond(3),krvb_pp(3),krvb(3)
!
!
!-------------------------------------------------
! Simple interpolation linéaire en RVB.
!-------------------------------------------------
!
zopac=max( 0. ,min( 1. ,popac))
krvb=nint(zopac*real(krvb_pp)+(1.-zopac)*real(krvb_fond))
end
