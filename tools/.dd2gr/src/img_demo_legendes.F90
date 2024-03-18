subroutine img_demo_legendes
! --------------------------------------------------------------
! **** *img_demo_legendes* Demonstration de légendage.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2005-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Image de sortie.
!-------------------------------------------------
!
integer(kind=4), parameter :: jpx=300
integer(kind=4), parameter :: jpy=200
integer(kind=4) :: irvb_sor(3,jpx,jpy)
!
!-------------------------------------------------
! Définition de la légende à écrire.
!-------------------------------------------------
!
integer(kind=4), parameter :: jpleg=6
character*30 :: cltexte(jpleg) ! item-texte de la légende.
integer(kind=4) :: irvb_palette(3,jpleg) ! couleur associée.
integer(kind=4) :: irvb_fond(3)
integer(kind=4) :: irvb_pp(3)
real(kind=8) zseuil(jpleg)
irvb_fond=190
irvb_pp=0
ileg=0
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=255 ; irvb_palette(3,ileg)=255 ; cltexte(ileg)='Sc'
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=000 ; irvb_palette(3,ileg)=255 ; cltexte(ileg)='Nuages bas'
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=000 ; irvb_palette(3,ileg)=000 ; cltexte(ileg)='Cu cong.'
ileg=ileg+1 ; irvb_palette(1,ileg)=000 ; irvb_palette(2,ileg)=255 ; irvb_palette(3,ileg)=000 ; cltexte(ileg)='Cb'
ileg=ileg+1 ; irvb_palette(1,ileg)=000 ; irvb_palette(2,ileg)=000 ; irvb_palette(3,ileg)=255 ; cltexte(ileg)='Cu hum.'
ileg=ileg+1 ; irvb_palette(1,ileg)=133 ; irvb_palette(2,ileg)=144 ; irvb_palette(3,ileg)=155 ; cltexte(ileg)='Cu'
!
!-------------------------------------------------
! Appel au tracé de la légende.
!-------------------------------------------------
!
call img_legende_alphanumerique(jpx,jpy,irvb_fond,irvb_pp,ileg,irvb_palette,cltexte,irvb_sor)
!
!-------------------------------------------------
! Ecriture de cette image sur un fichier.
!-------------------------------------------------
!
clficpix='legende_alphanumerique.tmp.ppm'
call img_ecr(clficpix,jpx,jpy,irvb_sor)
!
!-------------------------------------------------
! Définition de la légende à écrire.
!-------------------------------------------------
!
irvb_fond=190
irvb_pp=0
ileg=0
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=255 ; irvb_palette(3,ileg)=255 ; zseuil(ileg)=0.
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=000 ; irvb_palette(3,ileg)=255 ; zseuil(ileg)=50.
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=000 ; irvb_palette(3,ileg)=000 ; zseuil(ileg)=150.
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=100 ; irvb_palette(3,ileg)=000 ; zseuil(ileg)=250.
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=150 ; irvb_palette(3,ileg)=000 ; zseuil(ileg)=550.
ileg=ileg+1 ; irvb_palette(1,ileg)=255 ; irvb_palette(2,ileg)=180 ; irvb_palette(3,ileg)=000
clunite='mm/jour'
iseuil=ileg-1
!
!-------------------------------------------------
! Appel au tracé de la légende.
!-------------------------------------------------
!
call img_legende_discrete(jpx,jpy,irvb_fond,irvb_pp,iseuil,irvb_palette,zseuil,clunite,irvb_sor)
!
!-------------------------------------------------
! Ecriture de cette image sur un fichier.
!-------------------------------------------------
!
clficpix='legende_discrete.tmp.ppm'
call img_ecr(clficpix,jpx,jpy,irvb_sor)
end
