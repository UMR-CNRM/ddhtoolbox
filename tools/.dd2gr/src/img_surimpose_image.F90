subroutine img_surimpose_image(knx_pp,kny_pp,krvb_pp,popac,ktyps,kloc,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_surimpose_image* Imposition d'une image sur une autre, en transparence.
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
! knx_pp, kny_pp: nombre de pixels en X et Y de l'image premier plan (IPP).
! krvb_pp(3,knx_pp,kny_pp): triplets RVB de IPP.
! popac: opacité du premier plan: 0. vitre parfaite, 1. écran parfait.
! ktyps(4):
!    - si ktyps(1)=0, alors l'opacité est la même pour toutes les couleurs de IPP.
!    - si ktyps(1)=1, alors l'opacité est de popac pour toutes les couleurs de IPP
!      sauf celle donnée en RVB par ktyps(2),ktyps(3),ktyps(4), qui a une opacité de 0..
!      i.e. LE FOND DE IPP EST IGNORE.
!    - si ktyps(1)=2, alors l'opacité est de 1. pour toutes les couleurs de IPP
!      sauf celle donnée en RVB par ktyps(2),ktyps(3),ktyps(4), qui a une opacité de popac.
!      i.e. LE FOND DE IPP EST transparent, TANDIS QUE TOUTES LES AUTRES SONT OPAQUES.
! kloc(3): position de IPP dans l'image de fond (IFOND):
!    - si kloc(1)=0, alors kloc(2)=position X du coin haut gauche de IPP dans IFOND.
!                          kloc(3)=         Y
!    - si kloc(1)=1, IPP est en bas  à gauche de IFOND.
!    - si kloc(1)=2, IPP est en bas  à droite de IFOND.
!    - si kloc(1)=3, IPP est en haut à gauche de IFOND.
!    - si kloc(1)=4, IPP est en haut à droite de IFOND.
!    - si kloc(1)=10, alors kloc(2)=position X du centre de IPP dans IFOND.
!                           kloc(3)=         Y
! kx, ky: nombre de pixels en X et Y de IFOND.
! En entrée/sortie:
! krvb(3,kx,ky): triplets RVB de IFOND.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
integer(kind=4) :: knx_pp,kny_pp,krvb_pp(3,knx_pp,kny_pp),kloc(3),kx,ky,krvb(3,kx,ky),iloc
integer(kind=4) :: ktyps(4)
integer(kind=4) :: iprop,iloc_x,iloc_y,jx,jy,ipp(3),ifond(3),jcoul
real(kind=8) :: popac,zopac
!
!-------------------------------------------------
! Contrôle de faisabilité de la surimposition.
!-------------------------------------------------
!
if(knx_pp > kx.or.kny_pp > ky) then
  print*,'img_surimpose_image/ATTENTION: l''image à surimposer est plus grande que la cible!...'
  if(knx_pp > kx) then
    print*,'	Image à surimposer: ',knx_pp,' pixels en X, or il y en a ',kx,' sur l''image fond.'
  endif
  if(kny_pp > ky) then
    print*,'	Image à surimposer: ',kny_pp,' pixels en Y, or il y en a ',ky,' sur l''image fond.'
  endif
endif
!
!-------------------------------------------------
! Localisation du coin haut gauche de IPP dans IFOND.
!-------------------------------------------------
!
iprop=96
if(kloc(1) == 0) then
  iloc_x=kloc(2)
  iloc_y=kloc(3)
elseif(kloc(1) == 1) then
  iloc_x=kx/iprop
  iloc_y=ky-kny_pp+1
elseif(kloc(1) == 2) then
  iloc_x=kx-knx_pp-kx/iprop
  iloc_y=ky-kny_pp-kx/iprop
elseif(kloc(1) == 3) then
  iloc_x=kx/iprop
  iloc_y=kx/iprop
elseif(kloc(1) == 4) then
  iloc_x=kx-knx_pp-kx/iprop
  iloc_y=kx/iprop
elseif(kloc(1) == 10) then
  iloc_x=kloc(2)-knx_pp/2
  iloc_y=kloc(3)-kny_pp/2
else
  print*,'img_surimpose_image/ERREUR: option kloc(1) inconnue: ',kloc(1)
  call exit(1)
endif
!
!-------------------------------------------------
! Si les coordonnées calculées ont été trop optimistes,
! recalage de IPP au bord de IFOND.
!-------------------------------------------------
!
if(iloc_x < 1) then
  iloc_x=1
elseif(iloc_x+knx_pp-1 > kx) then
  iloc_x=kx-knx_pp+1
endif
if(iloc_y < 1) then
  iloc_y=1
elseif(iloc_y+kny_pp-1 > ky) then
  iloc_y=ky-kny_pp+1
endif
!
!-------------------------------------------------
! Modification du fond par ajout de la transparence.
!-------------------------------------------------
!
do jx=1,knx_pp
  do jy=1,kny_pp
    if(jx+iloc_x-1 > kx .or. jx+iloc_x-1 < 1 .or. jy+iloc_y-1 > ky .or. jy+iloc_y-1 < 1) then
      !
      !-------------------------------------------------
      ! L'image à surimoposer est plus grande que la cible.
      ! Le point courant de l'image source est hors de la cible.
      ! On l'ignore et passe au suivant.
      !-------------------------------------------------
      !
      cycle
    endif
    do jcoul=1,3
      ipp(jcoul)=krvb_pp(jcoul,jx,jy)
      ifond(jcoul)=krvb(jcoul,jx+iloc_x-1,jy+iloc_y-1)
    enddo
    if(ktyps(1) == 0) then
      !
      ! -------------------------------------------------
      ! La transparence est la même partout.
      ! -------------------------------------------------
      !
      zopac=popac
    else
      !
      ! -------------------------------------------------
      ! Transparence différente pour une couleur donnée de IPP.
      ! -------------------------------------------------
      !
      if(ipp(1) /= ktyps(2).or.ipp(2) /= ktyps(3).or.ipp(3) /= ktyps(4)) then
        !
        ! -------------------------------------------------
        ! Le pixel de IPP n'est pas de la couleur remarquable.
        ! -------------------------------------------------
        !
        if(ktyps(1) == 1) then
          zopac=popac
        elseif(ktyps(1) == 2) then
          zopac= 1.
        else
          print*,'img_surimpose_image/ERREUR: ktyps(1)=',ktyps(1)
          call exit(1)
        endif
      else
        !
        ! -------------------------------------------------
        ! Le pixel de IPP est de la couleur remarquable.
        ! -------------------------------------------------
        !
        if(ktyps(1) == 1) then
          zopac= 0.
        elseif(ktyps(1) == 2) then
          zopac=popac
        else
          print*,'img_surimpose_image/ERREUR: ktyps(1)=',ktyps(1)
          call exit(1)
        endif
      endif
    endif
    call img_transparence(ifond,ipp,zopac,ifond)
    do jcoul=1,3
      krvb(jcoul,jx+iloc_x-1,jy+iloc_y-1)=ifond(jcoul)
    enddo
  enddo
enddo
end
