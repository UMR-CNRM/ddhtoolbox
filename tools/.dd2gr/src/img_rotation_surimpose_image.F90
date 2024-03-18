subroutine img_rotation_surimpose_image(knx_pp &
&,kny_pp,krvb_pp,kx_cible,ky_cible,pang,knx_cible,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_rotation_surimpose_image* Rotation/interpolation d'une image puis surimposition sur une autre, en transparence.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2012-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! knx_pp, kny_pp: nombre de pixels en X et Y de l'image premier plan (IPP).
! krvb_pp(3,knx_pp,kny_pp): triplets RVB de IPP.
! kx_cible: position en X du centre de IPP sur l'image IFOND.
! ky_cible: position en Y du centre de IPP sur l'image IFOND.
! pang: angle en degrés dont l'image doit être tournée. Angle positif: rotation sens trigonométrique.
! knx_cible: longueur en pixels en X qu'aurait l'image cible si elle n'était pas tournée.
!	* si knx_cible=knx_pp, l'image sera seulement tournée.
!	* si knx_cible /= knx_pp, l'image sera tournée et interpolée vers une autre résolution.
! kx, ky: nombre de pixels en X et Y de IFOND.
! En entrée/sortie:
! krvb(3,kx,ky): triplets RVB de IFOND.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
integer(kind=4) :: knx_pp,kny_pp,krvb_pp(3,knx_pp,kny_pp),kx,ky,krvb(3,kx,ky),iloc
integer(kind=4) :: iprop,iloc_x,iloc_y,jx,jy,ipp(3),ifond(3),jcoul
real(kind=8) :: zopac,pang
!
! On boucle sur l'image d'entrée.
!
do jx=1,knx_pp
  do jy=1,kny_pp
    ! Pour ce pixel P on cherche son image P' dans l'image tournée et anamorphosée.
    !
    ! Expression des coordonnées de P en polaire, dans le repère de l'image.
    zx_entree=real(jx-knx_pp/2)
    zy_entree=real(jy-kny_pp/2)
    call recpol(zx_entree,zy_entree,zr,ztheta)
    ! On tourne de pang, puisqu'on cherche les coordonnées de P'.
    ztheta=ztheta+pang*atan(1.)/45.
    ! Le rayon est multiplié par le rapport d'aspect demandé par l'utilisateur.
    zr=zr*real(knx_cible)/real(knx_pp)
    zdx=zr*cos(ztheta)
    zdy=zr*sin(ztheta)
    ! Coordonnées-pixels de P' sur l'image de sortie.
    ix_cible=kx_cible+nint(zdx)
    iy_cible=ky_cible+nint(zdy)
    if(ix_cible >= 1 .and. ix_cible <= kx .and. iy_cible >= 1 .and. iy_cible <= ky) then
      ! P' est bien sur l'image de sortie. On l'y écrit.
      do jc=1,3
        krvb(jc,ix_cible,iy_cible)=krvb_pp(jc,jx,jy)
      enddo
    endif
  enddo
enddo
end
