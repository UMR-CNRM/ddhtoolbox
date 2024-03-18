subroutine img_trait(ktabx0,ktaby0 &
&,ktabx1,ktaby1,kpix_largeur,krvb_trait,krvb,kximage,kyimage)
! --------------------------------------------------------------
! **** *img_trait* Tracé d'un trait de largeur donnée, d'un point A à un point B.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!		Pour chaque point à l'intérieur du rectangle dont une diagonale
!		est le segment [A,B] allongé de 2*kpix_largeur, on calcule la distance
!		au segment [A,B]. Si cette distance est inférieure à kpix_largeur,
!		on affecte au point la couleur krvb_trait.
! Externes:
! Auteur:   2004-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entrée:
!	ktabx0,ktaby0: coordonnées pixels du point A.
!	ktabx1,ktaby1: coordonnées pixels du point B.
!	kpix_largeur: largeur du trait en pixels.
!	krvb_trait(3): couleur du trait désiré.
! En entrée/sortie:
!	krvb(3,kximage,kyimage): image sur laquelle tracer le trait.
! --------------------------------------------------------------
!
#include"implicit_r8i4.h"
integer(kind=4), intent(in) :: krvb_trait(3)
integer(kind=4), intent(inout) :: krvb(3,kximage,kyimage)
!
!-------------------------------------------------
! Coin bas gauche du rectangle dont une diagonale
! est le segment [A,B] allongé de kpix_largeur.
!-------------------------------------------------
!
ixbg=max(1,min(ktabx0-kpix_largeur/2,ktabx1-kpix_largeur/2))
iybg=max(1,min(ktaby0-kpix_largeur/2,ktaby1-kpix_largeur/2))
!
!-------------------------------------------------
! Coin haut droit du rectangle dont une diagonale
! est le segment [A,B] allongé de kpix_largeur.
!-------------------------------------------------
!
ixhd=min(kximage,max(ktabx0+kpix_largeur/2,ktabx1+kpix_largeur/2))
iyhd=min(kyimage,max(ktaby0+kpix_largeur/2,ktaby1+kpix_largeur/2))
!
!-------------------------------------------------
! Equation de la diagonale D: sin(theta)x-cos(theta)*y=b.
!-------------------------------------------------
!
zxpente=real(ktabx1-ktabx0)
zypente=real(-ktaby1+ktaby0)
call recpol(zxpente,zypente,zr,ztheta)
zb=sin(ztheta)*(real(ktabx0)-0.5)-cos(ztheta)*(real(-ktaby0)-0.5)
zpi=4.*atan(1.)
!
!-------------------------------------------------
! Boucle sur tous les points de ce rectangle.
!-------------------------------------------------
!
do jx=ixbg,ixhd
  do jy=iybg,iyhd
    !
    !-------------------------------------------------
    ! Coordonnées du point courant C.
    !-------------------------------------------------
    !
    zx=real(jx)-0.5
    zy=-real(jy)+0.5
    !
    !-------------------------------------------------
    ! Equation de la droite E passant par C, perpendiculaire à D.
    !-------------------------------------------------
    !
    ztheta_per=ztheta+0.5*zpi
    zb_per=sin(ztheta_per)*zx-cos(ztheta_per)*zy
    !
    !-------------------------------------------------
    ! Intersection I de D et E.
    !-------------------------------------------------
    !
    zxinter=(-zb*cos(ztheta_per)+zb_per*cos(ztheta))
    zyinter=(sin(ztheta)*zb_per-sin(ztheta_per)*zb)
    !
    !-------------------------------------------------
    ! Distance de C à la droite D: c'est la distance
    ! de C à I.
    !-------------------------------------------------
    !
    zdist=sqrt((zx-zxinter)**2+(zy-zyinter)**2)
    idist=nint(zdist)
    !
    !-------------------------------------------------
    ! On compare la distance zdist à la largeur
    ! en pixels demandée, multipliée par un facteur zfact
    ! très proche de 1 lorsque
    ! theta = 0 ou pi/2 ou pi ou 3*pi/2,
    ! et proche de 1/2 lorsque
    ! theta est proche des premières bissectrices.
    ! En effet si toutes les directions de droites
    ! sont dans la nature, les pixels, eux,
    ! sont toujours alignés horizontalement et verticalement.
    ! Ne pas tenir compte de cet effet zfact reviendrait
    ! donc à tracer des traits plus gras pour les droites proches
    ! en direction des premières bissectrices que pour celles proches
    ! des axes de X ou Y.
    !-------------------------------------------------
    !
    zfact=1.1/(1.+abs(sin(2.*ztheta)))
    if(zdist < real(kpix_largeur)*zfact) then
      !
      !-------------------------------------------------
      ! Le point courant C est à une distance du segment
      ! inférieure à kpix_largeur/2. On le colorie.
      !-------------------------------------------------
      !
      do jc=1,3
        krvb(jc,jx,jy)=krvb_trait(jc)
      enddo
    endif
  enddo
enddo
end
