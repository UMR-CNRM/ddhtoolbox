subroutine img_bordure(krvb,kx,ky,krvb_bord)
! --------------------------------------------------------------
! **** *img_bordure*
! --------------------------------------------------------------
! Sujet:
!	Ajout d'une bordure à l'image, de couleur spécifiée.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2003-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree/sortie:
!	krvb(3,kx,ky): image RVB.
! En entree:
!	kx,ky dimension de l'image.
!	krvb_bord: couleur du bord dont il faut entourer l'image.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
real(kind=8) :: pxmin,pxmax,zfrac
integer(kind=4) :: krvb_bord(3),kx,ky,krvb(3,kx,ky),jx,jy,jc
!
!-------------------------------------------------
! Bordure.
!-------------------------------------------------
!
do jx=1,kx
  do jc=1,3
    krvb(jc,jx,1)=krvb_bord(jc)
    krvb(jc,jx,ky)=krvb_bord(jc)
  enddo
enddo
do jy=1,ky
  do jc=1,3
    krvb(jc,1,jy)=krvb_bord(jc)
    krvb(jc,kx,jy)=krvb_bord(jc)
  enddo
enddo
end
