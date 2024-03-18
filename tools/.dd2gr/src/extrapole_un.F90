subroutine extrapole_un(knx,kny,ptab,knx2,kny2,ptab2)
! --------------------------------------------------------------
! **** ** Extrapolation de un demi rectangle à gauche, à droite, en haut, en bas.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  knx,kny: dimension du tableau ptab.
!  knx2,kny2: dimensions du tableau ptab2.
! En sortie:
!  tableau ptab2 extrapolé.
! --------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8) ptab(knx,kny)
real(kind=8) ptab2(knx2,kny2)
!
!-------------------------------------------------
! 4 coins. Extrapolation.
!-------------------------------------------------
!
ptab2(1,1)=2.*ptab(1,1)-ptab(2,2)
ptab2(knx2,kny2)=2.*ptab(knx2-1,kny2-1)-ptab(knx2-2,kny2-2)
ptab2(knx2,1)=2.*ptab(knx2-1,1)-ptab(knx2-2,2)
ptab2(1,kny2)=2.*ptab(1,kny2-1)-ptab(2,kny2-2)
!
!-------------------------------------------------
! Bords, hors coins. Interpolation et extrapolation.
!-------------------------------------------------
!
do jx=2,knx2-1
  !
  ! Horizontal bas.
  !
  zv1=0.5*(ptab(jx-1,1)+ptab(jx,1))
  zv2=0.5*(ptab(jx-1,2)+ptab(jx,2))
  ptab2(jx,1)=2.*zv1-zv2
  !
  ! Horizontal haut.
  !
  zv1=0.5*(ptab(jx-1,kny)+ptab(jx,kny))
  zv2=0.5*(ptab(jx-1,kny-1)+ptab(jx,kny-1))
  ptab2(jx,kny2)=2.*zv1-zv2
enddo
do jy=2,kny2-1
  !
  ! Vertical gauche.
  !
  zv1=0.5*(ptab(1,jy-1)+ptab(1,jy))
  zv2=0.5*(ptab(2,jy-1)+ptab(2,jy))
  ptab2(1,jy)=2.*zv1-zv2
  !
  ! Vertical droit.
  !
  zv1=0.5*(ptab(knx,jy-1)+ptab(knx,jy))
  zv2=0.5*(ptab(knx,jy-1)+ptab(knx,jy))
  ptab2(knx2,jy)=2.*zv1-zv2
enddo
!
!-------------------------------------------------
! Intérieur. Interpolation sur 4 points.
!-------------------------------------------------
!
do jx=2,knx2-1
  do jy=2,kny2-1
    ptab2(jx,jy)=0.25*(ptab(jx-1,jy-1)+ptab(jx,jy-1)+ptab(jx-1,jy)+ptab(jx,jy))
  enddo
enddo
end
