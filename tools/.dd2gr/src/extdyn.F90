subroutine extdyn(psaut,px,py,kcourbes,kndta,pxmin,pxmax,pymin,pymax)
! --------------------------------------------------------------
! **** ** Détermination des extrêmes de tracé par moyenne + r fois l'écart-type.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   px(kcourbes,kndta), py(kcourbes,kndta): courbes de points.
!   cgextdyn (du module parametres): facteur r multipliant l'écart-type.
! En sortie:
!   pxmin,pxmax,pymin,pymax les extrêmes de tracé.
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8) :: px(kcourbes,kndta)
real(kind=8) :: py(kcourbes,kndta)
!
!-------------------------------------------------
! Calcul de moyenne et écart-type sur toutes les courbes, de façon
! indifférenciée.
!-------------------------------------------------
!
zsomx=0.
zsom2x=0.
zsomy=0.
zom2y=0.
idta=0
do jcourbes=1,kcourbes
  do jdta=1,kndta
    if(px(jcourbes,jdta) /= rindef .and. px(jcourbes,jdta) /= psaut) then
      idta=idta+1
      zsomx=zsomx+px(jcourbes,jdta)
      zsomy=zsomy+py(jcourbes,jdta)
      zsom2x=zsom2x+px(jcourbes,jdta)*px(jcourbes,jdta)
      zsom2y=zsom2y+py(jcourbes,jdta)*py(jcourbes,jdta)
    endif
  enddo
enddo
!
!-------------------------------------------------
! Calcul de moyenne et écart-type.
!-------------------------------------------------
!
if(idta == 0) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/ERREUR: nombre de données nul dans extdyn!...'
  write(*,fmt=*)
  call exit(1)
endif
zmoyx=zsomx/real(idta)
zmoyy=zsomy/real(idta)
zectx=sqrt(max(0.,zsom2x/real(idta)-zmoyx*zmoyx))
zecty=sqrt(max(0.,zsom2y/real(idta)-zmoyy*zmoyy))
!
!-------------------------------------------------
! Lecture du facteur r sur la variable cgextdyn.
!-------------------------------------------------
!
read(cgextdyn,fmt=*) zr
!
!-------------------------------------------------
! Calculs finaux.
!-------------------------------------------------
!
pxmin=zmoyx-zr*zectx
pymin=zmoyy-zr*zecty
end
