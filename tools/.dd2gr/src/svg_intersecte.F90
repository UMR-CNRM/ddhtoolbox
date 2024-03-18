subroutine svg_intersecte(pxbon,pybon,pxhors,pyhors,pix,piy)
! --------------------------------------------------------------
! **** *svg_intersecte* Détermination de l'intersection d'un segment avec le cadre de tracé.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2015-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! pxbon,pybon: coordonnées du "bon" point B: celui qui est dans le cadre de tracé.
! pxhors,pyhors: coordonnées du point H hors du cadre de tracé.
!
! En sortie:
! pix,piy: coordonnées du point I intersection.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! On calcule l'intersection du segment BH avec les 4 droites suivantes:
! y=1
! y=0
! x=1
! x=0
!-------------------------------------------------
!
if(pybon == pyhors) then
  !
  !-------------------------------------------------
  ! Cas trivial: le segment BH est horizontal.
  !-------------------------------------------------
  !
  if(pxhors >= 1.) then
    pix=1.
    piy=pybon
  else
    pix=0.
    piy=pybon
  endif
elseif(pxbon == pxhors) then
  !
  !-------------------------------------------------
  ! Cas trivial: le segment BH est vertical.
  !-------------------------------------------------
  !
  if(pyhors >= 1.) then
    pix=pxbon
    piy=1.
  else
    pix=pxbon
    piy=0.
  endif
else
  !
  !-------------------------------------------------
  ! Cas général: le segment BH est oblique.
  ! Calcul des 4 points d'intersection.
  !-------------------------------------------------
  !
  !
  ! Avec y=0.
  !
  zxy0=pxhors-pyhors*(pxbon-pxhors)/(pybon-pyhors)
  !
  ! Avec y=1.
  !
  zxy1=pxhors+(1.-pyhors)*(pxbon-pxhors)/(pybon-pyhors)
  !
  ! Avec x=0.
  !
  zyx0=pyhors-pxhors*(pybon-pyhors)/(pxbon-pxhors)
  !
  ! Avec x=1.
  !
  zyx1=pyhors+(1.-pxhors)*(pybon-pyhors)/(pxbon-pxhors)
  !
  !-------------------------------------------------
  ! De ces 4 points on garde celui situé sur le périmètre du rectangle-bord de
  ! tracé, i.e. dont les coordonnées sont entre 0. et 1. et situés à l'intérieur
  ! du segment BH.
  !-------------------------------------------------
  !
  zeps=1.e-4
  !
  !-------------------------------------------------
  ! Test inégalités strictes.
  !-------------------------------------------------
  !
  if(zxy0 > 0. .and. zxy0 < 1. .and. (zxy0-pxbon)*(zxy0-pxhors)/(pxbon-pxhors)**2 < -zeps) then
    !
    !-------------------------------------------------
    ! y=0.
    !-------------------------------------------------
    !
    pix=zxy0
    piy=0.
  elseif(zxy1 > 0. .and. zxy1 < 1. .and. (zxy1-pxbon)*(zxy1-pxhors)/(pxbon-pxhors)**2 < -zeps) then
    !
    !-------------------------------------------------
    ! y=1.
    !-------------------------------------------------
    !
    pix=zxy1
    piy=1.
  elseif(zyx0 > 0. .and. zyx0 < 1. .and. (zyx0-pybon)*(zyx0-pyhors)/(pybon-pyhors)**2 < -zeps) then
    !
    !-------------------------------------------------
    ! x=0.
    !-------------------------------------------------
    !
    pix=0.
    piy=zyx0
  elseif(zyx1 > 0. .and. zyx1 < 1. .and. (zyx1-pybon)*(zyx1-pyhors)/(pybon-pyhors)**2 < -zeps) then
    !
    !-------------------------------------------------
    ! x=1.
    !-------------------------------------------------
    !
    pix=1.
    piy=zyx1
  !
  !-------------------------------------------------
  ! Test inégalités ou égalités, au cas où le point intersection serait à
  ! l'extrémité du segment.
  !-------------------------------------------------
  !
  elseif(zxy0 >= 0. .and. zxy0 <= 1. .and. (zxy0-pxbon)*(zxy0-pxhors)/(pxbon-pxhors)**2 <= 0.) then
    !
    !-------------------------------------------------
    ! y=0.
    !-------------------------------------------------
    !
    pix=zxy0
    piy=0.
  elseif(zxy1 >= 0. .and. zxy1 <= 1. .and. (zxy1-pxbon)*(zxy1-pxhors)/(pxbon-pxhors)**2 <= 0.) then
    !
    !-------------------------------------------------
    ! y=1.
    !-------------------------------------------------
    !
    pix=zxy1
    piy=1.
  elseif(zyx0 >= 0. .and. zyx0 <= 1. .and. (zyx0-pybon)*(zyx0-pyhors)/(pybon-pyhors)**2 <= 0.) then
    !
    !-------------------------------------------------
    ! x=0.
    !-------------------------------------------------
    !
    pix=0.
    piy=zyx0
  elseif(zyx1 >= 0. .and. zyx1 <= 1. .and. (zyx1-pybon)*(zyx1-pyhors)/(pybon-pyhors)**2 <= 0.) then
    !
    !-------------------------------------------------
    ! x=1.
    !-------------------------------------------------
    !
    pix=1.
    piy=zyx1
  elseif(pxbon == 0. .or. pxbon == 1. .or. pybon == 0. .or. pybon == 1.) then
    pix=pxbon
    piy=pybon
  else
    write(*,fmt=*)
    write(*,fmt=*) 'intersecte/ERREUR: aucun des 4 points retenu !...'
    write(*,fmt=*) zxy0,zxy1,zyx0,zyx1
    write(*,fmt=*) 'pxbon,pxhors = ',pxbon,pxhors
    write(*,fmt=*) 'pybon,pyhors = ',pybon,pyhors
    write(*,fmt=*)
    call exit(1)
  endif
endif
end
