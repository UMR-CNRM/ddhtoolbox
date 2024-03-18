subroutine poif(pxtrac,pytrac,pxextf,pyextf,pxempd,pyempd,pxempg,pyempg)
! --------------------------------------------------------------------------
! **** *poif* Calcul de la pointe d'une flèche.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:         2014-12, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree: 
!   pxtrac,pytrac coordonnées de la base de la flèche.
!   pxextf,pyextf coordonnées de la pointe de la flèche.
! En sortie:
!   pxempd,pyempd coordonnées du bout droit de la pointe de la flèche.
!   pxempg,pyempg coordonnées du bout gauche de la pointe de la flèche.
! --------------------------------------------------------------------------
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Coordonnées du vecteur-flèche.
!-------------------------------------------------
!
zxvec=pxextf-pxtrac
zyvec=-(pyextf-pytrac)
!
!-------------------------------------------------
! Ce vecteur en polaire.
!-------------------------------------------------
!
call recpol(zxvec,zyvec,zr,zang)
!
!-------------------------------------------------
! Angle de la pointe.
!-------------------------------------------------
!
zpi=4.*atan( 1. )
zpointu=0.8*zpi ! angle de la pointe. Plus cet angle est proche de pi et plus la pointe est aiguë.
zang_d=zang-zpointu
zang_g=zang+zpointu
!
!-------------------------------------------------
! Taille de la pointe.
!-------------------------------------------------
!
zfrap=0.25 ! fraction de pointe: rapport entre la taille de la pointe et la longueur de la flèche.
!
!-------------------------------------------------
! Point droit.
!-------------------------------------------------
!
pxempd=pxextf+zfrap*zr*cos(zang_d)
pyempd=pyextf-zfrap*zr*sin(zang_d)
!
!-------------------------------------------------
! Point gauche.
!-------------------------------------------------
!
pxempg=pxextf+zfrap*zr*cos(zang_g)
pyempg=pyextf-zfrap*zr*sin(zang_g)
end
