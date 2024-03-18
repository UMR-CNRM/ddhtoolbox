subroutine recpol(px,py,pr,pang)
! --------------------------------------------------------------------------
! **** *RECPOL *  Conversion rectangulaire > polaire.
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
! Auteur:           92-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree: px et py coordonnees rectangulaires.
! En sortie: pr et pang rayon et azimuth.
! --------------------------------------------------------------------------

!#include "tsmbkind.h"

#include"implicit_r8i4.h"
real(kind=8) :: PANG
real(kind=8) :: PR
real(kind=8) :: PX
real(kind=8) :: PY
real(kind=8) :: ZINT
real(kind=8) :: ZPI
pr=sqrt(px*px+py*py)
zpi=4.*atan( 1. )
if(px ==  0. ) then
  if(py ==  0. ) then
    pang= 0. 
  elseif(py >  0. ) then
    pang=zpi/ 2. 
  else
    pang=-zpi/ 2. 
  endif
else
  zint=atan(py/px)
  if(px >=  0. ) then
    pang=zint
  elseif(py >=  0. ) then
    pang=zint+zpi
  else
    pang=zint-zpi
  endif
endif
end
