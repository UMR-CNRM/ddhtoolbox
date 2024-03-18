subroutine recpol(px,py,pr,pang)
! ---------------------------------------------------------------------------
! **** *RECPOL *  Conversion rectangulaire > polaire.
! ---------------------------------------------------------------------------
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
! ---------------------------------------------------------------------------
! En entree: px et py coordonnees rectangulaires.
! En sortie: pr et pang (radians) rayon et azimuth.
! ---------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)

pr=sqrt(px*px+py*py)
zpi=4.*atan(1.)
if(px == 0.) then
  if(py == 0.) then
    pang=0.
  elseif(py > 0.) then
    pang=zpi/2.
  else
    pang=-zpi/2.
  endif
else
  zint=atan(py/px)
  if(px >= 0.) then
    pang=zint
  elseif(py >= 0.) then
    pang=zint+zpi
  else
    pang=zint-zpi
  endif
endif
end
