subroutine arrr(px,kncs,px2)
! --------------------------------------------------------------------------
! **** *arrr*  - Arrondi de reel.
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
! Auteur:  J.M.Piriou
! -------
! Original : 92-02
! ----------
! Modifications:
! --------------------------------------------------------------------------
! En Entree       :
! px          : reel a convertir
! kncs        : nombre de chiffres significatifs desires
! En Sortie       :
! px2         : reel arrondi a kncs chiffres significatifs.
! --------------------------------------------------------------------------
#include"implicit_r8i4.h"
if(kncs < 1.or.kncs > 10) then
  ! Cas nombre de chiffres significatifs errone
  px2=0.
elseif(px == 0.) then
  ! Cas nombre nul
  px2=0.
else
  ! Cas general
  zx=px
  if(zx < 0.) then
    zx=-zx
    isign=-1
  else
    isign=1
  endif
  zlog=log(zx)/log(10.)
  if(zlog < 0.) then
    ilog=int(zlog)-1
  else
    ilog=int(zlog)
  endif
  ! On calcule la mantisse de zx, soit un nombre
  ! zmant tel que 1.<=zmant<10.
  zmant=zx/10.**float(ilog)
  if(zmant >= 100.) then
    print*,'ERREUR zmant=',zmant
    stop
  elseif(zmant >= 10.) then
    zmant=zmant/10.
    ilog=ilog+1
  elseif(zmant < .1) then
    print*,'ERREUR zmant=',zmant
    stop
  elseif(zmant < 1.) then
    zmant=zmant*10.
    ilog=ilog-1
  endif
  ! On ne conserve que kncs chiffres significatifs
  z10=10.**float(kncs-1)
  zmant=int(zmant*z10+.5)/z10
  px2=isign*zmant*10.**float(ilog)
endif
end
