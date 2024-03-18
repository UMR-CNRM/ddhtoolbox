subroutine interpole(pindef,ldextrapolation,px,py,pz,kndta,knx,kny,ptab)
! --------------------------------------------------------------
! **** ** Interpolation grille irrégulière vers régulière.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2009-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pindef: valeur réelle à affecter aux points de grille sans donnée.
!  ldextrapolation: 
!    vrai si remplissage de toutes les valeurs de la grille régulière de sortie.
!    faux si simple pointage des valeurs irrégulières d'entrée,
!  px,py,pz: coordonnées et valeur des données irrégulières.
!  rxmin,rxmax,rymin,rymax: extrêmes de X et Y imposés.
!  kndta: taille de ces données.
!  knx, kny: taille des données régulières de sortie.
! En sortie:
!  ptab(knx,kny): données sur grille régulière.
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8) px(kndta)
real(kind=8) py(kndta)
real(kind=8) pz(kndta)
real(kind=8) ptab(knx,kny)
real(kind=8) ztab(knx,kny)
integer(kind=4) itab(knx,kny)
!
!-------------------------------------------------
! Initialisation à valeur manquante.
!-------------------------------------------------
!
ptab=0.
itab=0
!
!-------------------------------------------------
! On parcourt les données irrégulières et les affecte dans chaque carré.
!-------------------------------------------------
!
if(rxmax == rxmin) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/interpole/ERREUR: rxmin=rxmax: vos données n''ont pas d''extension en X !...'
  write(*,fmt=*) 'rxmin=rxmax=',rxmax
  call exit(1)
endif
if(rymax == rymin) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/interpole/ERREUR: rymin=rymax: vos données n''ont pas d''extension en Y !...'
  write(*,fmt=*) 'rymin=rymax=',rymax
  call exit(1)
endif
do jcourbes=1,kndta
  ix=max(1,min(knx,int((px(jcourbes)-rxmin)/(rxmax-rxmin)*real(knx))+1))
  iy=max(1,min(kny,int((py(jcourbes)-rymin)/(rymax-rymin)*real(kny))+1))
  ptab(ix,iy)=ptab(ix,iy)+pz(jcourbes)
  itab(ix,iy)=itab(ix,iy)+1
enddo
!
!-------------------------------------------------
! On parcourt les données régulières
! et calcule la moyenne dans chaque case.
!-------------------------------------------------
!
do jx=1,knx
  do jy=1,kny
    if(itab(jx,jy) >= 1) then
      ptab(jx,jy)=ptab(jx,jy)/real(itab(jx,jy))
    else
      ptab(jx,jy)=pindef
    endif
  enddo
enddo
if(ldextrapolation) then
  !
  !-------------------------------------------------
  ! On élimine les valeurs manquantes,
  ! en remplissant tous les points de la grille régulière de sortie.
  !-------------------------------------------------
  !
  if(trim(cgextrmeth) == 'CONCENTRIQUE') then
    !
    !-------------------------------------------------
    ! Méthode d'interpolation recherchant
    ! les données disponibles par zones
    ! concentriques autour du point de valeur manquante.
    !-------------------------------------------------
    !
    call extrapole_concentrique(knx,kny,pindef,ptab)
  elseif(trim(cgextrmeth) == 'Y_PUIS_X') then
    !
    !-------------------------------------------------
    ! Méthode d'interpolation recherchant
    ! les données disponibles d'abord
    ! auprès des voisins en Y, puis des voisins en X.
    ! Méthode adaptée aux DDH, dans lesquels la grille
    ! est irrégulière, mais comporte plusieurs valeurs de Y de même X.
    !-------------------------------------------------
    !
    call extrapole_y_puis_x(knx,kny,pindef,ptab)
  else
    write(*,fmt=*) 
    write(*,fmt=*) 'dd2gr/ERREUR: méthode d''interpolation inconnue!...'
    write(*,fmt=*) trim(cgextrmeth)
    call exit(1)
  endif
endif
if(lgliss) then
  !
  !-------------------------------------------------
  ! Lissage.
  !-------------------------------------------------
  !
  if(rgliss > 0.) then
    ibord=0
    if(rgliss > 0.99) then
      !
      !-------------------------------------------------
      ! Le lissage est donné par un entier, qui donne le nombre de points à lisser.
      !-------------------------------------------------
      !
      ixliss=nint(rgliss)
      iyliss=nint(rgliss)
    else
      !
      !-------------------------------------------------
      ! Le lissage est donné comme une fraction du nombre de points en X et Y.
      !-------------------------------------------------
      !
      ixliss=max(0,min(knx,nint(abs(rgliss)*real(knx)/2.)))
      iyliss=max(0,min(kny,nint(abs(rgliss)*real(kny)/2.)))
    endif
  else
    !
    !-------------------------------------------------
    ! rgliss nul. Pas de lissage.
    !-------------------------------------------------
    !
    ibord=1
    ixliss=0
    iyliss=0
  endif
  inbxdiag=2*ixliss+1
  inbydiag=2*iyliss+1
  write(*,fmt=*) '  Lissage: ',inbxdiag,' points en X, ',inbydiag,' points en Y.'
  ztab=ptab
  do jx=1+ibord,knx-ibord
    do jy=1+ibord,kny-ibord
      ipoi=0
      ztab(jx,jy)=0.
      do jx1=max(1,jx-ixliss),min(knx,jx+ixliss)
        do jy1=max(1,jy-iyliss),min(kny,jy+iyliss)
          ipoi=ipoi+1
          ztab(jx,jy)=ztab(jx,jy)+ptab(jx1,jy1)
        enddo
      enddo
      ztab(jx,jy)=ztab(jx,jy)/real(ipoi)
    enddo
  enddo
  ptab=ztab
endif
end
