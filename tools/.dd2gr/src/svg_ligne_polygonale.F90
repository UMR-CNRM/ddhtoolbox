subroutine svg_ligne_polygonale(pfracx,pfracy,kbris,psaut,cdcoul,ldplusse,cdpoin,pwidth,pseuil)
! --------------------------------------------------------------
! **** *svg_ligne_polygonale* tracé d'une ligne brisée.
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
! pfracx(kbris): valeurs de X (si elles sont < 0. ou > 1. c'est que le point est hors cadre)
! pfracy(kbris): valeurs de Y (si elles sont < 0. ou > 1. c'est que le point est hors cadre)
! psaut: valeur numérique d'une valeur de saut de plume.
! cdcoul: couleur de la courbe.
! cdpoin: type de trait (pointillé, tireté, etc).
! pwidth: largeur de la courbe.
! pseuil: nombre de pixels en dessous duquel deux points de la ligne brisée sont considérés confondus.
!
! En sortie:
!	Ecriture sur le fichier SVG d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
real(kind=8), intent(in) :: pfracx(kbris),pfracy(kbris)
integer(kind=4), intent(in) :: kbris
real(kind=8), intent(in) :: psaut
character(len=*), intent(in) :: cdcoul
character(len=*), intent(in) :: cdpoin
real(kind=8), intent(in) :: pwidth
logical, intent(in) :: ldplusse

character(len=6000000) :: clligne,clligne2
!
!-------------------------------------------------
! Gestion des pointillés éventuels.
!-------------------------------------------------
!
if(trim(cdpoin) == trim(cgindef) .or. trim(cdpoin) == 'non') then
  cldash=' '
else
  cldash='; stroke-dasharray: '//trim(cdpoin)
endif
!
!-------------------------------------------------
! Ecriture d'une ligne polygonale.
!-------------------------------------------------
!
write(cldeb,fmt='(3a,f8.2,100a)') '<polyline style="fill:none;stroke:',trim(cdcoul),';stroke-linecap:round;stroke-width:',pwidth,trim(cldash),'" points="'
cldeb=cl_nettoie_blancs(cldeb)
clligne=cldeb
iecr=0 ! nombre de points écrits sur une ligne polygonale.
zxprec=0.
zyprec=0.
!
!-------------------------------------------------
! Boucle sur les kbris points de la ligne brisée.
!-------------------------------------------------
!
write(*,fmt=*) '    tracé de ligne polygonale : ',kbris,' points...'
ityp_prec=-9
do jbris=1,kbris
  if(abs(pfracx(jbris)-psaut) < 1.e-4 .or. abs(pfracy(jbris)-psaut) < 1.e-4) then
     !
     !-------------------------------------------------
     ! Saut de plume.
     !-------------------------------------------------
     !
     ityp=-9
  elseif(pfracx(jbris) < 0. .or. pfracx(jbris) > 1. .or. pfracy(jbris) < 0. .or. pfracy(jbris) > 1.) then
     !
     !-------------------------------------------------
     ! Hors cadre.
     !-------------------------------------------------
     !
     ityp=0
  else
     !
     !-------------------------------------------------
     ! Cas général.
     !-------------------------------------------------
     !
     ityp=1
  endif
  !
  !-------------------------------------------------
  ! llsuis: vrai si le point suivant est un saut ou un hors cadre.
  !-------------------------------------------------
  !
  ibris=min(kbris,jbris+1)
  llsuis=abs(pfracx(ibris)-psaut) < 1.e-4 .or. abs(pfracy(ibris)-psaut) < 1.e-4 .or. pfracx(ibris) < 0. .or. pfracx(ibris) > 1. .or. pfracy(ibris) < 0. .or. pfracy(ibris) > 1.
  if(ityp == 1) then
    !
    !-------------------------------------------------
    ! Cas général: le point courant est dans le cadre de tracé.
    !-------------------------------------------------
    !
    if(jbris > 1) then
      !
      !-------------------------------------------------
      ! On n'est pas au premier point.
      !-------------------------------------------------
      !
      if(ityp_prec == 0) then
        !
        !-------------------------------------------------
        ! Ce point est dans le cadre, le précédent est hors cadre.
        ! On calcule où le segment de droite allant du précédent P au courant
        ! intersecte le bord du domaine. Appelons I ce point intersection.
        !-------------------------------------------------
        !
        call svg_intersecte(pfracx(jbris),pfracy(jbris),pfracx(jbris-1),pfracy(jbris-1),zfracx_intersection,zfracy_intersection)
        !
        !-------------------------------------------------
        ! Ajout du point I à la ligne polygonale.
        !-------------------------------------------------
        !
        zx1=rxt+zfracx_intersection*rlxt
        zy1=ryt+(1.-zfracy_intersection)*rlyt
        if(abs(zx1-zxprec)+abs(zy1-zyprec) > pseuil .or. llsuis) then
          zxprec=zx1
          zyprec=zy1
          write(clligne2,fmt='(2a,i5,a,i5)') trim(clligne),' ',nint(zx1),', ',nint(zy1)
          clligne=clligne2
          iecr=iecr+1
          if(iecr == 1 .and. ldplusse) call plusse(zx1,zy1,cdcoul)
        endif
      endif
    endif
    !
    !-------------------------------------------------
    ! Ajout du point courant à la ligne polygonale.
    !-------------------------------------------------
    !
    zx1=rxt+pfracx(jbris)*rlxt
    zy1=ryt+(1.-pfracy(jbris))*rlyt
    if(abs(zx1-zxprec)+abs(zy1-zyprec) > pseuil .or. llsuis) then
      zxprec=zx1
      zyprec=zy1
      write(clligne2,fmt='(2a,i5,a,i5)') trim(clligne),' ',nint(zx1),', ',nint(zy1)
      clligne=clligne2
      iecr=iecr+1
      if(iecr == 1 .and. ldplusse) call plusse(zx1,zy1,cdcoul)
    endif
  elseif(ityp == 0) then
    !
    !-------------------------------------------------
    ! Hors cadre.
    !-------------------------------------------------
    !
    if(jbris == 1) then
      !
      !-------------------------------------------------
      ! On est au premier point, et il est hors cadre. On ignore ce point.
      !-------------------------------------------------
      !
    else
      !
      !-------------------------------------------------
      ! On n'est pas au premier point.
      !-------------------------------------------------
      !
      if(ityp_prec == 1) then
        !
        !-------------------------------------------------
        ! Ce point est hors cadre, mais pas le précédent.
        ! On calcule où le segment de droite allant du précédent P au courant
        ! intersecte le bord du domaine. Appelons I ce point intersection.
        !-------------------------------------------------
        !
        call svg_intersecte(pfracx(jbris-1),pfracy(jbris-1),pfracx(jbris),pfracy(jbris),zfracx_intersection,zfracy_intersection)
        !
        !-------------------------------------------------
        ! Ajout du point I à la ligne polygonale.
        !-------------------------------------------------
        !
        zx1=rxt+zfracx_intersection*rlxt
        zy1=ryt+(1.-zfracy_intersection)*rlyt
        if(abs(zx1-zxprec)+abs(zy1-zyprec) > pseuil .or. llsuis) then
          zxprec=zx1
          zyprec=zy1
          write(clligne2,fmt='(2a,i5,a,i5)') trim(clligne),' ',nint(zx1),', ',nint(zy1)
          clligne=clligne2
          iecr=iecr+1
          if(iecr == 1 .and. ldplusse) call plusse(zx1,zy1,cdcoul)
        endif
        !
        !-------------------------------------------------
        ! On lève la plume.
        !-------------------------------------------------
        !
        clligne=trim(clligne)//'" />'
        if(iecr > 0) write(nulsvg,fmt='(a)') trim(clligne)
        clligne=cldeb
        iecr=0
        zxprec=0
        zyprec=0
      else
        !
        !-------------------------------------------------
        ! Ce point est hors cadre, le précédent aussi. On ignore le point courant.
        !-------------------------------------------------
        !
      endif
    endif
  else
    !
    !-------------------------------------------------
    ! Saut de plume.
    !-------------------------------------------------
    !
    if(jbris > 1) then
      if(ityp_prec /= -9) then
        !
        !-------------------------------------------------
        ! Le point courant est un saut de plume, et le précédent ne l'était pas.
        ! On ferme la ligne courante, et on en ouvre une nouvelle.
        !-------------------------------------------------
        !
        clligne=trim(clligne)//'" />'
        if(iecr > 0) write(nulsvg,fmt='(a)') trim(clligne)
        clligne=cldeb
        iecr=0
        zxprec=0
        zyprec=0
      endif
    endif
  endif
  ityp_prec=ityp
enddo ! jbris
!
!-------------------------------------------------
! Fermeture de la ligne polygonale.
!-------------------------------------------------
!
if(iecr > 0) then
  !
  !-------------------------------------------------
  ! Au moins un couple (x,y) a été écrit sur la ligne polygonale.
  ! On la ferme et on l'écrit sur le SVG.
  !-------------------------------------------------
  !
  clligne=trim(clligne)//'" />'
  write(nulsvg,fmt='(a)') trim(clligne)
endif
end
