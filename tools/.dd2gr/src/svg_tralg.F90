subroutine svg_tralg(p1,p2,cdaxe)
! --------------------------------------------------------------
! **** ** LEGende et Graduation d'un axe de type Date.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   p1, p2: min et max de la date julienne.
!   cdaxe: 'X' ou 'Y'
! En sortie:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
character(len=*) :: cdaxe
real(kind=8) zprinc(jplignes)
real(kind=8) zsec(jplignes)
!
!-------------------------------------------------
! zbarx: nombre maxi de barres-guides pour ne pas trop charger le graphique.
!-------------------------------------------------
!
zbarx=10.
!
!-------------------------------------------------
! Temps écoulé avec diverses unités.
!-------------------------------------------------
!
zjours=abs(p2-p1) ! nombre de jours écoulés.
zan=zjours/365.2425 ! nombre d'années.
zmois=zan*12.
zh=zjours*24.
if(zan > zbarx) then
  !
  !-------------------------------------------------
  ! Beaucoup d'années. On va créer une graduation automatique de réels, dont l'unité est
  ! l'année.
  !-------------------------------------------------
  !
  zmin=(p1-2451544.5)/365.2425+2000. ! différence entre la date julienne et celle du 1.1.2000, convertie en années, puis ajoutée à 2000.
  zmax=(p2-2451544.5)/365.2425+2000. ! idem pour la date finale.
  if(trim(cdaxe) == 'X') then
    zsauv_xmin=rxmin
    zsauv_xmax=rxmax
    rxmin=zmin
    rxmax=zmax
    cglegx='temps (années)'
  else
    zsauv_ymin=rymin
    zsauv_ymax=rymax
    rymin=zmin
    rymax=zmax
    cglegy='temps (années)'
  endif
  !
  !-------------------------------------------------
  ! On crée la graduation.
  !-------------------------------------------------
  !
  call lega(zmin,zmax,rindef,iprinc,zprinc,isec,zsec)
  call tralps(iprinc,zprinc,isec,zsec,cdaxe) ! tracé des lignes principales et secondaires de l'axe.
  !
  !-------------------------------------------------
  ! On remet les valeurs de min et max en unités de dates juliennes, pour
  ! permettre le tracé des courbes dans cette unité.
  ! En effet les fichiers de data sont en dates juliennes.
  !-------------------------------------------------
  !
  if(trim(cdaxe) == 'X') then
    rxmin=zsauv_xmin
    rxmax=zsauv_xmax
  else
    rymin=zsauv_ymin
    rymax=zsauv_ymax
  endif
else
  !
  !-------------------------------------------------
  ! Il n'y a que quelques années ou moins (quelques mois, quelques jours,
  ! quelques heures). Ecriture de l'unité.
  !-------------------------------------------------
  !
  if(trim(cdaxe) == 'X') then
    ixtxt=rxt+rlxt+0.05*rx_legcour
    iytxt=ryt+rlyt+0.7*ry_legx
    clleg=cglegx
    write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
    & ,ixtxt,'" y="' &
    & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',rgtaille_fonte &
    & ,'" text-anchor="left" fill="black" >' &
    & ,trim(clleg),'</text>'
  else
    ixtxt=0.26*rxt
    iytxt=ryt+0.5*rlyt
    clleg=cglegy
    write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,a,i5,a,i5,3a)') '<text x="' &
    & ,ixtxt,'" y="' &
    & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',rgtaille_fonte &
    & ,'" text-anchor="middle" transform="rotate(-90,',ixtxt,', ',iytxt,')" fill="black" >' &
    & ,trim(clleg),'</text>'
  endif
  clsvg_texte=cl_nettoie_blancs(clsvg_txt)
  write(nulsvg,fmt='(9a)') '<!-- Ecriture de la valeur réelle de légende des axes. -->'
  write(nulsvg,fmt='(a)') trim(clsvg_texte)
  !
  !-------------------------------------------------
  ! On va choisir entre 3 façons de graduer:
  ! 1. Graduations principales en années, secondaires en mois.
  ! 2. Graduations principales en mois, secondaires en jours.
  ! 3. Graduations principales en jours, secondaires en heures.
  !-------------------------------------------------
  !
  if(zmois > zbarx) then
    !
    !-------------------------------------------------
    ! Graduations principales en années, secondaires en mois.
    !-------------------------------------------------
    !
    call svg_gradu('sec','mois',p1,p2,cdaxe)
    call svg_gradu('princ','années',p1,p2,cdaxe)
  elseif(zjours > zbarx) then
    !
    !-------------------------------------------------
    ! Graduations principales en mois, secondaires en jours.
    !-------------------------------------------------
    !
    call svg_gradu('sec','jours',p1,p2,cdaxe)
    call svg_gradu('princ','mois',p1,p2,cdaxe)
  else
    !
    !-------------------------------------------------
    ! Graduations principales en jours, secondaires en heures.
    !-------------------------------------------------
    !
    call svg_gradu('sec','heures',p1,p2,cdaxe)
    call svg_gradu('princ','jours',p1,p2,cdaxe)
  endif
endif
end
