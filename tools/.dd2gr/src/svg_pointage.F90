subroutine svg_pointage(pfracx,pfracy,kbris,pindef,cdcoul,prayon)
! --------------------------------------------------------------
! **** *svg_pointage* tracé de disques colorés.
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
! pindef: valeur numérique d'une valeur manquante.
! cdcoul: couleur du disque.
! prayon: rayon du disque.
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
real(kind=8), intent(in) :: pindef
character(len=*), intent(in) :: cdcoul
real(kind=8), intent(in) :: prayon

character(len=6000000) :: clligne,clligne2
!
!-------------------------------------------------
! Tracé d'un disque coloré.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Boucle sur les kbris points.
!-------------------------------------------------
!
do jbris=1,kbris
  if(pfracx(jbris) /= pindef .and. pfracx(jbris) >= 0. .and. pfracx(jbris) <= 1. &
  & .and. pfracy(jbris) /= pindef .and. pfracy(jbris) >= 0. .and. pfracy(jbris) <= 1.) then
    !
    !-------------------------------------------------
    ! On est dans le cadre de tracé. On va tracer un disque.
    !-------------------------------------------------
    !
    zx1=rxt+pfracx(jbris)*rlxt
    zy1=ryt+(1.-pfracy(jbris))*rlyt
    ! La syntaxe "stroke" et "stroke-width" passe bien en SVG et GIF (après conversion via convert), mais par
    ! contre pas après conversion en PDF (via inkscape). Dans l'immédiat on se contente
    ! d'écrire des disques pleins, on n'utilise donc que la syntaxe SVG
    ! "fill". Ainsi cela passe en SVG GIF EPS PDF.
    !write(clcercle,fmt=*) '<circle cx="',zx1,'" cy="',zy1,'" r="',prayon,'" fill="',trim(clcourbes(jcourbes)),'" stroke="',trim(clcourbes(jcourbes)),'" stroke-width="',prayon/30.,'"  />'
    write(clcercle,fmt=*) '<circle cx="',zx1,'" cy="',zy1,'" r="',prayon,'" fill="',trim(cdcoul),'"  />'
    clcercle=cl_nettoie_blancs(clcercle)
    write(nulsvg,fmt='(a)') trim(clcercle)
  endif
enddo
end
