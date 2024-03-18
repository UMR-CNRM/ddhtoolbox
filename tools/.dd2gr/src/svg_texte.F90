subroutine svg_texte(cdtxt,cdfont,ptaif,cdcoulpp,px,py,ldcadre,cdcoulcadre)
! --------------------------------------------------------------
! **** *svg_texte* écriture d'un texte, avec ou sans cadre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
!
! En entree:
! cdtxt : texte à écrire.
! cdfont : fonte du texte.
! ptaif : taille du texte.
! cdcoulpp : couleur du texte (pp: premier plan).
! px,py : coordonnées du texte dans l'espace des coordonnées réelles (ex: temps, valeur, etc).
! ldcadre : vrai si le texte doit être écrit dans un cadre.
! cdcoulcadre : couleur de fond dans le cadre.
!
! En sortie:
!	Ecriture sur le fichier SVG d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
character(len=*), intent(in) :: cdtxt,cdfont,cdcoulpp,cdcoulcadre
real(kind=8), intent(in) :: ptaif,px,py
logical, intent(in) :: ldcadre
!
!-------------------------------------------------
! Position en X et Y du centre du cadre et du texte.
!-------------------------------------------------
!
ix=nint(rxt+(px-rxmin)/(rxmax-rxmin)*rlxt)
iy=nint(ryt+rlyt-(py-rymin)/(rymax-rymin)*rlyt)+0.4*ptaif
!
!-------------------------------------------------
! Cadre éventuel.
!-------------------------------------------------
!
if(ldcadre) then
  icar=len_trim(cdtxt) ! nb de caractères du texte à écrire, pour dimensionner le cadre.
  !
  !-------------------------------------------------
  ! Tracé d'un pavé rectangulaire.
  !-------------------------------------------------
  !
  iwidth=icar*ptaif*0.67 ! largeur du pavé.
  iheight=ptaif*1.2 ! hauteur du pavé.
  ixpave=ix-iwidth/2 ! X du bord gauche du pavé.
  !iypave=iy-0.86*ptaif ! Y du bord haut du pavé.
  iypave=iy-0.88*ptaif ! Y du bord haut du pavé.
  write(clrect,fmt='(4(a,i5),1000a)') '<rect x="',ixpave,'" y="',iypave,'" width="',iwidth &
  &,'" height="',iheight,'" style="fill:',trim(cdcoulcadre),'; stroke: ',trim(cdcoulpp),';stroke-width: 1;" />'
  clrect=cl_nettoie_blancs(clrect)
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Ecriture d''un cadre pour le texte. -->'
  write(nulsvg,fmt='(a)') trim(clrect)
endif
!
!-------------------------------------------------
! Ecriture du texte.
!-------------------------------------------------
!
write(cltxt,fmt='(a,i5,a,i5,3a,g16.7,9a)') '<text x="' &
& ,ix,'" y="' &
& ,iy,'" ',trim(cdfont),' font-size="',ptaif &
& ,'" text-anchor="middle" fill="',trim(cdcoulpp),'" >' &
& ,trim(cdtxt),'</text>'
clsvg_texte=cl_nettoie_blancs(cltxt)
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Ecriture d''un texte. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
