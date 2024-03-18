subroutine plusse(px,py,cdcoul)
! --------------------------------------------------------------
! **** *plusse* Ecriture d'un + au départ d'une ligne brisée.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2020-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!    px,py coordonnées en pixels où écrire le +.
! En sortie:
!	Ecriture du fichier SVG.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: px,py
character(len=*), intent(in) :: cdcoul

zfont_titre=rlyt*0.030*rgplusse
ixtxt=nint(px)
iytxt=nint(py+0.4*zfont_titre)
cltexte='+'
write(clsvg,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
& ,'" text-anchor="middle" fill="',trim(cdcoul),'" >' &
& ,trim(cltexte),'</text>'

clsvg=cl_nettoie_blancs(clsvg)
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Ecriture d''un +. -->'
write(nulsvg,fmt='(a)') trim(clsvg)

end
