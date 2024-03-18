subroutine svg_tralgi(pval,cdaxe,cdprisec,cdtxt,ptaille_fonte,pdecal)
! --------------------------------------------------------------
! **** ** TRAce une Ligne-Guide individuelle.
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
!   cdprisec='princ' ou 'sec' suivant qu'il faut tracer une ligne-guide principale ou secondaire.
!   cdaxe: 'X' ou 'Y'
!   pdecal: décalage de l'écriture du texte, versus la position nominale, exprimé en fraction de l'espace dévolu au texte.
! En sortie:
! TRAce une Ligne-Guide individuelle en position réelle pval sur l'axe cdaxe, avec un trait principal ou secondaire (suivant la valeur de cdprisec), et comme nombre-légende le texte cdtxt.
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
character(len=*) :: cdaxe,cdprisec,cdtxt
!
!-------------------------------------------------
! Coordonnées-pixels associées à la valeur réelle pval.
!-------------------------------------------------
!
if(trim(cdaxe) == 'X') then
  zfrac=(pval-rxmin)/(rxmax-rxmin)
  zx=rxt+zfrac*rlxt
else
  zfrac=(pval-rymin)/(rymax-rymin)
  zy=ryt+(1.-zfrac)*rlyt
endif
if(zfrac < 0. .or. zfrac > 1.) return
!
!-------------------------------------------------
! Epaisseur du trait.
!-------------------------------------------------
!
if(trim(cdprisec) == 'princ') then
  !
  !-------------------------------------------------
  ! Ligne principale.
  !-------------------------------------------------
  !
  zwidth=4.54e-4*rlxsvg
  cldash=' ' ! ligne continue; pas de dash.
else
  !
  !-------------------------------------------------
  ! Ligne secondaire.
  !-------------------------------------------------
  !
  zwidth=2.27e-4*rlxsvg
  cldash=' stroke-dasharray: 2, 2;' ! ligne pointillée.
endif
clcoul="grey"
!
!-------------------------------------------------
! Départ et arrivée du trait.
!-------------------------------------------------
!
if(trim(cdaxe) == 'X') then
  zx1=zx
  zx2=zx
  zy1=ryt
  zy2=ryt+rlyt
else
  zy1=zy
  zy2=zy
  zx1=rxt
  zx2=rxt+rlxt
endif

write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
&,'" x2="',trim(clx2),'" y2="',trim(cly2) &
&,'" style="',trim(cldash),' stroke: ',trim(clcoul) &
&,'; stroke-width: ',trim(clwidth),';"/>'
!
!-------------------------------------------------
! Ecriture de la valeur réelle en face de la ligne.
!-------------------------------------------------
!
if(trim(cdaxe) == 'X') then
  ixtxt=zx1
  iytxt=ryt+rlyt+(0.43-pdecal)*ry_legx
else
  ixtxt=(0.65+pdecal)*rxt
  iytxt=zy1+0.4*ptaille_fonte
endif

write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ptaille_fonte &
& ,'" text-anchor="middle" fill="black" >' &
& ,trim(cdtxt),'</text>'

clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') '<!-- Ecriture de la valeur réelle de légende des axes. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
