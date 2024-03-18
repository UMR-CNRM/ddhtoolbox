subroutine cote_jours
! --------------------------------------------------------------
! **** *cote_jours*
! --------------------------------------------------------------
! Sujet:
!   Dans le cadre d'un diagramme Hayashi, cotation de jours sur l'axe vertical de droite + tracé de droites horizontales matérialisant 30 jours, 6 jours, etc.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
!	écriture sur le fichier SVG, d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
integer, parameter :: jpdays=3
real(kind=8) :: zdays(jpdays)
integer, parameter :: jpdays2=100
real(kind=8) :: zdays2(jpdays2)
if(lgpubli) then
  !
  !-------------------------------------------------
  ! On augmente la taille des caractères
  ! pour publication en A4 bi-colonne.
  !-------------------------------------------------
  !
  zpub=1.6
else
  zpub=1.
endif
!
!-------------------------------------------------
! Tracé de droites horizontales matérialisant 30 jours, 6 jours, etc.
!-------------------------------------------------
!
zdays(1)=3.
zdays(2)=6.
zdays(3)=30.
do jdays=1,jpdays
  !
  !-------------------------------------------------
  ! L'axe des Y est supposé être en CPD (cycles per day), on va donc coter zdays à la hauteur Y de son inverse.
  !-------------------------------------------------
  !
  zy=1./zdays(jdays)
  !
  !-------------------------------------------------
  ! Pixels de départ et arrivée de la droite horizontale.
  !-------------------------------------------------
  !
  zx1=rxt
  zx2=rxt+rlxt
  zy1=ryt+rlyt*(rymax-zy)/(rymax-rymin)
  zy2=zy1
  write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
  write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
  write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
  write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
  !
  !-------------------------------------------------
  ! Epaisseur de trait.
  !-------------------------------------------------
  !
  zwidth=1.00*zpub
  write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
  !
  !-------------------------------------------------
  ! Ecriture de la droite.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(100a)') '<!-- Tracé d''une ligne matérialisant les jours (ex:30 days) -->'
  clcoul='black'
  write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
  &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
  &,'" style="stroke-dasharray: 5, 5; stroke: ',trim(clcoul) &
  &,'; stroke-width: ',trim(clwidth),';"/>'
  !
  !-------------------------------------------------
  ! Ecriture d'une légende au-dessus de cette droite: "30 days".
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(100a)') '<!-- Texte donnant la valeur en jours; exemple : "30 days" -->'
  write(cltxt,fmt='(i2,a)') nint(zdays(jdays)),' days'
  !ixtxt=rxt+rlxt-0.06*rlxt
  ixtxt=rxt+rlxt-0.08*rlxt
  iytxt=zy1-0.4*rgtaille_fonte*rfont_axes
  ztaif=0.62*rgtaille_fonte*rfont_axes*zpub
  clfonte='font-family="URW Gothic L"'
  clfonte='font-family="Arial"'
  write(clxtxt,fmt=*) ixtxt ; clxtxt=adjustl(adjustr(clxtxt))
  write(clytxt,fmt=*) iytxt ; clytxt=adjustl(adjustr(clytxt))
  write(cltaif,fmt=*) ztaif ; cltaif=adjustl(adjustr(cltaif))
  write(nulsvg,fmt='(100a)') '<text x="' &
  & ,trim(clxtxt),'" y="' &
  & ,trim(clytxt),'" ',trim(clfonte),' font-size="',trim(cltaif) &
  & ,'" text-anchor="middle" fill="black" >' &
  & ,trim(cltxt),'</text>'
enddo
!
!-------------------------------------------------
! Cotation de jours sur l'axe vertical de droite.
!-------------------------------------------------
!
do jdays2=1,jpdays2
  zdays2(jdays2)=1./(0.05*real(jdays2))
  !
  !-------------------------------------------------
  ! L'axe des Y est supposé être en CPD (cyles per day), on va donc coter zdays à la hauteur Y de son inverse.
  !-------------------------------------------------
  !
  zy=1./zdays2(jdays2)
  if(zy > rymax) cycle
  !
  !-------------------------------------------------
  ! Pixels de départ et arrivée du tiret cotant la hauteur de cette valeur de jours.
  !-------------------------------------------------
  !
  zx1=rxt+rlxt
  zx2=rxt+rlxt+0.01*rlxt
  zy1=ryt+rlyt*(rymax-zy)/(rymax-rymin)
  zy2=zy1
  write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
  write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
  write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
  write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
  !
  !-------------------------------------------------
  ! Epaisseur de trait.
  !-------------------------------------------------
  !
  zwidth=1.
  write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
  !
  !-------------------------------------------------
  ! Ecriture de la droite.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(100a)') '<!-- Tiret matérialisant la hauteur d''un nombre de jours dans le diagramme Hayashi. -->'
  clcoul='black'
  write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
  &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
  &,'" style="stroke: ',trim(clcoul) &
  &,'; stroke-width: ',trim(clwidth),';"/>'
  !
  !-------------------------------------------------
  ! Ecriture de la valeur numérique en jours.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(100a)') '<!-- Valeur numérique en jours de ce tiret. -->'
  if(zdays2(jdays2) > 9.99) then
    clformat='(f4.1)'
  else
    clformat='(f4.2)'
  endif
  write(cltxt,fmt=clformat) zdays2(jdays2)
  ztaif=0.70*rgtaille_fonte*rfont_axes*zpub
  ixtxt=rxt+rlxt+0.05*rlxt+(zpub-1.)*rlxt*0.01
  iytxt=zy1+0.4*ztaif
  clfonte='font-family="URW Gothic L"'
  clfonte='font-family="Arial"'
  write(clxtxt,fmt=*) ixtxt ; clxtxt=adjustl(adjustr(clxtxt))
  write(clytxt,fmt=*) iytxt ; clytxt=adjustl(adjustr(clytxt))
  write(cltaif,fmt=*) ztaif ; cltaif=adjustl(adjustr(cltaif))
  write(nulsvg,fmt='(100a)') '<text x="' &
  & ,trim(clxtxt),'" y="' &
  & ,trim(clytxt),'" ',trim(clfonte),' font-size="',trim(cltaif) &
  & ,'" text-anchor="middle" fill="black" >' &
  & ,trim(cltxt),'</text>'
enddo
!
!-------------------------------------------------
! Texte en clair: "period: (days)".
!-------------------------------------------------
!
clleg='period (days)'
ixtxt=rxt+rlxt+4.9*ztaif-(zpub-1.)*2.0*ztaif
iytxt=ryt+0.5*rlyt
ztaif=0.80*rgtaille_fonte*rfont_unite*zpub
write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,a,i5,a,i5,3a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
& ,'" text-anchor="middle" transform="rotate(-90,',ixtxt,', ',iytxt,')" fill="black" >' &
& ,trim(clleg),'</text>'
clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') '<!-- Ecriture du texte en clair, légende des nombres de jours dans le cas Hayashi. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
!
!-------------------------------------------------
! Tracé des courbes-linéaments de la dispersion en shallow-water.
!-------------------------------------------------
!
call hayashi_shallow_water
end
