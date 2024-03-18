subroutine cotation(px1,px2,py1,py2,pec,pisol,cdcoul,ptail)
! --------------------------------------------------------------
! **** ** Cotation d'isolignes: on met un label réel.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Choix du format d'écriture du réel.
! iordg vaut -2 si pisol=0.01, 3 si pisol=1000..
!-------------------------------------------------
!
if(pisol == 0.) then
  iordg=1
else
  iordg=nint(log(abs(pisol))/log(10.)-0.5)
endif
!
!-------------------------------------------------
! Dans le cas où l'écartement d'isolignes est plus petit que la valeur d'isoligne
! elle-même, on prend le min des deux. Exemple: l'isoligne courante est 0.25 
! et l'écart d'isoligne 0.0001, il est préférable de coter la chaîne "0.2500"
! que "0.25". D'où le min ci-dessous.
!-------------------------------------------------
!
if(trim(cgtyppal) /= 'LOG') iordg=min(iordg,nint(log(abs(pec))/log(10.)-0.5))
!
!-------------------------------------------------
! Détermination du format à partir de l'ordre de grandeur iordg.
!-------------------------------------------------
!
if(iordg < -3) then
  llentier=.false.
  clformat='es10.3'
elseif(iordg <= 0) then
  llentier=.false.
  ivirg=-iordg
  itot=ivirg+10
  write(clvirg,fmt=*) ivirg
  clvirg=adjustl(clvirg)
  write(clformat,fmt='(a,i3.3,a,a)') 'f',itot,'.',trim(clvirg)
  if(ivirg == 0) llentier=.true.
elseif(iordg < 8) then
  llentier=.true.
  clformat='gol'
else
  llentier=.false.
  clformat='es10.3'
endif
clformat='('//trim(clformat)//')'

if(llentier) then
  !
  !-------------------------------------------------
  ! Cotation d'entiers.
  !-------------------------------------------------
  !
  write(cltexte,fmt=*) nint(pisol)
else
  !
  !-------------------------------------------------
  ! Cotation de réels.
  !-------------------------------------------------
  !
  write(cltexte,fmt=clformat) pisol
endif
cltexte=adjustl(adjustr(cltexte))
!
!-------------------------------------------------
! Taille du label.
!-------------------------------------------------
!
ztaille_leg=rgtaille_fonte*rfont_leg*0.55*ptail
ixtxt=nint(0.5*(px1+px2))
iytxt=nint(0.5*(py1+py2))
!
!-------------------------------------------------
! Angle de rotation du texte.
!-------------------------------------------------
!
zu=px2-px1
zv=py2-py1
call recpol(zu,zv,zr,zang)
zpi=4.*atan(1.)
ixtxt=ixtxt-nint(1.*cos(zang+zpi/2.))
iytxt=iytxt-nint(1.*sin(zang+zpi/2.))
zang=180./zpi*zang
if(zang > 90. .or. zang < -90. ) then
  zang=zang+180.
endif
iang=nint(zang) ! angle de rotation du texte.
write(clrotate,fmt='(a,i5,a,i5.5,a,i5.5,a)') ' transform="rotate(',iang,' ',ixtxt,',',iytxt,')"'
!
!-------------------------------------------------
! Ecriture du texte dans le SVG.
!-------------------------------------------------
!
write(clsvg_txt,fmt='(a,i5,a,i5,4a,g16.7,100a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_texte),trim(clrotate),' font-size="',ztaille_leg &
& ,'" text-anchor="middle" fill="',trim(cdcoul),'" >' &
& ,trim(cltexte),'</text>'

clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Ecriture d''une cotation d''isoligne. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
