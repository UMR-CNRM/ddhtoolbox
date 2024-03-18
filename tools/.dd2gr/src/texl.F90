subroutine texl
! --------------------------------------------------------------
! **** *texl* Ecriture d'un texte libre de l'utilisateur.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2020-05, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
!	Ecriture du fichier SVG.
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
character(len=180) :: clmot(40)
!
do jtexl=1,ntexl
  cldir=cgtexl(jtexl)
  call casc(cldir,1,clmot,ilmot)
  if(ilmot /= 5) then
    write(*,fmt=*)
    write(*,fmt=*) 'dd2gr/ERREUR: pour écrire un texte libre il faut 5 paramètres !...'
    write(*,fmt=*) trim(cldir)
    call exit(1)
  endif
  do jmot=1,ilmot
    clfct=clmot(jmot)(1:index(clmot(jmot),'=')-1)
    if(trim(clfct) == '#TEXTE') then
      cltexte=clmot(jmot)(index(clmot(jmot),'=')+1:len_trim(clmot(jmot)))
    elseif(trim(clfct) == 'TAIL') then
      clree=clmot(jmot)(index(clmot(jmot),'=')+1:len_trim(clmot(jmot)))
      read(clree,fmt=*) ztail
    elseif(trim(clfct) == 'COUL') then
      clcoul=clmot(jmot)(index(clmot(jmot),'=')+1:len_trim(clmot(jmot)))
    elseif(trim(clfct) == 'X') then
      clree=clmot(jmot)(index(clmot(jmot),'=')+1:len_trim(clmot(jmot)))
      read(clree,fmt=*) zx
    elseif(trim(clfct) == 'Y') then
      clree=clmot(jmot)(index(clmot(jmot),'=')+1:len_trim(clmot(jmot)))
      read(clree,fmt=*) zy
    else
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: fonction utilisateur non attendue dans texl !...'
      write(*,fmt=*) trim(cldir),trim(clfct)
      call exit(1)
    endif
  enddo ! jmot
  zfont_titre=rgtaille_fonte*rfont_titre*ztail
  ixtxt=nint(zx)
  iytxt=nint(zy)
  write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,9a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
  & ,'" text-anchor="middle" fill="',trim(clcoul),'" >' &
  & ,trim(cltexte),'</text>'
  
  clsvg_titre=cl_nettoie_blancs(clsvg_tit)
  write(nulsvg,fmt='(9a)') '<!-- Ecriture d''un texte utilisateur. -->'
  write(nulsvg,fmt='(a)') trim(clsvg_titre)
enddo !jtexl

end
