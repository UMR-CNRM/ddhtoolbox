subroutine svg_axe_log(pmin,pmax,cdaxe)
! --------------------------------------------------------------
! **** ** TRAcé des Lignes Principales et Secondaires horizontales (si cdaxe='X')
! ou verticales (si cdaxe='Y'), ou horizontales (si cdaxe='L', légende colorée),
! dans le cas logarithmique.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
character(len=*) :: cdaxe
integer(kind=4) :: imant(8)
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
if(pmin <= 0.) then
  write(*,fmt=*)
  write(*,fmt=*) 'svg_axe_log/ERREUR: la valeur de ',trim(cdaxe),' min est <= 0. !...'
  write(*,fmt=*) pmin
  call exit(1)
endif
if(pmax <= 0.) then
  write(*,fmt=*)
  write(*,fmt=*) 'svg_axe_log/ERREUR: la valeur de ',trim(cdaxe),' max est <= 0. !...'
  write(*,fmt=*) pmax
  call exit(1)
endif
!
!-------------------------------------------------
! Log10 des extrêmes.
!-------------------------------------------------
!
zlmin=log(pmin)/log(10.)
zlmax=log(pmax)/log(10.)
if(lgdebu) write(*,fmt=*) 'svg_axe_log: ',zlmin,' < log10 axe ',trim(cdaxe),' < ',zlmax
!
!-------------------------------------------------
! Leur entier inférieur (pour le min) et supérieur (pour le max).
!-------------------------------------------------
!
ilmin=nint(zlmin-0.5)
ilmax=nint(zlmax-0.5)+1
if(lgdebu) write(*,fmt=*) 'svg_axe_log: ',ilmin,' < entier log10 axe ',trim(cdaxe),' < ',ilmax
!
!-------------------------------------------------
! Pour chacune des valeurs de log10 entières, on trace une ligne principale.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(100a)') '<!-- Tracé des lignes principales de l''axe log ',trim(cdaxe),'.  -->'
if(ilmax < ilmin) then
  istep=-1
else
  istep=1
endif
do jent=ilmin,ilmax,istep
  zval=10.**real(jent)
  !
  !-------------------------------------------------
  ! Coordonnées associées à la jprinc-ième ligne principale.
  !-------------------------------------------------
  !
  zfrac=(real(jent)-zlmin)/(zlmax-zlmin)
  if(trim(cdaxe) == 'X') then
    zx=rxt+zfrac*rlxt
  elseif(trim(cdaxe) == 'Y') then
    zy=ryt+(1.-zfrac)*rlyt
  elseif(trim(cdaxe) == 'L') then
    zy=rypal+(1.-zfrac)*rlypal
  else
    write(*,fmt=*)
    write(*,fmt=*) 'svg_axe_log/ERREUR: cas d''axe non prévu !...'
    write(*,fmt=*) trim(cdaxe)
    call exit(1)
  endif
  if(lgdebu) write(*,fmt=*) 'svg_axe_log: jent=',jent,' zval=',zval
  !
  !-------------------------------------------------
  ! Tracé de la ligne verticale.
  !-------------------------------------------------
  !
  !zwidth=9.0e-4*rlxsvg
  zwidth=0.81
  if(trim(cdaxe) == 'X') then
    zx1=zx
    zx2=zx
    zy1=ryt
    zy2=ryt+rlyt
    clcoul="grey"
  elseif(trim(cdaxe) == 'Y') then
    zy1=zy
    zy2=zy
    zx1=rxt
    zx2=rxt+rlxt
    clcoul="grey"
  elseif(trim(cdaxe) == 'L') then
    zy1=zy
    zy2=zy
    zx1=rxpal+0.5*rlxpal-0.05*rx_legcour
    zx2=rxpal+0.5*rlxpal+0.05*rx_legcour
    clcoul="rgb(40,40,40)"
  else
    write(*,fmt=*)
    write(*,fmt=*) 'svg_axe_log/ERREUR: cas d''axe non prévu !...'
    write(*,fmt=*) trim(cdaxe)
    call exit(1)
  endif

  if(lgdebu) write(*,fmt=*) 'svg_axe_log: zx1,zx2,zy1,zy2=',zx1,zx2,zy1,zy2
  !
  !-------------------------------------------------
  ! La ligne est-elle au sein du cadre de tracé?
  !-------------------------------------------------
  !
  iok=1
  if(trim(cdaxe) == 'X' .or. trim(cdaxe) == 'Y') then
    if(zx1 >= rxt .and. zx1 <= rxt+rlxt &
    & .and. zx2 >= rxt .and. zx2 <= rxt+rlxt &
    & .and. zy1 >= ryt .and. zy1 <= ryt+rlyt &
    & .and. zy2 >= ryt .and. zy2 <= ryt+rlyt) then
      iok=1
    else
     iok=0
    endif
  else
    if(zy1 >= rypal .and. zy2 <= rypal+rlypal) then
      iok=1
    else
      iok=0
    endif
  endif
  if(iok == 1) then
    write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
    write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
    write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
    write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
    write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
    write(nulsvg,fmt='(100a)') ' '
    write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
    &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
    &,'" style="stroke: ',trim(clcoul) &
    &,'; stroke-width: ',trim(clwidth),';"/>'
    !
    !-------------------------------------------------
    ! Pointage de la valeur réelle.
    !-------------------------------------------------
    !
    if(jent == -3) then
      cltxt='0.001'
    elseif(jent == -2) then
      cltxt='0.01'
    elseif(jent == -1) then
      cltxt='0.1'
    elseif(jent == 0) then
      cltxt='1'
    elseif(jent == 1) then
      cltxt='10'
    elseif(jent == 2) then
      cltxt='100'
    elseif(jent == 3) then
      cltxt='1000'
    else
      write(cltxt,fmt='(a,i3)') '10**',jent
    endif
    !
    !-------------------------------------------------
    ! Ecriture de la valeur réelle en face de la ligne.
    !-------------------------------------------------
    !
    ztaif=0.80*rgtaille_fonte*rfont_axes*zpub
    if(trim(cdaxe) == 'X') then
      ixtxt=zx1
      iytxt=ryt+rlyt+0.43*ry_legx
    elseif(trim(cdaxe) == 'Y') then
      ixtxt=0.65*rxt
      iytxt=zy1+0.4*rgtaille_fonte*rfont_axes
    else
      ixtxt=rxpal+rlxpal+0.20*(rx_legcour-rlxpal)
      iytxt=zy1+0.4*rgtaille_fonte*rfont_axes
    endif
    
    write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
    & ,ixtxt,'" y="' &
    & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
    & ,'" text-anchor="middle" fill="black" >' &
    & ,trim(cltxt),'</text>'
    
    clsvg_texte=cl_nettoie_blancs(clsvg_txt)
    write(nulsvg,fmt='(a)') trim(clsvg_texte)
  endif
  !
  !-------------------------------------------------
  ! Tracé de traits fins en face des mantisses 2,3,4,5,6,7,8,9.
  !-------------------------------------------------
  !
  imant(1)=2
  imant(2)=3
  imant(3)=4
  imant(4)=5
  imant(5)=6
  imant(6)=7
  imant(7)=8
  imant(8)=9
  do jmant=1,8
    zval=real(imant(jmant))*10.**real(jent)
    zlog10=log(zval)/log(10.)
    !
    !-------------------------------------------------
    ! Coordonnées associées à ce trait fin.
    !-------------------------------------------------
    !
    zfrac=(zlog10-zlmin)/(zlmax-zlmin)
    if(trim(cdaxe) == 'X') then
      zx=rxt+zfrac*rlxt
    elseif(trim(cdaxe) == 'Y') then
      zy=ryt+(1.-zfrac)*rlyt
    elseif(trim(cdaxe) == 'L') then
      zy=rypal+(1.-zfrac)*rlypal
    else
      write(*,fmt=*)
      write(*,fmt=*) 'svg_axe_log/ERREUR: cas d''axe non prévu !...'
      write(*,fmt=*) trim(cdaxe)
      call exit(1)
    endif
    if(lgdebu) write(*,fmt=*) 'svg_axe_log: jmant=',jmant,' zval=',zval
    !
    !-------------------------------------------------
    ! Tracé du trait fin.
    !-------------------------------------------------
    !
    !zwidth=3.0e-4*rlxsvg
    zwidth=0.27
    !zpett=0.016 ! Gestion de la longueur du trait fin: on trace un petit trait.
    !zpett=0.000 ! Gestion de la longueur du trait fin: pas de trait fin.
    zpett=1.000 ! Gestion de la longueur du trait fin: il fait toute la longueur de l'espace de tracé.
    if(trim(cdaxe) == 'X') then
      zx1=zx
      zx2=zx
      zy1=ryt+(1.-zpett)*rlyt
      zy2=ryt+rlyt
    elseif(trim(cdaxe) == 'Y') then
      zy1=zy
      zy2=zy
      zx1=rxt
      zx2=rxt+zpett*rlxt
    elseif(trim(cdaxe) == 'L') then
      zy1=zy
      zy2=zy
      zx1=rxpal+0.5*rlxpal-0.05*rx_legcour
      zx2=rxpal+0.5*rlxpal+0.05*rx_legcour
    else
      write(*,fmt=*)
      write(*,fmt=*) 'svg_axe_log/ERREUR: cas d''axe non prévu !...'
      write(*,fmt=*) trim(cdaxe)
      call exit(1)
    endif
  
    if(lgdebu) write(*,fmt=*) 'svg_axe_log: zx1,zx2,zy1,zy2=',zx1,zx2,zy1,zy2
    !
    !-------------------------------------------------
    ! La ligne est-elle au sein du cadre de tracé?
    !-------------------------------------------------
    !
    iok=1
    if(trim(cdaxe) == 'X' .or. trim(cdaxe) == 'Y') then
      if(zx1 >= rxt .and. zx1 <= rxt+rlxt &
      & .and. zx2 >= rxt .and. zx2 <= rxt+rlxt &
      & .and. zy1 >= ryt .and. zy1 <= ryt+rlyt &
      & .and. zy2 >= ryt .and. zy2 <= ryt+rlyt) then
        iok=1
      else
       iok=0
      endif
    else
      if(zy1 >= rypal .and. zy2 <= rypal+rlypal) then
        iok=1
      else
        iok=0
      endif
    endif
    if(iok == 1) then
      write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
      write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
      write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
      write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
      write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
      write(nulsvg,fmt='(100a)') ' '
      write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
      &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
      &,'" style="stroke: ',trim(clcoul) &
      &,'; stroke-width: ',trim(clwidth),';"/>'
    endif
  enddo
enddo
!
!-------------------------------------------------
! Ecriture de l'unité.
!-------------------------------------------------
!
if(trim(cdaxe) == 'X') then
  !ixtxt=(rxt+rlxt)*0.97
  ixtxt=rxt+0.5*rlxt
  iytxt=ryt+rlyt+0.7*ry_legx
  clleg=cglegx
elseif(trim(cdaxe) == 'Y') then
  ixtxt=0.26*rxt
  iytxt=ryt+0.5*rlyt
  clleg=cglegy
else
  ixtxt=rxt+rlxt+0.3*(rlxsvg-rxt-rlxt)
  iytxt=rypal+rlypal+0.33*(rlysvg-rypal-rlypal)
  clleg=cgunite

endif
if(trim(clleg) == 'none') clleg=' '

if(trim(cdaxe) == 'Y') then
  write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,a,i5,a,i5,3a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
  & ,'" text-anchor="middle" transform="rotate(-90,',ixtxt,', ',iytxt,')" fill="black" >' &
  & ,trim(clleg),'</text>'
else
  write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
  & ,'" text-anchor="left" fill="black" >' &
  & ,trim(clleg),'</text>'
endif

clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') '<!-- Ecriture de l''unité. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
