subroutine tralps(kprinc,pprinc,ksec,psec,cdaxe)
! --------------------------------------------------------------
! **** ** TRAcé des Lignes Principales et Secondaires horizontales (si cdaxe='X')
! ou verticales (si cdaxe='Y'), ou horizontales (si cdaxe='L', légende colorée).
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
!   pprinc(1, ..., kprinc) : valeurs réelles à pointer en face de chaque ligne principale.
!   psec(1, ..., ksec) : valeurs réelles à pointer en face de chaque ligne secondaire.
! En sortie:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
real(kind=8) pprinc(jplignes)
real(kind=8) psec(jplignes)
character(len=*) :: cdaxe
zcadre_width=9.0e-4*rlxsvg
if(lgpubli) then
  !
  !-------------------------------------------------
  ! On augmente la taille des caractères
  ! pour publication en A4 bi-colonne.
  !-------------------------------------------------
  !
  zpub=1.4
else
  zpub=1.
endif
zmarq=0.015
!
!-------------------------------------------------
! On trace d'abord les lignes secondaires, car certaines seront ensuite
! recouvertes par les principales.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(100a)') '<!-- Tracé des lignes secondaires perpendiculaires à l''axe ',trim(cdaxe),'.  -->'
do jsec=1,ksec
  !
  !-------------------------------------------------
  ! Coordonnées associées à la jsec-ième ligne secondaire.
  !-------------------------------------------------
  !
  if(trim(cdaxe) == 'X') then
    zfrac=(psec(jsec)-rxmin)/(rxmax-rxmin)
    zx=rxt+zfrac*rlxt
  elseif(trim(cdaxe) == 'Y') then
    zfrac=(psec(jsec)-rymin)/(rymax-rymin)
    zy=ryt+(1.-zfrac)*rlyt
  else
    zfrac=(psec(jsec)-rcmin)/(rcmax-rcmin)
    zy=rypal+(1.-zfrac)*rlypal
  endif
  !
  !-------------------------------------------------
  ! Tracé de la ligne.
  !-------------------------------------------------
  !
  if(lgreticule) then
    zwidth=0.25*zcadre_width
  else
    zwidth=zcadre_width
  endif
  if(trim(cdaxe) == 'X') then
    zx1=zx
    zx2=zx
    zy1=ryt+rlyt
    if(lgreticule) then
      clcoul="grey"
      zy2=ryt
    else
      clcoul="black"
      zy2=ryt+(1.-0.5*zmarq)*rlyt
    endif
  elseif(trim(cdaxe) == 'Y') then
    zy1=zy
    zy2=zy
    zx1=rxt
    if(lgreticule) then
      clcoul="grey"
      zx2=rxt+rlxt
    else
      clcoul="black"
      zx2=rxt+0.5*zmarq*rlxt
    endif
  else
    clcoul="grey"
    zy1=zy
    zy2=zy
    zx1=rxpal+0.5*rlxpal-0.06*rx_legcour
    zx2=rxpal+0.5*rlxpal+0.06*rx_legcour
  endif

  write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
  write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
  write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
  write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
  write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
  if((zx1 >= rxt .and. zx1 <= rxt+rlxt &
    & .and. zx2 >= rxt .and. zx2 <= rxt+rlxt &
    & .and. zy1 >= ryt .and. zy1 <= ryt+rlyt &
    & .and. zy2 >= ryt .and. zy2 <= ryt+rlyt) &
    & .or. trim(cdaxe) == 'L' &
    & ) then
    if(trim(cdaxe) /= 'X' .and. trim(cdaxe) /= 'Y') then
    elseif(lgreticule) then
      !
      ! Trait tireté.
      !
      write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
      &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
      &,'" style="stroke-dasharray: 2, 2; stroke: ',trim(clcoul) &
      &,'; stroke-width: ',trim(clwidth),';"/>'
    else
      !
      ! Trait continu.
      !
      write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
      &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
      &,'" style="stroke: ',trim(clcoul) &
      &,'; stroke-width: ',trim(clwidth),';"/>'
    endif
  endif
enddo
!
!-------------------------------------------------
! Tracé des lignes principales.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(100a)') '<!-- Tracé des lignes principales perpendiculaires à l''axe ',trim(cdaxe),'.  -->'
do jprinc=1,kprinc
  !
  !-------------------------------------------------
  ! Pour éviter de pointer du 1e-8 parmi des 0.2, on projette sur 0. les valeurs négligeables devant l'étendue.
  !-------------------------------------------------
  !
  if(abs(pprinc(jprinc)/(pprinc(kprinc)-pprinc(1))) < 1.e-4_8) then
    pprinc(jprinc)=0.
  endif
  !
  !-------------------------------------------------
  ! Coordonnées associées à la jprinc-ième ligne principale.
  !-------------------------------------------------
  !
  if(trim(cdaxe) == 'X') then
    zfrac=(pprinc(jprinc)-rxmin)/(rxmax-rxmin)
    zx=rxt+zfrac*rlxt
  elseif(trim(cdaxe) == 'Y') then
    zfrac=(pprinc(jprinc)-rymin)/(rymax-rymin)
    zy=ryt+(1.-zfrac)*rlyt
  else
    zfrac=(pprinc(jprinc)-rcmin)/(rcmax-rcmin)
    zy=rypal+(1.-zfrac)*rlypal
  endif
  !
  !-------------------------------------------------
  ! Tracé de la ligne verticale.
  !-------------------------------------------------
  !
  if(lgreticule) then
    zwidth=0.5*zcadre_width
  else
    zwidth=zcadre_width
  endif
  !
  !-------------------------------------------------
  ! Trait plus gras en 0. ?
  !-------------------------------------------------
  !
  ! llc0 : vrai si la cote principale courante doit être tracée plus gras que les autres.
  if(pprinc(jprinc) == 0.) then
    llc0=.true.
  else
    !
    ! La cote principale courante n'est pas nulle.
    ! Elle est peut-être négligeable devant la précédente.
    !
    zeps=abs(pprinc(max(1,jprinc-1))/pprinc(jprinc))
    if(zeps > 1000.) then
      !
      ! La valeur courante est au moins 1000 fois plus petite que la précédente.
      !
      llc0=.true.
    else
      llc0=.false.
    endif
  endif
  if(llc0) zwidth=2.*zwidth ! épaisseur doublée.
  !
  !-------------------------------------------------
  ! Calcul du segment de droite.
  !-------------------------------------------------
  !
  if(trim(cdaxe) == 'X') then
    zx1=zx
    zx2=zx
    zy1=ryt+rlyt
    if(lgreticule .or. pprinc(jprinc) == 0.) then
      clcoul="grey"
      zy2=ryt
    else
      clcoul="black"
      zy2=ryt+(1.-zmarq)*rlyt
    endif
  elseif(trim(cdaxe) == 'Y') then
    zy1=zy
    zy2=zy
    zx1=rxt
    if(lgreticule .or. pprinc(jprinc) == 0.) then
      clcoul="grey"
      zx2=rxt+rlxt
    else
      clcoul="black"
      zx2=rxt+zmarq*rlxt
    endif
  else
    clcoul="grey"
    zy1=zy
    zy2=zy
    zx1=rxpal+0.5*rlxpal-0.06*rx_legcour
    zx2=rxpal+0.5*rlxpal+0.06*rx_legcour
  endif

  write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
  write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
  write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
  write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
  write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))

  ipix_zx1=nint(zx1)
  ipix_zx2=nint(zx2)
  ipix_zy1=nint(zy1)
  ipix_zy2=nint(zy2)
  ipix_rxt=nint(rxt)
  ipix_ryt=nint(ryt)
  ipix_rlxt=nint(rlxt)
  ipix_rlyt=nint(rlyt)

  if((ipix_zx1 >= ipix_rxt .and. ipix_zx1 <= ipix_rxt+ipix_rlxt &
    & .and. ipix_zx2 >= ipix_rxt .and. ipix_zx2 <= ipix_rxt+ipix_rlxt &
    & .and. ipix_zy1 >= ipix_ryt .and. ipix_zy1 <= ipix_ryt+ipix_rlyt &
    & .and. ipix_zy2 >= ipix_ryt .and. ipix_zy2 <= ipix_ryt+ipix_rlyt) &
    & .or. trim(cdaxe) == 'L' &
    & ) then
    if(lgreticule) then
      write(nulsvg,fmt='(100a)') ' '
      write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
      &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
      &,'" style="stroke: ',trim(clcoul) &
      &,'; stroke-width: ',trim(clwidth),';"/>'
    endif
    !
    !-------------------------------------------------
    ! Pointage de la valeur réelle.
    !-------------------------------------------------
    !
    !
    !-------------------------------------------------
    ! A quel format afficher les réels à droite de la virgule? 
    !-------------------------------------------------
    !
    zecart=abs(pprinc(2)-pprinc(1))
    idrov=max(0,-nint(log(zecart)/log(10.)-0.5))+1 ! nb de chiffres à afficher à droite de la virgule.
    if(nint(zecart) /= 0) then
      if(abs(zecart/real(nint(zecart))-1.) < 0.001) idrov=0 ! cas où le réel est un entier.
    endif
    !
    !-------------------------------------------------
    ! Combien de caractères à gauche de la virgule?
    !-------------------------------------------------
    !
    if(trim(cdaxe) == 'X') then
      igauv=max(1,nint(log(max(abs(rxmin),abs(rxmax)))/log(10.)-0.5)+1)
    else if(trim(cdaxe) == 'Y') then
      igauv=max(1,nint(log(max(abs(rymin),abs(rymax)))/log(10.)-0.5)+1)
    else
      igauv=max(1,nint(log(max(abs(rcmin),abs(rcmax)))/log(10.)-0.5)+1)
    endif
    if(trim(cglegf) /= trim(cgindef) .and. trim(cdaxe) == 'L') then
      !
      !-------------------------------------------------
      ! On trace un axe de légende et l'utilisateur a spécifié un format FORTRAN
      ! d'écriture des réels de la légende.
      !-------------------------------------------------
      !
      clformat2='('//trim(cglegf)//')'
      write(cltxt,fmt=clformat2) pprinc(jprinc)
    elseif(igauv > 5) then
      !
      !-------------------------------------------------
      ! Grand nombre. On ne peut l'afficher au format fixe.
      !-------------------------------------------------
      !
      !clformat2='(g16.3)'
      !write(cltxt,fmt=clformat2) pprinc(jprinc)
      call reecar(pprinc(jprinc),-1,3,cltxt,iltxt)
    else
      !
      !-------------------------------------------------
      ! Le format fixe comporte un nombre total de caractères égal à igauv + idrov +
      ! 2, le "+2" correspondant au point décimal et à l'éventuel signe "-" à gauche.
      !-------------------------------------------------
      !
      if(idrov /= 0) then
        !
        !-------------------------------------------------
        ! Ecriture de réel.
        !-------------------------------------------------
        !
        if(idrov <= 4) then
          !
          !-------------------------------------------------
          ! Format fixe.
          !-------------------------------------------------
          !
          itot=igauv+idrov+12
          write(clformat,fmt=*) '(F',itot,'.',idrov,')'
          clformat2=cl_nettoie_blancs(clformat)
          write(cltxt,fmt=clformat2) pprinc(jprinc)
        else
          !
          !-------------------------------------------------
          ! Format scientifique.
          !-------------------------------------------------
          !
          itot=igauv+idrov+12
          call reecar(pprinc(jprinc),-1,3,cltxt,iltxt)
        endif
      else
        !
        !-------------------------------------------------
        ! Ecriture d'entier.
        !-------------------------------------------------
        !
        write(cltxt,fmt=*) nint(pprinc(jprinc))
        cltxt=cl_nettoie_blancs(cltxt)
      endif
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
      write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
      & ,ixtxt,'" y="' &
      & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
      & ,'" text-anchor="middle" fill="black" >' &
      & ,trim(cltxt),'</text>'
    elseif(trim(cdaxe) == 'Y') then
      cltxt=cl_nettoie_blancs(cltxt)
      ixtxt=0.65*rxt
      iytxt=zy1+0.4*rgtaille_fonte*rfont_axes
      write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
      & ,ixtxt,'" y="' &
      & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
      & ,'" text-anchor="middle" fill="black" >' &
      & ,trim(cltxt),'</text>'
    else
      ixtxt=rxpal+rlxpal+0.25*(rx_legcour-rlxpal)
      iytxt=zy1+0.4*rgtaille_fonte*rfont_axes
      write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
      & ,ixtxt,'" y="' &
      & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
      & ,'" text-anchor="middle" fill="black" >' &
      & ,trim(cltxt),'</text>'
    endif
    clsvg_texte=cl_nettoie_blancs(clsvg_txt)
    write(nulsvg,fmt='(a)') trim(clsvg_texte)
  endif
enddo 
!
!-------------------------------------------------
! Ecriture de l'unité.
!-------------------------------------------------
!
if(trim(cdaxe) == 'X') then
  !
  ! Axe X.
  !
  !ixtxt_unite=(rxt+rlxt)*0.97
  ixtxt_unite=rxt+0.5*rlxt
  !iytxt_unite=ryt+rlyt+0.7*ry_legx
  !iytxt_unite=iytxt+0.27*ry_legx*rfont_axes
  iytxt_unite=0.505*(iytxt+rlysvg)
  clleg=cglegx
  ztaif=0.80*rgtaille_fonte*rfont_axes*zpub
elseif(trim(cdaxe) == 'Y') then
  !
  ! Axe Y.
  !
  ixtxt_unite=0.26*rxt
  iytxt_unite=ryt+0.5*rlyt
  clleg=cglegy
  ztaif=0.80*rgtaille_fonte*rfont_axes*zpub
else
  !
  ! Légende de la palette colorée.
  !
  !ixtxt_unite=rxpal+rlxpal+0.25*(rx_legcour-rlxpal)
  ixtxt_unite=rxt+rlxt+0.5*(rlxsvg-rxt-rlxt)
  iytxt_unite=rypal+rlypal+0.33*(rlysvg-rypal-rlypal)
  clleg=cgunite
  ztaif=0.80*rgtaille_fonte*rfont_unite*zpub
endif
if(trim(clleg) == 'none') clleg=' '

if(trim(cdaxe) == 'Y') then
  write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,a,i5,a,i5,3a)') '<text x="' &
  & ,ixtxt_unite,'" y="' &
  & ,iytxt_unite,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
  & ,'" text-anchor="middle" transform="rotate(-90,',ixtxt_unite,', ',iytxt_unite,')" fill="black" >' &
  & ,trim(clleg),'</text>'
else
  write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
  & ,ixtxt_unite,'" y="' &
  & ,iytxt_unite,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
  & ,'" text-anchor="middle" fill="black" >' &
  & ,trim(clleg),'</text>'
endif

clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') '<!-- Ecriture de l''unité. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
