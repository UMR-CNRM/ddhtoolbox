subroutine svg_trac1d(pindef,psaut,kbris,kcourbes,px,py,pb,ldb,cdtexte &
& ,ldlegende,kximage,kyimage,cdficppm)
! --------------------------------------------------------------
! **** *trac1d* Ecriture d'un fichier SVG image, contenant plusieurs courbes 1D.
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
! 	pindef: valeur réelle spécifiant une valeur manquante.
!	psaut: valeur réelle spécifiant un lever de plume lors du tracé de la courbe.
!	kbris: nombre maximal de valeurs définissant la ligne brisée à tracer.
!	kcourbes: nombre de courbes à tracer.
!	px(kcourbes,kbris): suite des valeurs de X de la ligne brisée définissant chaque courbe.
!	py(kcourbes,kbris): suite des valeurs de Y de la ligne brisée définissant chaque courbe.
!	cdtexte(kcourbes): nom en clair de chacune des courbes.
!	rxmin: valeur minimale de X de TOUTES les kcourbes courbes.
!	rxmax: valeur maximale de X de TOUTES les kcourbes courbes.
!	rymin: valeur minimale de Y de TOUTES les kcourbes courbes.
!	rymax: valeur maximale de Y de TOUTES les kcourbes courbes.
!	ldlegende: vrai si on veut un légendage de l'axe des X et Y.
!	kximage,kyimage,cdficppm: tailles X,Y et nom de l'image de sortie.
!
! En sortie:
!	Ecriture du fichier cdficppm.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
real(kind=8), intent(inout) :: px(kcourbes,kbris),py(kcourbes,kbris)
real(kind=8) :: zfracx(kbris),zfracy(kbris)
!
!-------------------------------------------------
! Si ldb(jcourbes) est vrai, alors la courbe n° jcourbes comporte 3 colonnes (et la 3ème est la barre d'erreur). Le tableau pb contient alors la valeur de cette barre d'erreur.
! Si ldb(jcourbes) est faux, alors la courbe n° jcourbes comporte 2 colonnes. Le tableau pb est alors initialisé à rindef.
!-------------------------------------------------
!
real(kind=8) pb(kcourbes,kbris)
logical, intent(in) :: ldb(kcourbes)

character*(*) cdtexte(kcourbes),cdficppm
integer(kind=4), parameter :: jpcoul=25 ! nombre max. de couleurs possibles, et donc de courbes gérables.
character(len=15) :: clcoul(jpcourbes)
real(kind=8) zprinc(jplignes)
real(kind=8) zsec(jplignes)
!
!-------------------------------------------------
! Coordonnées des pixels au sein du SVG: les X seront entre 0 et rlxsvg, les Y seront entre
! 0 et rlysvg. Le rapport d'aspect Y/X de l'image finale est contrôlé par
! l'utilisateur, car égal à kyimage/kximage, qu'il peut imposer via
! l'instruction "#IMAGE=".
!-------------------------------------------------
!
rlxsvg=real(kximage)
rlysvg=real(kyimage)
!
!-------------------------------------------------
! Largeur de l'espace à gauche pour le légendage de l'axe Y.
!-------------------------------------------------
!
rxt=0.10*rlxsvg
!
!-------------------------------------------------
! Largeur de de l'espace à droite pour le légendage des couleurs des
! courbes.
!-------------------------------------------------
!
if(kcourbes <= 1) then
  !
  !-------------------------------------------------
  ! Un seule courbe à tracer. Pas la peine d'écrire une légende.
  !-------------------------------------------------
  !
  lgleg=.false.
endif
if(lgleg) then
  rx_legcour=0.27*rlxsvg
else
  rx_legcour=20.
endif
!
!-------------------------------------------------
! Hauteur de l'espace du bas pour le légendage de l'axe X.
!-------------------------------------------------
!
ry_legx=0.10*rlysvg
!
!-------------------------------------------------
! Hauteur de la zone réservée au titre.
!-------------------------------------------------
!
ryt=0.15*rlysvg
if(lgpubli) ryt=14.
!
!-------------------------------------------------
! Au sein de l'espace total, la zone de tracé des courbes est:
!-------------------------------------------------
!
rlxt=rlxsvg-rx_legcour-rxt
rlyt=rlysvg-ryt-ry_legx
!
!-------------------------------------------------
! Cas où l'on souhaite imposer un respect du ratio Y/X sur l'image de sortie,
! i.e. le rapport Y/X du cadre de tracé égal au rapport Y/X des données réelles.
!-------------------------------------------------
!
if(lgxy_isotropes) then
  !
  !-------------------------------------------------
  ! On calcule quel doit être la hauteur en pixels de la zone de tracé,
  ! pour que le rapport d'aspect des données réelles soit respecté.
  !-------------------------------------------------
  !
  zlyt=abs((rymax-rymin)/(rxmax-rxmin))*rlxt
  !
  !-------------------------------------------------
  ! On force cette hauteur en augmentant conséquemment la hauteur de l'image de sortie.
  !-------------------------------------------------
  !
  irlysvg_prec=nint(rlysvg) ! pour diagnostic.
  rlysvg=rlysvg+zlyt-rlyt ! hauteur de l'image de sortie.
  rlysvg=2._8*nint(rlysvg/2._8) ! on rend la hauteur multiple de 2, c'est un besoin shadok du logiciel ffmpeg, qui permettra ensuite de créer des vidéos avec les images générées par dd2gr.
  rlyt=zlyt
  write(*,fmt=*) '  X et Y isotropes. Hauteur de l''image passée de ',irlysvg_prec,' à ',nint(rlysvg)
  write(*,fmt=*) '  Taille de l''image de sortie en pixels :        ',nint(rlxsvg),' x ',nint(rlysvg)
  write(*,fmt=*) ' '
endif
!
!-------------------------------------------------
! Ouverture du fichier SVG.
!-------------------------------------------------
!
nulsvg=40 ; open(nulsvg,file=cgfpix,form='formatted')
write(nulsvg,fmt='(a,f7.2,a,f7.2,a)') '<svg version="1.1" width="',rlxsvg,'" height="',rlysvg,'" baseProfile="full" xmlns="http://www.w3.org/2000/svg">'
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Tout le fond d''une couleur donnée. -->'
write(nulsvg,fmt='(9a)') '<rect width="100%" height="100%" fill="white"/>'
!
!-------------------------------------------------
! Initialisation par défaut des couleurs de courbes.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Cas où on cherche à faire une publication. 
! On se sert au maximum du type de trait
! car imprimer en noir et blanc est une contrainte.
!-------------------------------------------------
!
ic=0
ic=ic+1 ; clcoul(ic)="black"
ic=ic+1 ; clcoul(ic)="red"
ic=ic+1 ; clcoul(ic)="blue"
ic=ic+1 ; clcoul(ic)="green"
ic=ic+1 ; clcoul(ic)="cyan"
ic=ic+1 ; clcoul(ic)="rgb(248,128,23)" ! orange foncé.
!ic=ic+1 ; clcoul(ic)="pink"
!ic=ic+1 ; clcoul(ic)="brown"
!ic=ic+1 ; clcoul(ic)="yellow"
!ic=ic+1 ; clcoul(ic)="grey"
!ic=ic+1 ; clcoul(ic)="purple"
!ic=ic+1 ; clcoul(ic)="lightgreen"
!ic=ic+1 ; clcoul(ic)="rgb(180,25,25)"
!ic=ic+1 ; clcoul(ic)="rgb(100,100,200)"
if(ic > jpcourbes) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac1d/ERREUR: initialisation des couleurs!...'
  write(*,fmt=*) ic,jpcourbes
  call exit(1)
endif

!-------------------------------------------------
! Si l'utilisateur a forcé une périodicité pour la couleur de trait, on l'applique ici.
!-------------------------------------------------
if(ncoulper /= 0) ic=ncoulper
!
!-------------------------------------------------
! On va changer le type de trait à chaque courbe.
!-------------------------------------------------
!
do jcourbes=1,kcourbes
  !
  !-------------------------------------------------
  ! La couleur boucle de façon circulaire.
  !-------------------------------------------------
  !
  clcoul(jcourbes)=clcoul(1+modulo(jcourbes-1,ic))
  !
  !-------------------------------------------------
  ! Le type de trait monte de 1 toutes les courbes.
  !-------------------------------------------------
  !
  if(jcourbes == 1) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='0' ! continu.
  elseif(jcourbes == 2) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='6,6' ! tireté court.
  elseif(jcourbes == 3) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='0,7' ! pointillé.
  elseif(jcourbes == 4) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='13,13' ! tireté long.
  elseif(jcourbes == 5) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='13,6,1,7' ! mixte.
  elseif(jcourbes == 6) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='10,6,1,7,1,7' ! double mixte.
  elseif(jcourbes == 7) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='10,6,1,7,1,7,1,7' ! triple mixte.
  elseif(jcourbes == 8) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='10,6,1,7,1,7,1,7,1,7' ! quadruple mixte.
  elseif(jcourbes == 9) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='1,7,1,13' ! double pointillé espacé.
  elseif(jcourbes == 10) then
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)='1,7,1,7,1,13' ! triple pointillé espacé.
  else
    if(cgpoin(jcourbes) == cgindef) cgpoin(jcourbes)=cgpoin(modulo(jcourbes-1,10)+1)
  endif
  
  !-------------------------------------------------
  ! Si l'utilisateur a forcé une périodicité pour le type de trait, on l'applique ici.
  !-------------------------------------------------
  if(ntraiper /= 0) cgpoin(jcourbes)=cgpoin(modulo(jcourbes-1,ntraiper)+1)
enddo
!
!-------------------------------------------------
! Si l'utilisateur a prescrit des couleurs dans le fichier ".doc", on modifie
! clcoul en conséquence.
!-------------------------------------------------
!
do jc=1,jpcourbes
  if(trim(cgcoul(jc)) /= cgindef) clcoul(jc)=cgcoul(jc)
enddo
!
!-------------------------------------------------
! Ecriture du titre.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.33*ryt)
rgtaille_fonte=min(21.,rlysvg/35.)*rfont

zfont_titre=rgtaille_fonte*rfont_titre
if(cgtitre /= cgindef) then
  if(cgunite /= cgindef) cgtitre=trim(cgtitre)//' ('//trim(cgunite)//')'
  if(cgorigine /= cgindef) cgtitre=trim(cgtitre)//' , '//trim(cgorigine)
  if(.not.lgpubli) then
    write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
    & ,ixtxt,'" y="' &
    & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
    & ,'" text-anchor="middle" fill="',trim(cgcoult),'" >' &
    & ,trim(cgtitre),'</text>'
    
    clsvg_titre=cl_nettoie_blancs(clsvg_tit)
    write(nulsvg,fmt='(9a)') ' '
    write(nulsvg,fmt='(9a)') '<!-- Ecriture du titre. -->'
    write(nulsvg,fmt='(a)') trim(clsvg_titre)
  endif  
endif
!
!-------------------------------------------------
! Ecriture du sous-titre.
!-------------------------------------------------
!
if(trim(cgdate) /= cgindef .and. .not.lgpubli) then
  iytxt=nint(0.67*ryt)
  
  write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
  & ,'" text-anchor="middle" fill="',trim(cgcoult),'" >' &
  & ,trim(cgdate),'</text>'
  
  clsvg_titre=cl_nettoie_blancs(clsvg_tit)
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Ecriture du sous-titre. -->'
  write(nulsvg,fmt='(a)') trim(clsvg_titre)
endif
!
!-------------------------------------------------
! Ecriture du texte libre de l'utilisateur.
!-------------------------------------------------
!
call texl

!
!-------------------------------------------------
! Test de cohérence des extrêmes.
!-------------------------------------------------
!
if(rxmin == rxmax) then
  rxmax=max(rxmin+0.001,rxmin*1.4)
endif
if(rymin == rymax) then
  rymax=max(rymin+0.001,rymin*1.4)
endif
!
!-------------------------------------------------
! On élargit un peu la fenêtre de tracé.
!-------------------------------------------------
!
if(lgdebu) write(*,fmt=*) 'svg_trac1d pré  élargissement fenêtre de tracé/rymin=',rymin
if(lgdebu) write(*,fmt=*) 'svg_trac1d pré  élargissement fenêtre de tracé/rymax=',rymax
!zela_log=1.7 ! élargissement dans le cas échelle LOG.
zela_log=1.001 ! élargissement dans le cas échelle LOG.
!zela_lin=0.05 ! élargissement dans le cas échelle LIN.
zela_lin=0. ! élargissement dans le cas échelle LIN.
!
! Axe x.
!
if(trim(cgtypax) == 'LOG' .or. trim(cgtypax) == '-LOG') then
  if(lgdebu) write(*,fmt=*) 'svg_trac1d/axe x/branche LOG'
  if(rxmax> rxmin) then
    rxmin=rxmin/zela_log ; rxmax=rxmax*zela_log
  else
    rxmax=rxmax/zela_log ; rxmin=rxmin*zela_log
  endif
else
  if(lgdebu) write(*,fmt=*) 'svg_trac1d/axe x/branche LIN'
  if(rxmax> rxmin) then
    zdiff=zela_lin*(rxmax-rxmin) ; rxmin=rxmin-zdiff ; rxmax=rxmax+zdiff
  else
    zdiff=zela_lin*(rxmin-rxmax) ; rxmax=rxmax-zdiff ; rxmin=rxmin+zdiff
  endif
endif
!
! Axe y.
!
if(trim(cgtypay) == 'LOG' .or. trim(cgtypay) == '-LOG') then
  if(lgdebu) write(*,fmt=*) 'svg_trac1d/axe y/branche LOG'
  if(rymax> rymin) then
    rymin=rymin/zela_log ; rymax=rymax*zela_log
  else
    rymax=rymax/zela_log ; rymin=rymin*zela_log
  endif
else
  if(lgdebu) write(*,fmt=*) 'svg_trac1d/axe y/branche LIN'
  if(rymax> rymin) then
    zdiff=zela_lin*(rymax-rymin) ; rymin=rymin-zdiff ; rymax=rymax+zdiff
  else
    zdiff=zela_lin*(rymin-rymax) ; rymax=rymax-zdiff ; rymin=rymin+zdiff
  endif
endif
if(lgdebu) write(*,fmt=*) 'svg_trac1d post élargissement fenêtre de tracé/rymin=',rymin
if(lgdebu) write(*,fmt=*) 'svg_trac1d post élargissement fenêtre de tracé/rymax=',rymax
!
!-------------------------------------------------
! Légendage des axes.
!-------------------------------------------------
!
if(lgdebu) write(*,fmt=*) ' ----------------------------------------------------'
if(lgdebu) write(*,fmt=*) 'Axe X'
if(lgdebu) write(*,fmt=*) ' '
zmin=rxmin
zmax=rxmax
if(lgdebu) write(*,fmt=*) 'svg_trac1d/rxmin=',rxmin
if(lgdebu) write(*,fmt=*) 'svg_trac1d/rxmax=',rxmax
if (trim(cgtrace_axes) /= 'oui') then
  !
  !-------------------------------------------------
  ! Pas de tracé/légendage d'axes.
  !-------------------------------------------------
  !
elseif(trim(cgtypax) == 'LOG' .or. trim(cgtypax) == '-LOG') then
  !
  !-------------------------------------------------
  ! Axe logarithmique.
  !-------------------------------------------------
  !
  call svg_axe_log(zmin,zmax,'X')
elseif((zmin < zmax .and. zmin > 2436934.5 .and. zmax < 2524593.5) &
    .or. (zmin > zmax .and. zmin < 2524593.5 .and. zmax > 2436934.5)) then
  !
  !-------------------------------------------------
  ! La coordonnée est une date julienne entre 1960 et 2200.
  !-------------------------------------------------
  !
  write(*,fmt=*) '  La coordonnée X est une date julienne'
  call svg_tralg(zmin,zmax,'X')
else
  if(zmin > zmax) then
    ztmp=zmin
    zmin=zmax
    zmax=ztmp
  endif
  call lega(zmin,zmax,rgxret,iprinc,zprinc,isec,zsec) ! axe des X.
  call tralps(iprinc,zprinc,isec,zsec,'X') ! tracé des lignes principales et secondaires de l'axe X.
endif

if(lgdebu) write(*,fmt=*) ' ----------------------------------------------------'
if(lgdebu) write(*,fmt=*) 'Axe Y'
if(lgdebu) write(*,fmt=*) ' '
zmin=rymin
zmax=rymax
if(lgdebu) write(*,fmt=*) 'svg_trac1d/rymin=',rymin
if(lgdebu) write(*,fmt=*) 'svg_trac1d/rymax=',rymax
if (trim(cgtrace_axes) /= 'oui') then
  !
  !-------------------------------------------------
  ! Pas de tracé/légendage d'axes.
  !-------------------------------------------------
  !
elseif(trim(cgtypay) == 'LOG' .or. trim(cgtypay) == '-LOG') then
  !
  !-------------------------------------------------
  ! Axe logarithmique.
  !-------------------------------------------------
  !
  call svg_axe_log(zmin,zmax,'Y')
elseif((zmin < zmax .and. zmin > 2436934.5 .and. zmax < 2524593.5) &
    .or. (zmin > zmax .and. zmin < 2524593.5 .and. zmax > 2436934.5)) then
  !
  !-------------------------------------------------
  ! La coordonnée est une date julienne entre 1960 et 2200.
  !-------------------------------------------------
  !
  write(*,fmt=*) '  La coordonnée Y est une date julienne'
  call svg_tralg(zmin,zmax,'Y')
else
  if(zmin > zmax) then
    ztmp=zmin
    zmin=zmax
    zmax=ztmp
  endif
  call lega(zmin,zmax,rgyret,iprinc,zprinc,isec,zsec) ! axe des Y.
  call tralps(iprinc,zprinc,isec,zsec,'Y') ! tracé des lignes principales et secondaires de l'axe Y.
endif
!
!-------------------------------------------------
! Tracé des courbes.
!-------------------------------------------------
!
do jcourbes=1,kcourbes
  if(index(cgpoin(jcourbes),'.') /= 0) then
    !
    !-------------------------------------------------
    ! La ligne "#POIN=" de l'utilisateur a fourni un nombre réel.
    ! C'est qu'il veut un tracé type "nuage des points".
    !-------------------------------------------------
    !
    llpointage=.true.
  else
    !
    !-------------------------------------------------
    ! La ligne "#POIN=" de l'utilisateur n'a pas fourni un nombre réel.
    ! C'est qu'il veut un tracé type "segments de droite".
    !-------------------------------------------------
    !
    llpointage=.false.
  endif
  write(nulsvg,fmt='(9a)') ' '
  if(llpointage) then
    write(nulsvg,fmt='(a,i2,a)') '<!-- Tracé du nuage de points n° ',jcourbes,' donné par ses centres de cercles. -->'
  else
    write(nulsvg,fmt='(a,i2,a)') '<!-- Tracé de la courbe n° ',jcourbes,' donnée par ses segments de droite. -->'
  endif
  !
  !-------------------------------------------------
  ! Largeur de courbe.
  !-------------------------------------------------
  !
  if(cglarg(jcourbes) /= cgindef) then
    !
    !-------------------------------------------------
    ! L'utilisateur impose sa largeur de courbe.
    !-------------------------------------------------
    !
    clratio=cglarg(jcourbes)(index(cglarg(jcourbes),'=')+1:)
    read(clratio,fmt=*) zratio_courbe
  else
    !
    !-------------------------------------------------
    ! L'utilisateur n'a pas imposé sa largeur de courbe.
    !-------------------------------------------------
    !
    zratio_courbe=1.
  endif
  !
  !-------------------------------------------------
  ! Dans le cas d'une publication on élargit le trait, car la publi est souvent lue ou imprimée en A4 bi-colonne.
  !-------------------------------------------------
  !
  if(lgpubli) zratio_courbe=zratio_courbe*1.6
  zwidth=2.3e-3*rlxsvg*zratio_courbe
  !
  !-------------------------------------------------
  ! Gestion des pointillés éventuels.
  !-------------------------------------------------
  !
  if(trim(cgpoin(jcourbes)) == trim(cgindef) .or. trim(cgpoin(jcourbes)) == 'non' .or. llpointage) then
    cldash=' '
  else
    cldash='; stroke-dasharray: '//trim(cgpoin(jcourbes))
  endif
  do jbris=1,kbris
    !
    !-------------------------------------------------
    ! Coordonnées X associées au jbris ième point de la ligne brisée.
    !-------------------------------------------------
    !
    if(px(jcourbes,jbris) == pindef .or. abs((px(jcourbes,jbris)-psaut)/psaut) < 1.e-7) then
      !
      !-------------------------------------------------
      ! Le point est un indef ou un saut.
      !-------------------------------------------------
      !
      zfracx(jbris)=pindef
    elseif(trim(cgtypax) == 'LIN') then
      !
      !-------------------------------------------------
      ! Echelle linéaire.
      !-------------------------------------------------
      !
      zfracx(jbris)=(px(jcourbes,jbris)-rxmin)/(rxmax-rxmin)
    elseif(trim(cgtypax) == 'LOG' .or. trim(cgtypax) == '-LOG') then
      !
      !-------------------------------------------------
      ! Echelle logarithmique.
      !-------------------------------------------------
      !
      zfracx(jbris)=(log(px(jcourbes,jbris))-log(rxmin))/(log(rxmax)-log(rxmin))
    else
      write(*,fmt=*)
      write(*,fmt=*) 'svg_trac1d/ERREUR: cas de cgtypax non prévu !...'
      write(*,fmt=*) trim(cgtypax)
      call exit(1)
    endif
    !
    !-------------------------------------------------
    ! Coordonnées Y associées au jbris ième point de la ligne brisée.
    !-------------------------------------------------
    !
    if(py(jcourbes,jbris) == pindef .or. abs((py(jcourbes,jbris)-psaut)/psaut) < 1.e-7) then
      !
      !-------------------------------------------------
      ! Le point est un indef ou un saut.
      !-------------------------------------------------
      !
      zfracy(jbris)=pindef
    elseif(trim(cgtypay) == 'LIN') then
      !
      !-------------------------------------------------
      ! Echelle linéaire.
      !-------------------------------------------------
      !
      zfracy(jbris)=(py(jcourbes,jbris)-rymin)/(rymax-rymin)
    elseif(trim(cgtypay) == 'LOG' .or. trim(cgtypay) == '-LOG') then
      !
      !-------------------------------------------------
      ! Echelle logarithmique.
      !-------------------------------------------------
      !
      if(py(jcourbes,jbris) <= 0.) py(jcourbes,jbris)=rymin
      zfracy(jbris)=(log(py(jcourbes,jbris))-log(rymin))/(log(rymax)-log(rymin))
    else
      write(*,fmt=*)
      write(*,fmt=*) 'svg_trac1d/ERREUR: cas de cgtypay non prévu !...'
      write(*,fmt=*) trim(cgtypay)
      call exit(1)
    endif
  enddo
  if(llpointage) then
    read(cgpoin(jcourbes),fmt=*) zrayon
    zrayon=zrayon*3.2
    call svg_pointage(zfracx,zfracy,kbris,pindef,clcoul(jcourbes),zrayon)
  else
    zseuil=0.
    call svg_ligne_polygonale(zfracx,zfracy,kbris,pindef,clcoul(jcourbes),lgplusse(jcourbes),cgpoin(jcourbes),zwidth,zseuil)
  endif
  !
  !-------------------------------------------------
  ! Ecriture des barres d'erreur, si l'utilisateur en a fourni.
  !-------------------------------------------------
  !
  if(ldb(jcourbes) .and. trim(cgtypay) == 'LIN' .and. .not. llpointage) then
    !
    !-------------------------------------------------
    ! Ecriture d'une ligne polygonale.
    !-------------------------------------------------
    !
    write(cldeb,fmt='(3a,f8.2,100a)') '<polyline style="fill:none;stroke:',trim(clcoul(jcourbes)),';stroke-width:',zwidth,trim(cldash),'" points="'
    cldeb=cl_nettoie_blancs(cldeb)
    do jbris=1,kbris
      if(pb(jcourbes,jbris) /= rindef) then
        !
        !-------------------------------------------------
        ! La barre d'erreur est fournie, pour cette courbe et ce point.
        ! On va tracer une moustache centrée sur le point courant.
        !-------------------------------------------------
        !
        !
        !-------------------------------------------------
        ! Coordonnées X associées au jbris ième point de la ligne brisée.
        !-------------------------------------------------
        !
        if(trim(cgtypax) == 'LIN') then
          zfrac=(px(jcourbes,jbris)-rxmin)/(rxmax-rxmin)
        elseif(trim(cgtypax) == 'LOG' .or. trim(cgtypax) == '-LOG') then
          zfrac=(log(px(jcourbes,jbris))-log(rxmin))/(log(rxmax)-log(rxmin))
        else
          write(*,fmt=*)
          write(*,fmt=*) 'svg_trac1d/ERREUR: cas de cgtypax non prévu !...'
          write(*,fmt=*) trim(cgtypax)
          call exit(1)
        endif
        if(zfrac < 0. .or. zfrac > 1.) cycle 
        zxc=rxt+zfrac*rlxt
        zfrac=(py(jcourbes,jbris)-rymin)/(rymax-rymin)
        if(zfrac < 0. .or. zfrac > 1.) cycle 
        zyc=ryt+(1.-zfrac)*rlyt
        !
        !-------------------------------------------------
        ! Extension de la moustache, en pixels.
        !-------------------------------------------------
        !
        zdeltax=3.
        zdeltay=pb(jcourbes,jbris)/(rymax-rymin)*rlyt
        !
        !-------------------------------------------------
        ! Ecriture de la moustache dans le SVG.
        !-------------------------------------------------
        !
        write(cltrait,fmt='(3a,f8.2,2a,12(f8.2,a),100a)') '<polyline style="fill:none;stroke:',trim(clcoul(jcourbes)),';stroke-width:',zwidth,trim(cldash) &
          & ,'" points="' &
          &,zxc-zdeltax,', ',zyc-zdeltay,' ',zxc+zdeltax,', ',zyc-zdeltay,' ' &
          &,zxc        ,', ',zyc-zdeltay,' ',zxc        ,', ',zyc+zdeltay,' ' &
          &,zxc+zdeltax,', ',zyc+zdeltay,' ',zxc-zdeltax,', ',zyc+zdeltay,' ' &
          &,'" />'
        write(nulsvg,fmt='(a)') ' '
        write(nulsvg,fmt='(9a)') '<!-- Tracé d''une moustache - barre d''erreur. -->'
        write(nulsvg,fmt='(a)') trim(cltrait)
      endif
    enddo
  endif
  if(lgleg) then
    !
    !-------------------------------------------------
    ! Légende de la courbe courante.
    ! Tracé du trait pour donner la couleur de la courbe.
    !-------------------------------------------------
    !
    zx1=rxt+rlxt+0.12*rx_legcour
    zx2=rxt+rlxt+0.32*rx_legcour
    zinterl=rlyt/max(10.,real(kcourbes+3)) ! interligne entre deux tirets de courbes.
    zy1=ryt+real(jcourbes)*zinterl
    zy2=zy1
    llpoinlc=cgchamp(jcourbes) /= ' ' ! vrai si une légende de cette courbe doit être écrite.
    if(llpointage .and. llpoinlc) then
      !
      !-------------------------------------------------
      ! Tracé d'un cercle coloré.
      !-------------------------------------------------
      !
      zxcercle=0.5*(zx1+zx2)
      write(clcercle,fmt=*) '<circle cx="',zxcercle,'" cy="',zy1,'" r="',zrayon,'" fill="',trim(clcoul(jcourbes)),'"  />'
      clcercle=cl_nettoie_blancs(clcercle)
      write(nulsvg,fmt='(a)') trim(clcercle)
    elseif(llpoinlc) then
      !
      !-------------------------------------------------
      ! Tracé d'un segment.
      !-------------------------------------------------
      !
      write(cltrait,fmt='(3a,f8.2,a,4(a,f8.2),100a)') '<polyline style="fill:none;stroke:',trim(clcoul(jcourbes)),';stroke-linecap:round;stroke-width:',zwidth,trim(cldash),'" points="',zx1,', ',zy1,' ',zx2,', ',zy2,'" />'
      write(nulsvg,fmt='(a)') ' '
      write(nulsvg,fmt='(9a)') '<!-- Tracé du trait pour donner la couleur de la courbe. -->'
      write(nulsvg,fmt='(a)') trim(cltrait)
    endif
    !
    !-------------------------------------------------
    ! Ecriture du nom en clair associé à la courbe.
    ! Ce nom est dans cgchamp(jcourbes)
    !-------------------------------------------------
    !
    if(llpoinlc) then
      cltxt=cgchamp(jcourbes)
      !
      ! Dans le cas où l'utilisateur a mis de longs noms de courbes, on pratique un fonte plus petite.
      ! zfrac_leg vaut 1. pour des longueurzfrac_legs de noms <= 13, et décroît vers 0. pour les longs noms.
      !
      zfrac_leg=1./(1.+(max(13.,real(len_trim(cltxt)))-13.)/13.)
      ztaille_leg=rgtaille_fonte*rfont_leg*zfrac_leg
      if(lgpubli) ztaille_leg=ztaille_leg*1.3 ! impression en A4 bi-colonne.
      ixtxt=zx2+0.04*rx_legcour
      iytxt=zy1+0.3*ztaille_leg
      
      write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
      & ,ixtxt,'" y="' &
      & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',ztaille_leg &
      & ,'" text-anchor="left" fill="black" >' &
      & ,trim(cltxt),'</text>'
      
      clsvg_texte=cl_nettoie_blancs(clsvg_txt)
      write(nulsvg,fmt='(9a)') ' '
      write(nulsvg,fmt='(9a)') '<!-- Ecriture du nom en clair associé à la courbe. -->'
      write(nulsvg,fmt='(a)') trim(clsvg_texte)
    endif
  endif ! lgleg.
enddo
!
!-------------------------------------------------
! Cadre intérieur: zone de tracé des courbes: ligne brisée fermée.
!-------------------------------------------------
!
zxsomme=rxt+rlxt
zysomme=ryt+rlyt
zcadre_width=9.0e-4*rlxsvg
write(clc,fmt=*) '<path d="M ',rxt,',',ryt,' L ',rxt &
& ,',',zysomme,' L ',zxsomme,',',zysomme,' L ' &
& ,zxsomme,',',ryt,' Z" style="fill:none; stroke:' &
& ,'black','" stroke-width="',zcadre_width,'" />'
clc=cl_nettoie_blancs(clc)
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Cadre intérieur: zone de tracé des courbes. -->'
write(nulsvg,fmt='(a)') trim(clc)
!
!-------------------------------------------------
! Fermeture du fichier SVG.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt='(9a)') '<!-- Indicateur de fin de fichier SVG. -->'
write(nulsvg,fmt='(a)') '</svg>'
close(nulsvg)
!
!-------------------------------------------------
! Epilogue.
!-------------------------------------------------
!
write(*,fmt=*) ' '
write(*,fmt=*) '  Fichier SVG écrit : ',trim(cgfpix)
write(*,fmt=*) ' '
end
