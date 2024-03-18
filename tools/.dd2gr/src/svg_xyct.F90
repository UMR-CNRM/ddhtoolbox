subroutine svg_xyct(kdta,px,py,cdrvb,cdtexte)
! --------------------------------------------------------------
! **** *xyct* Ecriture d'un fichier image SVG, contenant un pointage de texte.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-03, J.M. Piriou, sur un besoin d'Eric Bazile.
! Modifications:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
!
real(kind=8), intent(in) :: px(kdta)
real(kind=8), intent(in) :: py(kdta)
character(len=*), intent(in) :: cdrvb(kdta)
character(len=*), intent(in) :: cdtexte(kdta)

integer(kind=4) :: irvb(3)
character(len=180) :: clmot(40)
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
rlxsvg=real(nximage)
rlysvg=real(nyimage)
rlysvg=2._8*nint(rlysvg/2._8) ! on rend la hauteur multiple de 2, c'est un besoin shadok du logiciel ffmpeg, qui permettra ensuite de créer des vidéos avec les images générées par dd2gr.
if(trim(cgtrace_axes) == 'oui') then
  !
  !-------------------------------------------------
  ! Largeur de l'espace à gauche pour le légendage de l'axe Y.
  !-------------------------------------------------
  !
  rxt=0.10*rlxsvg
  !
  !-------------------------------------------------
  ! Hauteur de l'espace du bas pour le légendage de l'axe X.
  !-------------------------------------------------
  !
  ry_legx=0.10*rlysvg
else
  !
  !-------------------------------------------------
  ! Largeur de l'espace à gauche pour le légendage de l'axe Y.
  !-------------------------------------------------
  !
  rxt=0.02*rlxsvg
  !
  !-------------------------------------------------
  ! Hauteur de l'espace du bas pour le légendage de l'axe X.
  !-------------------------------------------------
  !
  ry_legx=0.02*rlysvg
endif
!
!-------------------------------------------------
! Largeur de l'espace à droite pour le légendage des couleurs des courbes.
!-------------------------------------------------
!
rx_legcour=0.20*rlxsvg
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
  if(trim(cgformat) == 'LLV') then
    !
    ! Isotropie sur la sphère projetée.
    !
    zpi=4.*atan(1.)
    zlyt=(rymax-rymin)/((rxmax-rxmin)*cos((0.5*(rymax+rymin))*zpi/180.))*rlxt
  else
    !
    ! Isotropie simple.
    !
    zlyt=(rymax-rymin)/(rxmax-rxmin)*rlxt
  endif
  !
  !-------------------------------------------------
  ! On force cette hauteur en augmentant conséquemment la hauteur de l'image de sortie.
  !-------------------------------------------------
  !
  rlysvg=rlysvg+zlyt-rlyt ! hauteur de l'image de sortie.
  rlysvg=2._8*nint(rlysvg/2._8) ! on rend la hauteur multiple de 2, c'est un besoin shadok du logiciel ffmpeg, qui permettra ensuite de créer des vidéos avec les images générées par dd2gr.
  rlyt=zlyt
endif
!
!-------------------------------------------------
! Ouverture du fichier SVG.
!-------------------------------------------------
!
nulsvg=40 ; open(nulsvg,file=cgfpix,form='formatted')
write(nulsvg,fmt='(a,f7.2,a,f7.2,a)') '<svg version="1.1" width="',rlxsvg,'" height="',rlysvg,'" baseProfile="full" xmlns="http://www.w3.org/2000/svg">'
write(nulsvg,fmt='(9a)') '<!-- Tout le fond d''une couleur donnée. -->'
write(nulsvg,fmt='(9a)') '<rect width="100%" height="100%" fill="white"/>'
!
!-------------------------------------------------
! Ecriture du titre.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.33*ryt)
rgtaille_fonte=min(21.,rlysvg/35.)*rfont

if(cgtitre /= cgindef) then
  if(cgorigine /= cgindef) cgtitre=trim(cgtitre)//' , '//trim(cgorigine)
endif

zfont_titre=rgtaille_fonte*rfont_titre
write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
& ,'" text-anchor="middle" fill="',trim(cgcoult),'" >' &
& ,trim(cgtitre),'</text>'

clsvg_titre=cl_nettoie_blancs(clsvg_tit)
if(.not.lgpubli) write(nulsvg,fmt='(9a)') '<!-- Ecriture du titre. -->'
if(.not.lgpubli) write(nulsvg,fmt='(a)') trim(clsvg_titre)
!
!-------------------------------------------------
! Ecriture du sous-titre.
!-------------------------------------------------
!
if(trim(cgdate) /= cgindef) then
  iytxt=nint(0.67*ryt)
  
  write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
  & ,'" text-anchor="middle" fill="',trim(cgcoult),'" >' &
  & ,trim(cgdate),'</text>'
  
  clsvg_titre=cl_nettoie_blancs(clsvg_tit)
  if(.not.lgpubli) write(nulsvg,fmt='(9a)') '<!-- Ecriture du titre. -->'
  if(.not.lgpubli) write(nulsvg,fmt='(a)') trim(clsvg_titre)
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
! Tracé 2D proprement dit.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') '<!-- Tracé 2D proprement dit. -->'
call img_pal_init
!
!-------------------------------------------------
! Pointage de valeur irrégulières, avec des textes de couleur.
!-------------------------------------------------
!
!-------------------------------------------------
! Boucle sur les kdta triplets (x,y,v).
!-------------------------------------------------
!
write(*,fmt=*) '  Pointage de textes colorés...'
do jdta=1,kdta
  !
  !-------------------------------------------------
  ! La chaîne clrvb est du type "255,143,23". On la casse en ses 3 entiers.
  !-------------------------------------------------
  !
  call casc(cdrvb(jdta),10,clmot,ilmot)
  read(clmot(1),fmt=*) irvb(1)
  read(clmot(2),fmt=*) irvb(2)
  read(clmot(3),fmt=*) irvb(3)
  !
  !-------------------------------------------------
  ! Position X et Y du centre du cercle.
  !-------------------------------------------------
  !
  zx1=rxt+rlxt*(px(jdta)-rxmin)/(rxmax-rxmin)
  zy1=ryt+rlyt*(rymax-py(jdta))/(rymax-rymin)

  ztaille=0.75*rgtaille_fonte ! taille du texte de label.
  if(trim(cglabt) /= trim(cgindef)) then
    !
    !-------------------------------------------------
    ! L'utilisateur a spécifié "#LABEL_TAILLE=valeur" dans le ".doc".
    ! On lit cette valeur et la multiplie par la taille standard.
    !-------------------------------------------------
    !
    read(cglabt,fmt=*) zmult
    ztaille=ztaille*zmult
  endif
  llcercle_plus_texte=.true.
  llcercle_plus_texte=.false.
  if(llcercle_plus_texte) then
    !
    !-------------------------------------------------
    ! Cercle + label.
    !-------------------------------------------------
    !
    !
    !-------------------------------------------------
    ! Rayon du disque fourni par l'utilisateur.
    !-------------------------------------------------
    !
    if(cgpoin(0) == cgindef) then
      zmultray=1.
    else
      read(cgpoin(0),fmt=*) zmultray
    endif
    !
    !-------------------------------------------------
    ! Ecriture du cercle en SVG.
    !-------------------------------------------------
    !
    zrayon=4.*zmultray
    write(clcercle,fmt='(3(a,g14.5),3(a,i3.3),a)') '<circle cx="',zx1,'" cy="',zy1,'" r="',zrayon &
      & ,'" fill="rgb(',irvb(1),',',irvb(2),',',irvb(3) &
      & ,')"  />'
    clcercle=cl_nettoie_blancs(clcercle)
    write(nulsvg,fmt='(a)') trim(clcercle)
    !
    !-------------------------------------------------
    ! Label : on écrit la valeur du champ en ce point.
    !-------------------------------------------------
    !
    zx_label=zx1+1.6*zrayon
    zy_label=zy1

    write(cltxt,fmt='(a,f10.2,a,f40.2,3a,g16.7,a,i3,a,i3,a,i3,3a)') '<text x="' &
    & ,zx_label,'" y="' &
    & ,zy_label,'" ',trim(cgfonte_texte),' font-size="',ztaille &
    & ,'" text-anchor="left" fill="rgb(',irvb(1),',',irvb(2),',',irvb(3) &
      & ,')" >' &
    & ,trim(cdtexte(jdta)),'</text>'
  else
    !
    !-------------------------------------------------
    ! Label, centré.
    !-------------------------------------------------
    !
    zx_label=zx1
    zy_label=zy1+0.3*ztaille
    write(cltxt,fmt='(a,f10.2,a,f40.2,3a,g16.7,a,i3,a,i3,a,i3,3a)') '<text x="' &
    & ,zx_label,'" y="' &
    & ,zy_label,'" ',trim(cgfonte_texte),' font-size="',ztaille &
    & ,'" text-anchor="middle" fill="rgb(',irvb(1),',',irvb(2),',',irvb(3) &
      & ,')" >' &
    & ,trim(cdtexte(jdta)),'</text>'
  endif
  !
  ! Ecriture de ce texte sur le SVG.
  !
  cltxt=cl_nettoie_blancs(cltxt)
  write(nulsvg,fmt='(a)') trim(cltxt)
enddo
if(trim(cgtrace_axes) == 'oui') then
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
  if((zmin < zmax .and. zmin > 2436934.5 .and. zmax < 2524593.5) &
    .or. (zmin > zmax .and. zmin < 2524593.5 .and. zmax > 2436934.5)) then
    !
    !-------------------------------------------------
    ! La coordonnée est une date julienne entre 1960 et 2200.
    !-------------------------------------------------
    !
    call svg_tralg(zmin,zmax,'X')
  else
    call lega(zmin,zmax,rgxret,iprinc,zprinc,isec,zsec) ! axe des X.
    call tralps(iprinc,zprinc,isec,zsec,'X') ! tracé des lignes principales et secondaires de l'axe X.
  endif
  
  if(lgdebu) write(*,fmt=*) ' ----------------------------------------------------'
  if(lgdebu) write(*,fmt=*) 'Axe Y'
  if(lgdebu) write(*,fmt=*) ' '
  zmin=rymin
  zmax=rymax
  if(zmin> 2436934.5 .and. zmax < 2524593.5) then
    !
    !-------------------------------------------------
    ! La coordonnée est une date julienne entre 1960 et 2200.
    !-------------------------------------------------
    !
    call svg_tralg(zmin,zmax,'Y')
  else
    call lega(zmin,zmax,rgyret,iprinc,zprinc,isec,zsec) ! axe des Y.
    call tralps(iprinc,zprinc,isec,zsec,'Y') ! tracé des lignes principales et secondaires de l'axe Y.
  endif
endif
!
!-------------------------------------------------
! Superposition du fond de carte.
!-------------------------------------------------
!
if(lgfdc) then
  call svg_fdc(kximage,kyimage,ibordblcg,ibordblch,ixpix,iypix)
endif
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
