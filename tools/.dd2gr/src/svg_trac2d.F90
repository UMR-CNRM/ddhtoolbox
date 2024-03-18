subroutine svg_trac2d(px,py,pz,kdta,pval,kx,ky,pmin,pmax,cdcoord,cdpal &
&,ldlegende,ldlegendexy,cdlegx,cdlegy,kximage &
&,kyimage,pindef,krvb_indef,cdficsvg,ldclose)
! --------------------------------------------------------------
! **** *trac2d* Ecriture d'un fichier image, contenant le tracé 2D d'un tableau 2D de réels.
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
!   px: tableau 1D des données irrégulières à tracer.
!   py: tableau 1D des données irrégulières à tracer.
!   pz: tableau 1D des données irrégulières à tracer.
!   kdta: nombre de valeur dans le tableau pz.
! 	pval: tableau rectangulaire des réels à tracer.
!	kx,ky: dimensions de pval.
!	pmin,pmax: valeurs de pval à associer au minimum (resp. maximum) de la palette de couleurs de sortie.
!	cdcoord: coordonnées des axes X et Y:
!		cdcoord='non' si les coordonnées ne sont pas fournies,
!		cdcoord='1.5  28.  -78.   45.' si les coordonnées réelles
!			* du bas gauche du carré en bas à gauche sont X=1.5 Y=-78.
!			* du haut droite du carré en haut à droite sont X=28. Y=45.
!		en l'état actuel du logiciel ces coordonnées ne servent que si l'on superpose
!		un fond de carte.
!	cdpal: palette de couleurs demandée.
!	ldlegende: vrai si on veut une légende du lien entre couleurs et valeurs réelles.
!	cdlegx, cdlegy: si ldlegende est vrai cdlegx et cdlegy contiennent légende de ces axes (nom, unité, etc).
!	ldlegendexy: vrai si on veut une légende des axes X et Y.
!	kximage,kyimage,cdficsvg: tailles X,Y et nom de l'image de sortie.
!  pindef: valeur réelle que prend le tableau pval, lorsque l'utilisateur veut spécifier une valeur manquante.
!  krvb_indef(3): couleur que l'utilisateur veut voir associée aux valeurs manquantes.
! En sortie:
!	Ecriture du fichier cdficsvg.
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
!
real(kind=8), intent(inout) :: px(kdta)
real(kind=8), intent(inout) :: py(kdta)
real(kind=8), intent(inout) :: pz(kdta)
real(kind=8), intent(in) :: pval(kx,ky)
character(len=*), intent(in) :: cdpal,cdficsvg,cdcoord,cdlegx,cdlegy
integer(kind=4), intent(in) :: krvb_indef(3)
logical, intent(in) :: ldclose
integer(kind=4), parameter :: jpcoul=15 ! nombre max. de couleurs possibles, et donc de courbes gérables.
character(len=15) :: clcourbes(jpcoul)
real(kind=8) zprinc(jplignes)
real(kind=8) zsec(jplignes)
integer(kind=4) :: irvb(3)
integer(kind=4) :: irvb_fic(3,kx,ky)
integer(kind=4), allocatable :: irvb_base64(:,:,:)
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
if(trim(cgformat) == 'HAS' .or. trim(cgformat) == 'HAA') then
  !
  !-------------------------------------------------
  ! Cas particulier des tracés type Hayashi. Besoin de plus de place à droite pour coter les jours sur l'axe de droite.
  !-------------------------------------------------
  !
  rx_legcour=0.26*rlxsvg
!elseif(.not.lgc2d .or. .not.lgleg) then
!  !
!  !-------------------------------------------------
!  ! L'utilisateur ne désire pas coloriser le tracé 2D.
!  ! On ne va tracer que des isolignes.
!  ! On peut réduire au strict minimum l'espace à droite pour le légendage des couleurs des courbes.
!  !-------------------------------------------------
!  !
!  rx_legcour=0.07*rlxsvg
else
  !
  !-------------------------------------------------
  ! Cas général.
  !-------------------------------------------------
  !
  rx_legcour=0.20*rlxsvg
endif
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
!if(lgxy_isotropes .and. .not.lgimaf) then
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
  irlysvg_prec=nint(rlysvg) ! pour diagnostic.
  rlysvg=rlysvg+zlyt-rlyt ! hauteur de l'image de sortie.
  rlysvg=2._8*nint(rlysvg/2._8) ! on rend la hauteur multiple de 2, c'est un besoin shadok du logiciel ffmpeg, qui permettra ensuite de créer des vidéos avec les images générées par dd2gr.
  rlyxvg=2._8*nint(rlxsvg/2._8) ! on rend la largeur multiple de 2, c'est un besoin shadok du logiciel ffmpeg, qui permettra ensuite de créer des vidéos avec les images générées par dd2gr.
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
write(nulsvg,fmt='(a,f7.2,a,f7.2,a)') '<svg version="1.1" width="',rlxsvg,'" height="',rlysvg,'" baseProfile="full" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" >'
write(nulsvg,fmt='(9a)') '<!-- Tout le fond d''une couleur donnée. -->'
write(nulsvg,fmt='(9a)') '<rect width="100%" height="100%" fill="white"/>'
!
!-------------------------------------------------
! Ecriture du titre.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.25*ryt)
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
  iytxt=nint(0.50*ryt)
  
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
! Ecriture des min/max.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.75*ryt)

zvaltmp=rcmin_reel ; call reecar(zvaltmp,-1,3,clmin,iltmp)
zvaltmp=rcmax_reel ; call reecar(zvaltmp,-1,3,clmax,iltmp)
zvaltmp=rcmoy ; call reecar(zvaltmp,-1,3,clmoy,iltmp)
zvaltmp=rcect ; call reecar(zvaltmp,-1,3,clect,iltmp)
zvaltmp=rcrcm ; call reecar(zvaltmp,-1,3,clrcm,iltmp)
if(trim(cglang) == 'ENG') then
  write(clstit,fmt='(100a)') 'Min=',trim(clmin),' Max=',trim(clmax) &
  & ,' Mean=',trim(clmoy) &
  & ,' StdD=',trim(clect) &
  & ,' RMS=',trim(clrcm)
else
  write(clstit,fmt='(100a)') 'Min=',trim(clmin),' Max=',trim(clmax) &
  & ,' Moy=',trim(clmoy) &
  & ,' Ect=',trim(clect) &
  & ,' Rcm=',trim(clrcm)
endif

if(lgminmax) then
  write(clsvg_tit,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',zfont_titre &
  & ,'" text-anchor="middle" fill="',trim(cgcoult),'" >' &
  & ,trim(clstit),'</text>'
  
  clsvg_titre=cl_nettoie_blancs(clsvg_tit)
  if(.not.lgpubli) write(nulsvg,fmt='(9a)') '<!-- Ecriture du sous-titre. -->'
  if(.not.lgpubli) write(nulsvg,fmt='(a)') trim(clsvg_titre)
endif

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
write(*,fmt=*) '  Palette de couleurs : ',trim(cdpal)
if(cgimpf /= ' ') then
  
  !-------------------------------------------------
  ! Cas du pointage d'IMPacts de Foudre (cgimpf). On ne trace pas le champ de densité de foudre.
  !-------------------------------------------------
elseif(lgpointage .and. lgc2d) then
  !
  !-------------------------------------------------
  ! Pointage de valeur irrégulières, avec des disques de couleur.
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
  ! Rayon par défaut.
  !-------------------------------------------------
  !
  zrayd=0.7*sqrt(rlxt*rlyt/real(kdta)/3.1416)
  !
  !-------------------------------------------------
  ! Rayon nominal.
  !-------------------------------------------------
  !
  zrayn=min(3.2,zrayd)
  !
  !-------------------------------------------------
  ! Rayon combiné.
  !-------------------------------------------------
  !
  zrayon_ref=zrayn*zmultray
  write(*,fmt=*) ' '
  write(*,fmt=*) '  Rayon du disque : défaut      ',zrayd
  write(*,fmt=*) '  Rayon du disque : nominal     ',zrayn
  write(*,fmt=*) '  Rayon du disque : utilisateur ',zmultray
  write(*,fmt=*) '  Rayon du disque : final       ',zrayon_ref
  if(lgpoil) then
    !
    !-------------------------------------------------
    ! Pointage de lieux. On trie le fichier pour mettre ces lieux à la fin, pour que le disque généré ne soit pas écrasé par
    ! le dessin de ses voisins.
    !-------------------------------------------------
    !
    iperm=kdta
    do jdta=1,kdta
      if(abs(pz(jdta)-rgpointage) < 0.01 .and. jdta < iperm) then
        !
        !-------------------------------------------------
        ! On permute jdta et iperm.
        !-------------------------------------------------
        !
        zinterm=px(jdta)
        px(jdta)=px(iperm)
        px(iperm)=zinterm
        !
        zinterm=py(jdta)
        py(jdta)=py(iperm)
        py(iperm)=zinterm
        !
        zinterm=pz(jdta)
        pz(jdta)=pz(iperm)
        pz(iperm)=zinterm
        !
        iperm=iperm-1
      endif
    enddo
  endif
  !
  !-------------------------------------------------
  ! Boucle sur les kdta triplets (x,y,v).
  !-------------------------------------------------
  !
  write(*,fmt=*) '  Pointage de disques colorés...'
  do jdta=1,kdta
    zval=pz(jdta) ! valeur réelle dont on cherche la couleur.
    !
    !-------------------------------------------------
    ! Couleur RVB associée à cette valeur réelle.
    !-------------------------------------------------
    !
    call couleur_relle(zval,pmin,pmax,pindef,cdpal,krvb_indef,irvb)
    !
    !-------------------------------------------------
    ! Position X et Y du centre du cercle.
    !-------------------------------------------------
    !
    zx1=rxt+rlxt*(px(jdta)-rxmin)/(rxmax-rxmin)
    zy1=ryt+rlyt*(rymax-py(jdta))/(rymax-rymin)
    !
    !-------------------------------------------------
    ! Tracé d'un disque noir si l'utilisateur a mis la valeur rgpointage dans ses données.
    !-------------------------------------------------
    !
    if(abs(zval-rgpointage) < 0.01) then
      irvb(:)=0
      zrayon=zrayon_ref*rgfacp
    else
      zrayon=zrayon_ref
    endif
    !
    !-------------------------------------------------
    ! Ecriture du cercle en SVG.
    !-------------------------------------------------
    !
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
    if(trim(cglabf) /= trim(cgindef)) then
      !
      !-------------------------------------------------
      ! cglabf contient le format fortran, par exemple "F4.0".
      !-------------------------------------------------
      !
      !
      ! Lecture de l'angle de rotation du texte: cglaba en degrés (LABel Angle).
      !
      read(cglaba,fmt=*) zlaba
      ilaba=-nint(zlaba)
      zlaba_rad=atan(1.)/45.*zlaba
      !
      !-------------------------------------------------
      ! Coordonnées du label.
      !-------------------------------------------------
      !
      zx_label=zx1+1.6*zrayon*cos(zlaba_rad)
      zy_label=zy1-1.6*zrayon*sin(zlaba_rad)
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
      clfmt='('//trim(cglabf)//')'
      write(cllabel,fmt=clfmt) zval
      
      write(cltxt,fmt='(a,f10.2,a,f40.2,3a,g16.7,a,i4,a,f10.2,a,f10.2,3a)') '<text x="' &
      & ,zx_label,'" y="' &
      & ,zy_label,'" ',trim(cgfonte_texte),' font-size="',ztaille &
      & ,'" text-anchor="left" transform="rotate(',ilaba,'  ,' &
      & ,zx_label,', ',zy_label,')" fill="black" >',trim(cllabel),'</text>'
      
      cltxt=cl_nettoie_blancs(cltxt)
      write(nulsvg,fmt='(a)') trim(cltxt)
    endif
  enddo
elseif(lgc2d) then ! lgpointage.
  !
  !-------------------------------------------------
  ! Pas de pointage, on remplit donc ici des rectangles de couleur, régulièrement
  ! répartis.
  !-------------------------------------------------
  !
  write(*,fmt=*) '  Ecriture de rectangles colorés...'
  iavaprec=0
  llava=ky > 800
  if(llava) write(*,fmt='(a,$)') '   '
  do jy=1,ky
    iava=nint(real(jy)/real(ky)*100.)
    if(iava /= iavaprec) then
      if(llava) write(*,fmt='(i3,a,$)') iava,'% '
      iavaprec=iava
    endif
    do jx=1,kx
      zval=pval(jx,jy)
      !zval=modulo(zval,2.) ! pour obtenir une palette périodique.
      !
      !-------------------------------------------------
      ! Couleur RVB associée à cette valeur réelle.
      !-------------------------------------------------
      !
      call couleur_relle(zval,pmin,pmax,pindef,cdpal,krvb_indef,irvb)
      !
      !-------------------------------------------------
      ! Taille du pavé.
      !-------------------------------------------------
      !
      zwidth_rect=rlxt/real(kx)
      zheight_rect=rlyt/real(ky)
      !
      !-------------------------------------------------
      ! Coordonnées du coin haut gauche du pavé 2D.
      !-------------------------------------------------
      !
      zxcoin=rxt+zwidth_rect*(jx-1)
      zycoin=ryt+zheight_rect*(ky-jy)
      !
      !-------------------------------------------------
      ! On met ce pixel coloré dans un tableau recevant l'image 2D.
      !-------------------------------------------------
      !
      do jc=1,3
        irvb_fic(jc,jx,ky-jy+1)=irvb(jc)
      enddo
    enddo
  enddo
  if(llava) write(*,fmt='(a)') ' '
  ilxt=nint(rlxt)
  ilyt=nint(rlyt)
  allocate(irvb_base64(3,ilxt,ilyt))
  !
  !-------------------------------------------------
  ! On échantillonne la grille (kx,ky) sur la grille-pixels de sortie (rlxt,rlyt).
  !-------------------------------------------------
  !
  write(*,fmt=*) '  La grille ',kx,' x ',ky,' est échantillonnée vers la grille-image de sortie ',ilxt,' x ',ilyt,'...'
  do jy=1,ilyt
    do jx=1,ilxt
      zfracx=(real(jx)-0.5)/real(ilxt)
      ix=max(1,min(kx,int(zfracx*kx)+1))
      zfracy=(real(jy)-0.5)/real(ilyt)
      iy=max(1,min(ky,int(zfracy*ky)+1))
      do jc=1,3
        irvb_base64(jc,jx,jy)=irvb_fic(jc,ix,iy)
      enddo
    enddo
  enddo
  !
  !-------------------------------------------------
  ! On écrit un fichier GIF contenant les pixels colorés.
  !-------------------------------------------------
  !
  clfgif=trim(cgfpix)//'.tmp.gif'
  call img_ecr(clfgif,ilxt,ilyt,irvb_base64)
  clbase64=trim(clfgif)//'.base64'
  !
  !-------------------------------------------------
  ! On convertit ce fichier GIF en base64.
  !-------------------------------------------------
  !
  write(clexe,fmt='(100a)') 'base64 ',trim(clfgif),' >  ',trim(clbase64)
  write(*,fmt=*) trim(clexe)
  ierr=system(clexe)
  if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
  !
  !-------------------------------------------------
  ! On supprime le GIF.
  !-------------------------------------------------
  !
  write(clexe,fmt='(100a)') 'rm ',trim(clfgif)
  write(*,fmt=*) trim(clexe)
  ierr=system(clexe)
  if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
  !
  !-------------------------------------------------
  ! Inclusion de ce fichier base64 dans le fichier vectoriel SVG.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') '<!-- Inclusion d''une image bitmap codée en base64. -->'!
  write(nulsvg,fmt='(9a)') '<g>'
  write(nulsvg,fmt='(9a)') '<image'
  write(clsvg,fmt='(a,f8.2,a)') '       y="',ryt,'"'
  clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
  write(clsvg,fmt='(a,f8.2,a)') '       x="',rxt,'"'
  clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
  write(nulsvg,fmt='(9a)') '       id="image3047"'
  write(nulsvg,fmt='(9a)') '       xlink:href="data:image/gif;base64,'
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  iule=78 ; open(iule,file=clbase64,form='formatted')
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  inomal=0
  do
    read(iule,fmt='(a)',iostat=ios) clc
    if(ios == -1) then
      !
      !-------------------------------------------------
      ! Fin de fichier.
      !-------------------------------------------------
      !
      exit
    elseif(ios == 0) then
      !
      !-------------------------------------------------
      ! Cas général.
      !-------------------------------------------------
      !
      inomal=inomal+1
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
      stop 'call abort'
    endif
    !
    !-------------------------------------------------
    ! Traitement de la ligne courante.
    !-------------------------------------------------
    !
    write(nulsvg,fmt='(9a)') trim(clc)
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(iule)
  !
  !-------------------------------------------------
  ! On supprime le fichier base64.
  !-------------------------------------------------
  !
  write(clexe,fmt='(100a)') 'rm ',trim(clbase64)
  write(*,fmt=*) trim(clexe)
  ierr=system(clexe)
  if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
  !
  !-------------------------------------------------
  ! Dimensions de la zone base64.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') '"'
  write(clsvg,fmt='(a,f8.2,a)') '       height="',rlyt,'"'
  clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
  write(clsvg,fmt='(a,f8.2,a)') '       width="',rlxt,'" />'
  clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
  write(nulsvg,fmt='(9a)') '  </g>'
endif ! lgpointage.
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
  if(zmin> 2436934.5 .and. zmax < 2524593.5) then
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
! Légende. On crée la légende de la palette colorée.
!-------------------------------------------------
!
write(*,fmt=*) '  Légende de la palette colorée...'
if(lgc2d .and. lgleg .and. cgimpf == ' ') call svg_legende(cdpal)
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
! Isolignage éventuel.
!-------------------------------------------------
!
if(nfic_isol > 0) then
  call svg_isol
endif
if(trim(cgformat) == 'HAS' .or. trim(cgformat) == 'HAA') then
  !
  !-------------------------------------------------
  ! Tracé de diagramme type Hayashi. Cotation des jours sur l'axe de droite.
  !-------------------------------------------------
  !
  call cote_jours
endif
if(cgimpf /= ' ') then
  
  !-------------------------------------------------
  ! Tracé d'impacts de foudre, par pointage un par un.
  !-------------------------------------------------
  call svg_impf
endif
if(ldclose) then
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
endif
end
