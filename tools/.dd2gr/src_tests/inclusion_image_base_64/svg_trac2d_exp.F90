subroutine svg_trac2d(pval,kx,ky,pmin,pmax,cdcoord,cdpal &
&,cdfdc,cdtitre,ldlegende,ldlegendexy,cdlegx,cdlegy,kximage &
&,kyimage,pindef,krvb_indef,cdficsvg)
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
! 	pval: tableau rectangulaire des réels à tracer.
!	kx,ky: dimensions de pval.
!	pmin,pmax: valeurs de pval à associer au minimum (resp. maximum) de la palette de couleurs de sortie.
!	cdcoord: coordonnées des axes X et Y:
!		cdcoord='non' si les coordonnées ne sont pas fournies,
!		cdcoord='1.5  28.  -78.   45.' si les coordonnées réelles
!			* du bas gauche du carré en bas à gauche sont X=1.5 Y=-78.
!			* du haut droite du carré en haut à droite sont X=28. Y=45.
!		en l'état actuel du logiciel ces coordonnées ne servent que si l'on superpose
!		un fond de carte (variable cdfdc).
!	cdpal: palette de couleurs demandée.
!	cdtitre: titre du graphique de sortie.
!	cdfdc: fond de carte à superposer à l'image:
!		cdfdc='non' si aucun fond de carte désiré,
!		sinon cdfdc='/home/piriou/ch/fdc/ref  255 0 0'
!		si on veut par exemple lire le fond de carte sur le fichier /home/piriou/ch/fdc/ref
!		et le tracer en rouge (triplet RVB 255 0 0).
!		Dans le fichier fond de carte le saut de plume est supposé donné
!		par une ligne sur laquelle sont écrits les deux réels 999.999 999.999.
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
real(kind=8), intent(in) :: pval(kx,ky)
character(len=*), intent(in) :: cdpal,cdtitre,cdficsvg,cdfdc,cdcoord,cdlegx,cdlegy
integer(kind=4), intent(in) :: krvb_indef(3)
character(len=180) clmot(40)
integer(kind=4), parameter :: jpcoul=15 ! nombre max. de couleurs possibles, et donc de courbes gérables.
character(len=15) :: clcourbes(jpcoul)
real(kind=8) zprinc(jplignes)
real(kind=8) zsec(jplignes)
integer(kind=4) :: irvb(3)
integer(kind=4) :: irvb_fic(3,kx,ky)
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
zcadre_width=1.18e-3*rlxsvg
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
! Largeur de l'espace à gauche pour le légendage de l'axe Y.
!-------------------------------------------------
!
rxt=0.09*rlxsvg
!
!-------------------------------------------------
! Largeur de de l'espace à droite pour le légendage des couleurs des
! courbes.
!-------------------------------------------------
!
rx_legcour=0.27*rlxsvg
!
!-------------------------------------------------
! Hauteur de l'espace du bas pour le légendage de l'axe X.
!-------------------------------------------------
!
ry_legx=0.09*rlysvg
!
!-------------------------------------------------
! Hauteur de la zone réservée au titre.
!-------------------------------------------------
!
ryt=0.15*rlysvg
!
!-------------------------------------------------
! Au sein de l'espace total, la zone de tracé des courbes est:
!-------------------------------------------------
!
rlxt=rlxsvg-rx_legcour-rxt
rlyt=rlysvg-ryt-ry_legx
!
!-------------------------------------------------
! Ecriture du titre.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.33*ryt)
ztaille_fonte=rlxsvg/55.

write(clsvg_tit,fmt='(a,i5,a,i5,a,g16.7,3a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" font-family="New Century Schoolbook" font-size="',ztaille_fonte &
& ,'" text-anchor="middle" stroke="black" >' &
& ,trim(cdtitre),'</text>'

clsvg_titre=cl_nettoie_blancs(clsvg_tit)
write(nulsvg,fmt='(9a)') '<!-- Ecriture du titre. -->'
write(nulsvg,fmt='(a)') trim(clsvg_titre)
!
!-------------------------------------------------
! Ecriture du sous-titre.
!-------------------------------------------------
!
ixtxt=nint(rlxsvg/2.)
iytxt=nint(0.67*ryt)
ztaille_fonte=rlxsvg/70.

zvaltmp=rcmin_reel ; call reecar(zvaltmp,-1,3,clmin,iltmp)
zvaltmp=rcmax_reel ; call reecar(zvaltmp,-1,3,clmax,iltmp)
zvaltmp=rcmoy ; call reecar(zvaltmp,-1,3,clmoy,iltmp)
zvaltmp=rcect ; call reecar(zvaltmp,-1,3,clect,iltmp)
zvaltmp=rceqm ; call reecar(zvaltmp,-1,3,cleqm,iltmp)
write(clstit,fmt='(100a)') 'Min=',trim(clmin),' Max=',trim(clmax) &
& ,' Moy=',trim(clmoy) &
& ,' Ect=',trim(clect) &
& ,' Eqm=',trim(cleqm)

write(clsvg_tit,fmt='(a,i5,a,i5,a,g16.7,3a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" font-family="New Century Schoolbook" font-size="',ztaille_fonte &
& ,'" text-anchor="middle" stroke="black" >' &
& ,trim(clstit),'</text>'

clsvg_titre=cl_nettoie_blancs(clsvg_tit)
write(nulsvg,fmt='(9a)') '<!-- Ecriture du sous-titre. -->'
write(nulsvg,fmt='(a)') trim(clsvg_titre)
!
!-------------------------------------------------
! Cadre intérieur: zone de tracé des courbes: ligne brisée fermée.
!-------------------------------------------------
!
zxsomme=rxt+rlxt
zysomme=ryt+rlyt
write(clc,fmt=*) '<path d="M ',rxt,',',ryt,' L ',rxt &
& ,',',zysomme,' L ',zxsomme,',',zysomme,' L ' &
& ,zxsomme,',',ryt,' Z" style="fill:none; stroke:' &
& ,'black','" stroke-width="',zcadre_width,'" />'
clc=cl_nettoie_blancs(clc)
write(nulsvg,fmt='(9a)') '<!-- Cadre intérieur: zone de tracé des courbes. -->'
write(nulsvg,fmt='(a)') trim(clc)

!
!-------------------------------------------------
! Test de cohérence des extrêmes.
!-------------------------------------------------
!
if(rxmin > rxmax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'svg_trac1D/ERREUR: le minimum de X doit être < au maximum!...'
  write(*,fmt=*) rxmin,rxmax
  stop 'call abort'
elseif(rxmin == rxmax) then
  rxmax=max(rxmin+0.001,rxmin*1.4)
endif
if(rymin > rymax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'svg_trac1D/ERREUR: le minimum de Y doit être < au maximum!...'
  write(*,fmt=*) rymin,rymax
  stop 'call abort'
elseif(rymin == rymax) then
  rymax=max(rymin+0.001,rymin*1.4)
endif
!
!-------------------------------------------------
! Tracé 2D proprement dit.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') '<!-- Tracé 2D proprement dit. -->'
call img_pal_init
do jy=1,ky
  do jx=1,kx
    zval=pval(jx,jy)
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
    ! Couleur du pavé.
    !-------------------------------------------------
    !
    if(zval /= pindef) then
      !
      !-------------------------------------------------
      ! Valeur non manquante.
      !-------------------------------------------------
      !
      if(trim(cdpal) == 'SPECIF') then
        !
        !-------------------------------------------------
        ! L'utilisateur a spécifié une palette irrégulière par paliers, 
        ! via le fichier ".doc", avec la syntaxe suivante::
        ! #PALETTE=SPECIF
        ! #PAL_SPECIF= <  1 255 0 0 
        ! #PAL_SPECIF= < 2 255 255 255 
        ! #PAL_SPECIF= < 10 0 255 0
        ! #PAL_SPECIF= > 10 0 0 0
        ! qui spécifie:
        !    - si la valeur du champ est < 1 la couleur est RVB="255 0 0"
        !    - si la valeur du champ est entre 1 et 2 la couleur est RVB="255 255 255"
        !    - si la valeur du champ est entre 2 et 10 la couleur est RVB="0 255 0"
        !    - si la valeur du champ est > 10 la couleur est RVB="0 0 0"
        !-------------------------------------------------
        !
        if(npalspec == 0) then
          write(*,fmt=*) 
          write(*,fmt=*) 'img_trac2D/ERREUR: palette spécifiée par l''utilisateur, or aucune spécification de plages de réels effectuée.'
          stop 'call abort'    
        endif
        lloktrav=.false.
        do jpalspec=1,npalspec
          call casc(cgpalspec(jpalspec),1,clmot,ilmot)
          read(clmot(2),fmt=*) zspec_seuil
          read(clmot(3),fmt=*) irspec
          read(clmot(4),fmt=*) ivspec
          read(clmot(5),fmt=*) ibspec
          if(trim(clmot(1)) == '<') then
            if(zval < zspec_seuil) then
              irvb(1)=irspec
              irvb(2)=ivspec
              irvb(3)=ibspec
              lloktrav=.true.
              exit
            endif
          elseif(trim(clmot(1)) == '>') then
            irvb(1)=irspec
            irvb(2)=ivspec
            irvb(3)=ibspec
            lloktrav=.true.
            exit
          else
            write(*,fmt=*) 
            write(*,fmt=*) 'img_trac2D/ERREUR: syntaxe de la spécification de palette:'
            write(*,fmt=*) trim(cgpalspec(jpalspec))
            write(*,fmt=*) 'on doit trouver un signe < ou > .'
            stop 'call abort'           
          endif
        enddo
        if(.not.lloktrav) then
          write(*,fmt=*) 
          write(*,fmt=*) 'img_trac2D/ERREUR: la dernière spécification de seuil #PAL_SPECIF doit commencer par le signe > !'
          write(*,fmt=*) zval,npalspec,trim(clmot(1)),irspec,ivspec,ibspec
          stop 'call abort'   
        endif 
      elseif(cdpal(1:4) /= 'AUTO') then
        !
        !-------------------------------------------------
        ! L'utilisateur ne veut pas une palette automatique.
        ! C'est qu'il a choisi une palette telle ARC-EN-CIEL,
        ! et qu'il la veut continue.
        !-------------------------------------------------
        !
        zfrac=(zval-pmin)/(pmax-pmin)
        call img_pal(cdpal,'CONT','FRAC',zfrac,irvb)
      else
        !
        !-------------------------------------------------
        ! L'utilisateur veut une palette automatique.
        ! C'est une palette qui sera différente suivant 
        ! que le champ réel embrasse 0 ou non,
        ! et une palette discrète.
        !-------------------------------------------------
        !
        call img_pal(cdpal,cltype,clabs,zval,irvb)
      endif
    else
      !
      !-------------------------------------------------
      ! Valeur manquante.
      !-------------------------------------------------
      !
      irvb(1)=krvb_indef(1)
      irvb(2)=krvb_indef(2)
      irvb(3)=krvb_indef(3)
    endif
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
!
!-------------------------------------------------
! On écrit un fichier GIF contenant les pixels colorés.
!-------------------------------------------------
!
clfgif=trim(cgfpix)//'.tmp.gif'
call img_ecr(clfgif,kx,ky,irvb_fic)
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
write(nulsvg,fmt='(9a)') '"'
write(clsvg,fmt='(a,f8.2,a)') '       height="',rlyt,'"'
clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
write(clsvg,fmt='(a,f8.2,a)') '       width="',rlxt,'" />'
clsvg=cl_nettoie_blancs(clsvg) ; write(nulsvg,fmt='(100a)') trim(clsvg)
write(nulsvg,fmt='(9a)') '  </g>'
!-------------------------------------------------
! Légendage des axes.
!-------------------------------------------------
!
write(*,fmt=*) ' ----------------------------------------------------'
write(*,fmt=*) 'Axe X'
write(*,fmt=*) ' '
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
  call lega(zmin,zmax,iprinc,zprinc,isec,zsec) ! axe des X.
  call tralps(iprinc,zprinc,isec,zsec,'X') ! tracé des lignes principales et secondaires de l'axe X.
endif

write(*,fmt=*) ' ----------------------------------------------------'
write(*,fmt=*) 'Axe Y'
write(*,fmt=*) ' '
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
  call lega(zmin,zmax,iprinc,zprinc,isec,zsec) ! axe des Y.
  call tralps(iprinc,zprinc,isec,zsec,'Y') ! tracé des lignes principales et secondaires de l'axe Y.
endif
!
!-------------------------------------------------
! Superposition du fond de carte.
!-------------------------------------------------
!
if(trim(cdfdc) /= 'non') then
  if(lgdebu) write(*,fmt=*) 'svg_trac2d: pré appel au fond de carte : cdfdc=',trim(cdfdc)
  write(nulsvg,fmt='(9a)') '<!-- Tracé du fond de carte. -->'
  call svg_fdc(cdfdc,kximage,kyimage,ibordblcg,ibordblch,ixpix,iypix)
endif
!
!-------------------------------------------------
! Légende. On crée la légende de la palette colorée.
!-------------------------------------------------
!
call svg_legende(cdpal)
!
!-------------------------------------------------
! Fermeture du fichier SVG.
!-------------------------------------------------
!
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
