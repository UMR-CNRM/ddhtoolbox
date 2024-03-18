subroutine img_trac1d(pindef,psaut,kbris,kcourbes,px,py,cdtexte,pxmin,pxmax,pymin,pymax &
& ,cdtitre,ldlegende,kpix_largeur,kximage,kyimage,cdficppm)
! --------------------------------------------------------------
! **** *trac1d* Ecriture d'un fichier image, contenant plusieurs courbes 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2013-11, J.M. Piriou.
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
!	pxmin: valeur minimale de X de TOUTES les kcourbes courbes.
!	pxmax: valeur maximale de X de TOUTES les kcourbes courbes.
!	pymin: valeur minimale de Y de TOUTES les kcourbes courbes.
!	pymax: valeur maximale de Y de TOUTES les kcourbes courbes.
!	cdtitre: titre global au graphique de sortie.
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
real(kind=8) px(kcourbes,kbris)
real(kind=8) py(kcourbes,kbris)
character*(*) cdtexte(kcourbes),cdtitre,cdficppm
integer(kind=4) :: irvb(3,kximage,kyimage)
integer(kind=4) :: irvb_trait(3)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: iloc(3)
integer(kind=4) :: ityps(4)
integer(kind=4), allocatable :: irvb_leg(:,:,:)
integer(kind=4), parameter :: jpcoul=18 ! nombre max. de couleurs possibles, et donc de courbes gérables.
integer(kind=4) :: irvb_courbes(3,jpcoul)
real(kind=8) :: zval_gradu(2) ! valeurs pour les graduations en X et Y.
real(kind=8) :: zxpix_graduation(2), zypix_graduation(2)
integer(kind=4) :: irvb_legendexy_pp(3)
integer(kind=4) :: irvb_axes(3)
integer(kind=4) :: irvb_legendexy_fond(3)
!
!-------------------------------------------------
! Initialisation des couleurs de courbes.
!-------------------------------------------------
!
ic=0
ic=ic+1 ; irvb_courbes(1,ic)=255 ; irvb_courbes(2,ic)=000 ; irvb_courbes(3,ic)=000 ! rouge.
ic=ic+1 ; irvb_courbes(1,ic)=059 ; irvb_courbes(2,ic)=185 ; irvb_courbes(3,ic)=255 ! bleu ciel sombre.
ic=ic+1 ; irvb_courbes(1,ic)=000 ; irvb_courbes(2,ic)=255 ; irvb_courbes(3,ic)=000 ! vert.
ic=ic+1 ; irvb_courbes(1,ic)=246 ; irvb_courbes(2,ic)=096 ; irvb_courbes(3,ic)=171 ! rose flashy.
ic=ic+1 ; irvb_courbes(1,ic)=000 ; irvb_courbes(2,ic)=255 ; irvb_courbes(3,ic)=255 ! cyan.

ic=ic+1 ; irvb_courbes(1,ic)=150 ; irvb_courbes(2,ic)=150 ; irvb_courbes(3,ic)=150 ! gris.
!ic=ic+1 ; irvb_courbes(1,ic)=000 ; irvb_courbes(2,ic)=000 ; irvb_courbes(3,ic)=000 ! noir.
ic=ic+1 ; irvb_courbes(1,ic)=152 ; irvb_courbes(2,ic)=005 ; irvb_courbes(3,ic)=023 ! marron.
ic=ic+1 ; irvb_courbes(1,ic)=173 ; irvb_courbes(2,ic)=169 ; irvb_courbes(3,ic)=110 ! kaki.
ic=ic+1 ; irvb_courbes(1,ic)=141 ; irvb_courbes(2,ic)=056 ; irvb_courbes(3,ic)=201 ! violet.

ic=ic+1 ; irvb_courbes(1,ic)=058 ; irvb_courbes(2,ic)=200 ; irvb_courbes(3,ic)=048 ! vert sombre.
ic=ic+1 ; irvb_courbes(1,ic)=235 ; irvb_courbes(2,ic)=213 ; irvb_courbes(3,ic)=188 ! beige clair.
ic=ic+1 ; irvb_courbes(1,ic)=201 ; irvb_courbes(2,ic)=235 ; irvb_courbes(3,ic)=136 ! vert caca d'oie.
ic=ic+1 ; irvb_courbes(1,ic)=126 ; irvb_courbes(2,ic)=182 ; irvb_courbes(3,ic)=165 ! bleu pétrole.
if(ic > jpcoul) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac1d/ERREUR: initialisation des couleurs!...'
  write(*,fmt=*) ic,jpcoul
  call exit(1)
endif
if(kcourbes > ic) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac1d/ATTENTION: plus de courbes demandées que de couleurs différentes initialisées!...'
  write(*,fmt=*) ic,kcourbes
endif
!
!-------------------------------------------------
! La dernière courbe est mise en noir, pour raison de lisibilité.
! Le noir est en effet très lisible, on souhaite qu'il soit systématiquement
! utilisé.
!-------------------------------------------------
!
irvb_courbes(1,kcourbes)=000 ; irvb_courbes(2,kcourbes)=000 ; irvb_courbes(3,kcourbes)=000 ! noir.
!
!-------------------------------------------------
! Initialisation à blanc.
!-------------------------------------------------
!
irvb=255
!
!-------------------------------------------------
! Taille de l'image de sortie affectée à la légende L des courbes: nxlegende et
! nylegende. Cette légende est calée en bas à D de l'image de sortie.
!
! Le titre est centré, et au-dessus de la légende L.
!
! La zone de tracé T est en bas à G de l'image de sortie.
! Elle a pour dimension (ixt, iyt) = ( nximage-nxlegende , nyimage-nylegende ), 
! en incluant le tracé des axes au bord.
!
! Le tracé des axes X et Y et leur légende se fait dans une zone de largeur
! ilarg_axes, tout autour à l'intérieur de T.
!
! Le tracé des courbes proprement dit se fait dans la zone restante,
! soit dans une zone C de dimension (ixc,iyc) = (ixt-ilarg_axes,iyt-ilarg_axes).
! Le point haut-G de la zone C a pour coordonnées (iposxc,iposyc) = (ilarg_axes+1,nyimage-nylegende+ilarg_axes).
!-------------------------------------------------
! 
ilarg_axes=63
ixt=nximage-nxlegende
iyt=nylegende
ixc=ixt-2*ilarg_axes
iyc=iyt-2*ilarg_axes
iposxc=ilarg_axes+1
iposyc=nyimage-nylegende+ilarg_axes
!
!-------------------------------------------------
! Tracé de lignes noires-bordures.
!-------------------------------------------------
!
irvb_legendexy_pp(:)=0
irvb_legendexy_fond(:)=255
ipix_largeur=1
!
! Trait haut.
!
ixdepart=1 ; iydepart=1
ixarrivee=kximage ; iyarrivee=1
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait bas.
!
ixdepart=1 ; iydepart=kyimage
ixarrivee=kximage ; iyarrivee=kyimage
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait G.
!
ixdepart=1 ; iydepart=1
ixarrivee=1 ; iyarrivee=kyimage
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait D.
!
ixdepart=kximage ; iydepart=1
ixarrivee=kximage ; iyarrivee=kyimage
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait séparant le titre (en haut) des zones de tracé et de légende.
!
ixdepart=1 ; iydepart=kyimage-nylegende+1
ixarrivee=kximage ; iyarrivee=iydepart
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait séparant la zone de tracé T de la légende L.
!
ixdepart=ixt+1 ; iydepart=kyimage-nylegende+1
ixarrivee=ixdepart ; iyarrivee=kyimage
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait séparant la zone de tracé C de la légende des Y.
!
ixdepart=ilarg_axes ; iydepart=kyimage-nylegende+ilarg_axes
ixarrivee=ixdepart ; iyarrivee=kyimage-ilarg_axes+1
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
!
! Trait séparant la zone de tracé C de la légende des X.
!
ixdepart=ilarg_axes ; iydepart=kyimage-ilarg_axes+1
ixarrivee=ixt-ilarg_axes ; iyarrivee=iydepart
call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
! 
!-------------------------------------------------
! Titre.
!-------------------------------------------------
!
irvb_fond_texte=191
zopac_fond_texte=0.
irvb_pp_texte=0
zopac_pp_texte=1.
ifonte=1
iloc(1)=10 ! coordonnées centrées.
iloc(2)=kximage/2
iloc(3)=(nyimage-nylegende)/2
call img_texte(cdtitre,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Test de cohérence des extrêmes.
!-------------------------------------------------
!
if(pxmin == pxmax) then
  pxmax=max(pxmin+0.001,pxmin*1.4)
endif
if(pymin == pymax) then
  pymax=max(pymin+0.001,pymin*1.4)
endif
!
!-------------------------------------------------
! On élargit un peu la fenêtre de tracé.
!-------------------------------------------------
!
zdiff=0.05*(pxmax-pxmin) ; pxmin=pxmin-zdiff ; pxmax=pxmax+zdiff
zdiff=0.05*(pymax-pymin) ; pymin=pymin-zdiff ; pymax=pymax+zdiff
!
!-------------------------------------------------
! Axes X=0 et Y=0.
!-------------------------------------------------
!
irvb_axes=180 ! axes X=0 et Y=0 en gris.
if(pxmin*pxmax < 0.) then
  !
  !-------------------------------------------------
  ! 0 est dans les données x.
  ! Tracé d'un axe vertical en 0.
  !-------------------------------------------------
  !
  ixdepart=iposxc+(0.-pxmin)/(pxmax-pxmin)*real(ixc) ; iydepart=iposyc
  ixarrivee=ixdepart ; iyarrivee=iydepart+iyc
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_axes,irvb,kximage,kyimage)
endif
if(pymin*pymax < 0.) then
  !
  !-------------------------------------------------
  ! 0 est dans les données y.
  ! Tracé d'un axe horizontal en 0.
  !-------------------------------------------------
  !
  ixdepart=iposxc ; iydepart=iposyc+(1.-(0.-pymin)/(pymax-pymin))*real(iyc)
  ixarrivee=iposxc+ixc ; iyarrivee=iydepart
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_axes,irvb,kximage,kyimage)
endif
!
!-------------------------------------------------
! Tracé des courbes.
!-------------------------------------------------
!
do jcourbes=1,kcourbes
  !
  !-------------------------------------------------
  ! Initialisation de la couleur de la courbe.
  !-------------------------------------------------
  !
  do jc=1,3
    irvb_trait(jc)=irvb_courbes(jc,modulo(jcourbes-1,ic)+1)
  enddo
  do jbris=1,kbris-1
    !
    !-------------------------------------------------
    ! Saut si donnée manquante ou saut imposé.
    !-------------------------------------------------
    !
    if(px(jcourbes,jbris) == pindef .or. abs((px(jcourbes,jbris)-psaut)/psaut) < 1.e-7) cycle
    if(py(jcourbes,jbris) == pindef .or. abs((py(jcourbes,jbris)-psaut)/psaut) < 1.e-7) cycle
    !
    !-------------------------------------------------
    ! Coordonnées du pixel associé au jbris ième point de la ligne brisée.
    !-------------------------------------------------
    !
    itabx0=int((px(jcourbes,jbris)-pxmin)/(pxmax-pxmin)*real(ixc))
    itaby0=int((1.-(py(jcourbes,jbris)-pymin)/(pymax-pymin))*real(iyc))
    if(itabx0 > ixc .or. itabx0 < 1) cycle
    if(itaby0 > iyc .or. itaby0 < 1) cycle
    itabx0=itabx0+iposxc
    itaby0=itaby0+iposyc
    !
    !-------------------------------------------------
    ! Coordonnées du pixel associé au (jbris+1) ième point de la ligne brisée.
    !-------------------------------------------------
    !
    itabx1=int((px(jcourbes,jbris+1)-pxmin)/(pxmax-pxmin)*real(ixc))
    itaby1=int((1.-(py(jcourbes,jbris+1)-pymin)/(pymax-pymin))*real(iyc))
    if(itabx1 > ixc .or. itabx1 < 1) cycle
    if(itaby1 > iyc .or. itaby1 < 1) cycle
    itabx1=itabx1+iposxc
    itaby1=itaby1+iposyc
    !
    !-------------------------------------------------
    ! Tracé d'un trait de largeur kpix_largeur pixels reliant ces points successifs.
    !-------------------------------------------------
    !
    call img_trait(itabx0,itaby0,itabx1,itaby1,kpix_largeur,irvb_trait,irvb,kximage,kyimage)
    !
    !-------------------------------------------------
    ! Tracé d'une croix centrée sur le point courant.
    !-------------------------------------------------
    !
    iarete=0 ! arête de la croix: 5 pour activer le tracé de la croix, 0 si pas de tracé de la croix.
    do jarete=-iarete,iarete
      do jc=1,3 ! 3 couleurs RVB.
        irvb(jc,itabx0+jarete,itaby0)=irvb_trait(jc)
        irvb(jc,itabx0,itaby0+jarete)=irvb_trait(jc)
        irvb(jc,itabx1+jarete,itaby1)=irvb_trait(jc)
        irvb(jc,itabx1,itaby1+jarete)=irvb_trait(jc)
      enddo
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Légende de la courbe courante.
  ! Tracé du trait pour donner la couleur de la courbe.
  !-------------------------------------------------
  !
  itabx0=ixt+30
  itaby0=kyimage-nylegende+50+(jcourbes-1)*40
  itabx1=itabx0+35 
  itaby1=itaby0
  call img_trait(itabx0,itaby0,itabx1,itaby1,kpix_largeur,irvb_trait,irvb,kximage,kyimage)
  !
  !-------------------------------------------------
  ! Ecriture du nom en clair associé à la courbe.
  !-------------------------------------------------
  !
  irvb_fond_texte=191
  zopac_fond_texte=0.
  irvb_pp_texte=0
  zopac_pp_texte=1.
  ifonte=1
  iloc(1)=10 ! coordonnées centrées.
  iloc(2)=ixt+nxlegende/2
  iloc(3)=itaby0-10
  call img_texte(cgchamp(jcourbes),irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
enddo
!
!-------------------------------------------------
! Graduation X.
!-------------------------------------------------
!
ival=2
call img_echlog(pxmax,ilogx)
zval_gradu(1)=pxmin*10.**real(ilogx) ; zval_gradu(2)=pxmax*10.**real(ilogx)
zxpix_graduation(1)=real(ilarg_axes+1)
zxpix_graduation(2)=real(ilarg_axes+ixc)
zypix_graduation(1)=real(kyimage-nylegende+ilarg_axes+iyc+ilarg_axes/2)
zypix_graduation(2)=zypix_graduation(1)
zopac_fond=0.
zopac_pp=1.
ifonte=1
call img_graduation(zval_gradu,ival,zxpix_graduation,zypix_graduation,irvb_legendexy_fond,zopac_fond &
  &,irvb_legendexy_pp,zopac_pp,1,kximage,kyimage,irvb)
if(ilogx /= 0) then
  !
  !-------------------------------------------------
  ! On ajoute une légende d'échelle du type "*10**-8".
  !-------------------------------------------------
  !
  write(clech,fmt=*) '*10**',ilogx
  !
  !-------------------------------------------------
  ! On enlève les blancs de cette chaîne.
  !-------------------------------------------------
  !
  clechx=' ' ; ic2=0
  do jc=1,len_trim(clech)
    if(clech(jc:jc) /= ' ') then
      ic2=ic2+1
      clechx(ic2:ic2)=clech(jc:jc)
    endif
  enddo
endif
!
!-------------------------------------------------
! Graduation Y.
!-------------------------------------------------
!
ival=2
call img_echlog(pymax,ilogy)
zval_gradu(1)=pymin*10.**real(ilogy) ; zval_gradu(2)=pymax*10.**real(ilogy)
zxpix_graduation(1)=real(ilarg_axes/2)
zxpix_graduation(2)=zxpix_graduation(1)
zypix_graduation(1)=real(nyimage-nylegende+ilarg_axes+iyc)
zypix_graduation(2)=real(nyimage-nylegende+ilarg_axes)
zopac_fond=0.
zopac_pp=1.
ifonte=1
call img_graduation(zval_gradu,ival,zxpix_graduation,zypix_graduation,irvb_legendexy_fond,zopac_fond &
  &,irvb_legendexy_pp,zopac_pp,1,kximage,kyimage,irvb)
if(ilogy /= 0) then
  !
  !-------------------------------------------------
  ! On ajoute une légende d'échelle du type "*10**-8".
  !-------------------------------------------------
  !
  write(clech,fmt=*) '*10**',ilogy
  !
  !-------------------------------------------------
  ! On enlève les blancs de cette chaîne.
  !-------------------------------------------------
  !
  clechy=' ' ; ic2=0
  do jc=1,len_trim(clech)
    if(clech(jc:jc) /= ' ') then
      ic2=ic2+1
      clechy(ic2:ic2)=clech(jc:jc)
    endif
  enddo
endif
! 
!-------------------------------------------------
! Légende X.
!-------------------------------------------------
!
irvb_fond_texte=191
zopac_fond_texte=0.
irvb_pp_texte=0
zopac_pp_texte=1.
ifonte=1
iloc(1)=10 ! coordonnées centrées.
iloc(2)=ilarg_axes+ixc
iloc(3)=kyimage-15
if(ilogx /= 0) then
  cllegx=trim(cglegx)//' '//trim(clechx)
else
  cllegx=cglegx
endif
call img_texte(cllegx,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
! 
!-------------------------------------------------
! Légende Y.
!-------------------------------------------------
!
irvb_fond_texte=191
zopac_fond_texte=0.
irvb_pp_texte=0
zopac_pp_texte=1.
ifonte=1
iloc(1)=10 ! coordonnées centrées.
iloc(2)=ilarg_axes
iloc(3)=kyimage-nylegende+ilarg_axes/2
if(ilogy /= 0) then
  cllegy=trim(cglegy)//' '//trim(clechy)
else
  cllegy=cglegy
endif
call img_texte(cllegy,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Ecriture du fichier-image.
!-------------------------------------------------
!
call img_ecr(cdficppm,kximage,kyimage,irvb)
end
