subroutine img_demo_trac1d
! --------------------------------------------------------------
! **** *img_demo_trac1d* Demonstration de tracé multicourbes.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Tableaux allouables recevant X, Y et le texte explicatif de chaque courbe.
!-------------------------------------------------
!
real(kind=8), allocatable :: zx(:,:),zy(:,:)
character*50, allocatable :: cltexte(:)
!
!-------------------------------------------------
! Allocation des tableaux.
!-------------------------------------------------
!
icourbes=2 ! nombre de courbes.
ibris=60 ! nombre maximal de points par courbe.
allocate(zx(icourbes,ibris))
allocate(zy(icourbes,ibris))
allocate(cltexte(icourbes))
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
zindef=8.5213e12
zsaut=999.999
zx=zindef
zy=zindef
!
!-------------------------------------------------
! Initialisation de la courbe 1.
!-------------------------------------------------
!
ic=1
cltexte(ic)='ellipse avec rotation'
zpi=4.*atan(1.)
do jbris=1,ibris
  zfrac=real(jbris-1)/real(ibris-1)
  zx(ic,jbris)=0.5*cos(2.*zpi*zfrac+0.3)
  zy(ic,jbris)=sin(2.*zpi*zfrac+0.3)
enddo
!
!-------------------------------------------------
! Initialisation de la courbe 2.
!-------------------------------------------------
!
ic=2
cltexte(ic)='spirale'
zpi=4.*atan(1.)
do jbris=1,ibris
  zfrac=real(jbris-1)/real(ibris-1)
  zr=2.*zfrac
  ztheta=2.*zpi*3.*zfrac
  zx(ic,jbris)=zr*cos(ztheta)
  zy(ic,jbris)=zr*sin(ztheta)
enddo
!
!-------------------------------------------------
! Paramètres graphiques.
!-------------------------------------------------
!
!
! Min et max des axes X et Y.
!
zxmin=-1. ; zxmax=1.
zymin=-1. ; zymax=2.
!
! Titre global.
!
cltitre='Ellipses'
lllegende=.true.
!
! Epaisseur de trait des courbes en pixels.
!
ipix_largeur=3
!
! Taille de l'image de sortie.
!
iximage=300 ; iyimage=600
!
! Nom de l'image de sortie.
!
clficppm='trac1d.gif'
!
! Appel au tracé.
!
call img_trac1d(zindef,zsaut,ibris,icourbes,zx,zy,cltexte,zxmin,zxmax,zymin,zymax &
& ,cltitre,lllegende,ipix_largeur,iximage,iyimage,clficppm)
end
subroutine img_demo
! --------------------------------------------------------------
! **** *img_demo* Demonstration d'usage d'images RVB.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!call img_surimp_et_texte
call img_palettes
end
subroutine img_palettes
! --------------------------------------------------------------
! **** Demonstration des img_palettes de img_pal.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
integer(kind=4), parameter :: jpx=100
integer(kind=4), parameter :: jpy=600
real(kind=8) :: zfx,zfy,ztempe,zmin,zmax,ztab(jpx,jpy)
integer(kind=4) :: irvb(3,jpx,jpy),jx,jy,ipixx,ipixy,inbprint
integer(kind=4), allocatable :: irvb_leg(:,:,:)
integer(kind=4), parameter :: jppal=16
character*200 clpal(jppal)
!
!-------------------------------------------------
! Initialisation du tableau de réels.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zfracx=real(jx-1)/real(jpx-1)
    zfracy=real(jy-1)/real(jpy-1)
    ztab(jx,jy)=zfracx**2+zfracy**2
  enddo
enddo
zmin=0.
zmax=1.
clpal(1)='ARC-EN-CIEL'
cltitre='z=x*y^2'
lllegende=.true.
iximage=2*jpx
iyimage=jpy
clficppm='tmp.trac2D.ppm'
call img_trac2d(ztab,jpx,jpy,zmin,zmax,clpal(1),cltitre,lllegende,iximage,iyimage,clficppm)
!
!-------------------------------------------------
! Palettes continues.
!-------------------------------------------------
!
call img_pal_init
ipal=0
ipal=ipal+1 ; clpal(ipal)='REF'
ipal=ipal+1 ; clpal(ipal)='AQUABLUE'
ipal=ipal+1 ; clpal(ipal)='VEG'
ipal=ipal+1 ; clpal(ipal)='SAFO_TMER'
ipal=ipal+1 ; clpal(ipal)='TS'
ipal=ipal+1 ; clpal(ipal)='OROG'
ipal=ipal+1 ; clpal(ipal)='CONTRASTE'
ipal=ipal+1 ; clpal(ipal)='ARC-EN-CIEL'
ipal=ipal+1 ; clpal(ipal)='NOIR-BLANC'
ipal=ipal+1 ; clpal(ipal)='TEST'
ipal=ipal+1 ; clpal(ipal)='BLEU-BLANC-ROUGE'
ipal=ipal+1 ; clpal(ipal)='NOIR-BLEU-ROUGE-VERT-BLANC'
ipal=ipal+1 ; clpal(ipal)='VIOLET-BLANC'
ipal=ipal+1 ; clpal(ipal)='BLANC-NOIR'
ipal=ipal+1 ; clpal(ipal)='JAUNE-ROUGE-ROSE'
do jpal=1,ipal
  do jy=1,jpy
    do jx=1,jpx
      zfx=real(jx-1)/real(jpx-1)
      zfy=real(jy-1)/real(jpy-1)
      call img_pal(clpal(jpal),'CONT','FRAC',zfy,irvb(1,jx,jy))
    enddo
  enddo
  write(clfic,fmt='(9a)') 'palette.cont.',trim(clpal(jpal)),'.ppm'
  call img_ecr(clfic,jpx,jpy,irvb)
enddo
!
!-------------------------------------------------
! Palette par paliers.
!-------------------------------------------------
!
do jy=1,jpy
  do jx=1,jpx
    zfx=real(jx-1)/real(jpx-1)
    zfy=real(jy-1)/real(jpy-1)
    call img_pal('AQUABLUE','PAL','FRAC',zfy,irvb(1,jx,jy))
  enddo
enddo
call img_ecr('tmp.img_demo.img_palettes_pal_aquablue.ppm',jpx,jpy,irvb)
!
!-------------------------------------------------
! Palette de température par paliers.
!-------------------------------------------------
!
do jy=1,jpy
  do jx=1,jpx
    zfx=real(jx-1)/real(jpx-1)
    zfy=real(jy-1)/real(jpy-1)
    ztempe=-50.+(40.+50.)*zfy
    ztempe=ztempe+273.16
    call img_pal('SAFO_TMER','PAL','ABS',ztempe,irvb(1,jx,jy))
  enddo
enddo
call img_ecr('tmp.img_demo.safo_tmer.ppm',jpx,jpy,irvb)
!
!-------------------------------------------------
! Légende. On crée la légende d'un champ entre zxmin et zxmax,
! avec la palette clpal, légende dont la taille pixels est (ipixx,ipixy).
!-------------------------------------------------
!
ipixx=600
ipixy=100
allocate(irvb_leg(3,ipixx,ipixy))
zmin=-363.
zmax=700.
inbprint=0
clpal(1)='VIOLET-BLANC'
call img_legende(zmin,zmax,inbprint,clpal(1),ipixx,ipixy,irvb_leg)
call img_ecr('tmp.legende.ppm',ipixx,ipixy,irvb_leg)
end
subroutine img_surimp_et_texte
! --------------------------------------------------------------
! **** *rvb* Demonstration d'usage d'images RVB.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
integer(kind=4), parameter :: jpx=600
integer(kind=4), parameter :: jpy=300
integer(kind=4) :: irvb(3,jpx,jpy),jx,jy,iloc(3),ityps(4)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
character*200 clfic,clfic_ppm,cltexte
integer(kind=4),allocatable :: irvb_pp(:,:,:)
integer(kind=4) :: inx_pp,iny_pp,ifonte
real(kind=8) :: zopac,zopac_fond_texte,zopac_pp_texte
!
!
!-------------------------------------------------
! Initialisation de l'image de fond IFOND.
!-------------------------------------------------
!
do jy=1,jpy
  do jx=1,jpx
    irvb(1,jx,jy)=0
    irvb(2,jx,jy)=int(real(jx-1)/real(jpx-1)*255.)
    irvb(3,jx,jy)=int(real(jy-1)/real(jpy-1)*255.)
  enddo
enddo
call img_ecr('tmp.img_demo.init.ppm',jpx,jpy,irvb)
!
!-------------------------------------------------
! Lecture de l'image de premier plan IPP à surimposer.
!-------------------------------------------------
!
clfic_ppm='hsv.ppm'
call img_taille(clfic_ppm,inx_pp,iny_pp)
allocate(irvb_pp(3,inx_pp,iny_pp))
call img_lec(clfic_ppm,inx_pp,iny_pp,irvb_pp)
call img_ecr('gol.ppm',inx_pp,iny_pp,irvb_pp)
stop
!
!-------------------------------------------------
! Passage d'une image aux data ftn associées.
!-------------------------------------------------
!
!-------------------------------------------------
! Superposition de IPP sur IFOND, toute l'image
! étant opaque.
!-------------------------------------------------
!
zopac=1.
iloc(1)=3 ! haut G
ityps(1)=0 ! même opacité pour toutes les couleurs de IPP.
call img_surimpose_image(inx_pp,iny_pp,irvb_pp,zopac,ityps,iloc,jpx,jpy,irvb)
!
!-------------------------------------------------
! Superposition de IPP sur IFOND, le fond de IPP étant une img_transparence,
! et le reste étant opaque.
!-------------------------------------------------
!
zopac=0.5
iloc(1)=4 ! haut D
ityps(1)=2 ;ityps(2)=252 ; ityps(3)=254 ; ityps(4)=252
call img_surimpose_image(inx_pp,iny_pp,irvb_pp,zopac,ityps,iloc,jpx,jpy,irvb)
!
!-------------------------------------------------
! Ecriture de texte.
!-------------------------------------------------
!
cltexte='2>3?!BASE 1999_08_27 00h + 0h, BÂCHE bêêêê 00h'
!
! Couleur et opacité du fond de texte.
!
irvb_fond_texte(1)=255 ; irvb_fond_texte(2)=255 ; irvb_fond_texte(3)=255
zopac_fond_texte=0.5
!
! Couleur et opacité du pp de texte.
!
irvb_pp_texte(1)=000 ; irvb_pp_texte(2)=000 ; irvb_pp_texte(3)=000
zopac_pp_texte=1.
!
! Fonte du texte.
!
ifonte=1
!
! Localisation du texte.
!
iloc(1)=1 ! bas G
!
! Surimposition de ce texte sur l'image de base irvb.
!
call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,jpx,jpy,irvb)
!
!-------------------------------------------------
! Ecriture d'un fichier ppm ascii de l'image résultante.
!-------------------------------------------------
!
call img_ecr('tmp.img_demo.final.ppm',jpx,jpy,irvb)
end
subroutine img_surimpose_image(knx_pp,kny_pp,krvb_pp,popac,ktyps,kloc,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_surimpose_image* Imposition d'une image sur une autre, en transparence.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! knx_pp, kny_pp: nombre de pixels en X et Y de l'image premier plan (IPP).
! krvb(3,knx_pp,kny_pp): triplets RVB de IPP.
! popac: opacité du premier plan: 0. vitre parfaite, 1. écran parfait.
! ktyps(4):
!    - si ktyps(1)=0, alors l'opacité est la même pour toutes les couleurs de IPP.
!    - si ktyps(1)=1, alors l'opacité est de popac pour toutes les couleurs de IPP
!      sauf celle donnée en RVB par ktyps(2),ktyps(3),ktyps(4), qui a une opacité de 0..
!      i.e. LE FOND DE IPP EST IGNORE.
!    - si ktyps(1)=2, alors l'opacité est de 1. pour toutes les couleurs de IPP
!      sauf celle donnée en RVB par ktyps(2),ktyps(3),ktyps(4), qui a une opacité de popac.
!      i.e. LE FOND DE IPP EST transparent, TANDIS QUE TOUTES LES AUTRES SONT OPAQUES.
! kloc(3): position de IPP dans l'image de fond (IFOND):
!    - si kloc(1)=0, alors kloc(2)=position X du coin haut gauche de IPP dans IFOND.
!                          kloc(3)=         Y
!    - si kloc(1)=1, IPP est en bas  à gauche de IFOND.
!    - si kloc(1)=2, IPP est en bas  à droite de IFOND.
!    - si kloc(1)=3, IPP est en haut à gauche de IFOND.
!    - si kloc(1)=4, IPP est en haut à droite de IFOND.
!    - si kloc(1)=10, alors kloc(2)=position X du centre de IPP dans IFOND.
!                           kloc(3)=         Y
! kx, ky: nombre de pixels en X et Y de IFOND.
! En entrée/sortie:
! krvb(3,kx,ky): triplets RVB de IFOND.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
integer(kind=4) :: knx_pp,kny_pp,krvb_pp(3,knx_pp,kny_pp),kloc(3),kx,ky,krvb(3,kx,ky),iloc
integer(kind=4) :: ktyps(4)
integer(kind=4) :: iprop,iloc_x,iloc_y,jx,jy,ipp(3),ifond(3),jcoul
real(kind=8) :: popac,zopac
!
!-------------------------------------------------
! Contrôle de faisabilité de la surimposition.
!-------------------------------------------------
!
if(knx_pp > kx.or.kny_pp > ky) then
  print*,'img_surimpose_image/ATTENTION: l''image à surimposer est plus grande que la cible!...'
  if(knx_pp > kx) then
    print*,'	Image à surimposer: ',knx_pp,' pixels en X, or il y en a ',kx,' sur l''image fond.'
  endif
  if(kny_pp > ky) then
    print*,'	Image à surimposer: ',kny_pp,' pixels en Y, or il y en a ',ky,' sur l''image fond.'
  endif
  print*,'	Surimposition annulée.'
  return
endif
!
!-------------------------------------------------
! Localisation du coin haut gauche de IPP dans IFOND.
!-------------------------------------------------
!
iprop=96
if(kloc(1) == 0) then
  iloc_x=kloc(2)
  iloc_y=kloc(3)
elseif(kloc(1) == 1) then
  iloc_x=kx/iprop
  iloc_y=ky-kny_pp-kx/iprop
elseif(kloc(1) == 2) then
  iloc_x=kx-knx_pp-kx/iprop
  iloc_y=ky-kny_pp-kx/iprop
elseif(kloc(1) == 3) then
  iloc_x=kx/iprop
  iloc_y=kx/iprop
elseif(kloc(1) == 4) then
  iloc_x=kx-knx_pp-kx/iprop
  iloc_y=kx/iprop
elseif(kloc(1) == 10) then
  iloc_x=kloc(2)-knx_pp/2
  iloc_y=kloc(3)-kny_pp/2
else
  print*,'img_surimpose_image/ERREUR: option kloc(1) inconnue: ',kloc(1)
  call exit(1)
endif
!
!-------------------------------------------------
! Si les coordonnées calculées ont été trop optimistes,
! recalage de IPP au bord de IFOND.
!-------------------------------------------------
!
if(iloc_x < 1) then
  iloc_x=1
elseif(iloc_x+knx_pp-1 > kx) then
  iloc_x=kx-knx_pp+1
endif
if(iloc_y < 1) then
  iloc_y=1
elseif(iloc_y+kny_pp-1 > ky) then
  iloc_y=ky-kny_pp+1
endif
!
!-------------------------------------------------
! Modification du fond par ajout de la img_transparence.
!-------------------------------------------------
!
do jx=1,knx_pp
  do jy=1,kny_pp
    do jcoul=1,3
      ipp(jcoul)=krvb_pp(jcoul,jx,jy)
      ifond(jcoul)=krvb(jcoul,jx+iloc_x-1,jy+iloc_y-1)
    enddo
    if(ktyps(1) == 0) then
      !
      ! -------------------------------------------------
      ! La img_transparence est la même partout.
      ! -------------------------------------------------
      !
      zopac=popac
    else
      !
      ! -------------------------------------------------
      ! Transparence différente pour une couleur donnée de IPP.
      ! -------------------------------------------------
      !
      if(ipp(1) /= ktyps(2).or.ipp(2) /= ktyps(3).or.ipp(3) /= ktyps(4)) then
        !
        ! -------------------------------------------------
        ! Le pixel de IPP n'est pas de la couleur remarquable.
        ! -------------------------------------------------
        !
        if(ktyps(1) == 1) then
          zopac=popac
        elseif(ktyps(1) == 2) then
          zopac= 1.
        else
          print*,'img_surimpose_image/ERREUR: ktyps(1)=',ktyps(1)
          call exit(1)
        endif
      else
        !
        ! -------------------------------------------------
        ! Le pixel de IPP est de la couleur remarquable.
        ! -------------------------------------------------
        !
        if(ktyps(1) == 1) then
          zopac= 0.
        elseif(ktyps(1) == 2) then
          zopac=popac
        else
          print*,'img_surimpose_image/ERREUR: ktyps(1)=',ktyps(1)
          call exit(1)
        endif
      endif
    endif
    call img_transparence(ifond,ipp,zopac,ifond)
    do jcoul=1,3
      krvb(jcoul,jx+iloc_x-1,jy+iloc_y-1)=ifond(jcoul)
    enddo
  enddo
enddo
end
module imgyom
! --------------------------------------------------------------
! Module des palettes de couleur.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
save
!
integer(kind=4), parameter :: jppal=16 ! nombre de palettes.
integer(kind=4), parameter :: jpcoul=80 !nombre max. de couleurs par palette.
integer(kind=4) :: ncoul(jppal) ! nombre de couleurs de chaque palette.
real(kind=8) :: rvbpal(3,jpcoul,jppal) ! valeurs RVB de chaque couleur.
!
end module
subroutine img_pal_init
! --------------------------------------------------------------
! Palette RVB continue.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
use imgyom
do jpal=1,jppal
  if(jpal == 1) then
    !
    ! -------------------------------------------------
    ! Palette REF.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=042 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=094 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=145 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=196 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=247 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=212
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=109
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=007
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=147 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=210 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=159 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=056 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=005 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=097
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=149
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
  elseif(jpal == 2) then
    !
    ! -------------------------------------------------
    ! Palette AQUABLUE.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=150
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=084 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=223
  elseif(jpal == 3) then
    !
    ! -------------------------------------------------
    ! Palette VEG.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=189 ; rvbpal(2,ncoul(jpal),jpal)=122 ; rvbpal(3,ncoul(jpal),jpal)=019
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=076 ; rvbpal(2,ncoul(jpal),jpal)=140 ; rvbpal(3,ncoul(jpal),jpal)=074
  elseif(jpal == 4) then
    !
    ! -------------------------------------------------
    ! Palette SAFO_TMER.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=020 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=134
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=193
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=024 ; rvbpal(3,ncoul(jpal),jpal)=236
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=106 ; rvbpal(3,ncoul(jpal),jpal)=170
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=112 ; rvbpal(3,ncoul(jpal),jpal)=017
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=149 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=184 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=205 ; rvbpal(2,ncoul(jpal),jpal)=144 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=145 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=188 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 5) then
    !
    ! -------------------------------------------------
    ! Palette TS.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=132 ; rvbpal(2,ncoul(jpal),jpal)=045 ; rvbpal(3,ncoul(jpal),jpal)=206
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=144 ; rvbpal(2,ncoul(jpal),jpal)=066 ; rvbpal(3,ncoul(jpal),jpal)=211
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=157 ; rvbpal(2,ncoul(jpal),jpal)=087 ; rvbpal(3,ncoul(jpal),jpal)=216
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=169 ; rvbpal(2,ncoul(jpal),jpal)=108 ; rvbpal(3,ncoul(jpal),jpal)=221
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=181 ; rvbpal(2,ncoul(jpal),jpal)=129 ; rvbpal(3,ncoul(jpal),jpal)=226
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=194 ; rvbpal(2,ncoul(jpal),jpal)=150 ; rvbpal(3,ncoul(jpal),jpal)=231
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=206 ; rvbpal(2,ncoul(jpal),jpal)=171 ; rvbpal(3,ncoul(jpal),jpal)=235
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=218 ; rvbpal(2,ncoul(jpal),jpal)=192 ; rvbpal(3,ncoul(jpal),jpal)=240
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=213 ; rvbpal(3,ncoul(jpal),jpal)=245
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=243 ; rvbpal(2,ncoul(jpal),jpal)=234 ; rvbpal(3,ncoul(jpal),jpal)=250
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=129 ; rvbpal(2,ncoul(jpal),jpal)=005 ; rvbpal(3,ncoul(jpal),jpal)=065
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=142 ; rvbpal(2,ncoul(jpal),jpal)=030 ; rvbpal(3,ncoul(jpal),jpal)=084
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=154 ; rvbpal(2,ncoul(jpal),jpal)=055 ; rvbpal(3,ncoul(jpal),jpal)=103
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=167 ; rvbpal(2,ncoul(jpal),jpal)=080 ; rvbpal(3,ncoul(jpal),jpal)=122
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=105 ; rvbpal(3,ncoul(jpal),jpal)=141
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=192 ; rvbpal(2,ncoul(jpal),jpal)=130 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=205 ; rvbpal(2,ncoul(jpal),jpal)=155 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=217 ; rvbpal(2,ncoul(jpal),jpal)=180 ; rvbpal(3,ncoul(jpal),jpal)=198
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=205 ; rvbpal(3,ncoul(jpal),jpal)=217
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=242 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=236
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=026 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=026
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=051 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=051
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=077 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=077
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=102 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=128 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=128
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=153 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=153
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=204 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=204
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=230
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=026 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=051 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=077 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=102 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=128 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=153 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=204 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=230 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=065 ; rvbpal(2,ncoul(jpal),jpal)=163 ; rvbpal(3,ncoul(jpal),jpal)=023
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=103 ; rvbpal(2,ncoul(jpal),jpal)=181 ; rvbpal(3,ncoul(jpal),jpal)=069
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=122 ; rvbpal(2,ncoul(jpal),jpal)=191 ; rvbpal(3,ncoul(jpal),jpal)=093
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=141 ; rvbpal(2,ncoul(jpal),jpal)=200 ; rvbpal(3,ncoul(jpal),jpal)=116
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=160 ; rvbpal(2,ncoul(jpal),jpal)=209 ; rvbpal(3,ncoul(jpal),jpal)=139
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=179 ; rvbpal(2,ncoul(jpal),jpal)=218 ; rvbpal(3,ncoul(jpal),jpal)=162
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=227 ; rvbpal(3,ncoul(jpal),jpal)=185
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=217 ; rvbpal(2,ncoul(jpal),jpal)=237 ; rvbpal(3,ncoul(jpal),jpal)=209
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=236 ; rvbpal(2,ncoul(jpal),jpal)=246 ; rvbpal(3,ncoul(jpal),jpal)=232
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=026 ; rvbpal(3,ncoul(jpal),jpal)=026
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=051 ; rvbpal(3,ncoul(jpal),jpal)=051
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=077 ; rvbpal(3,ncoul(jpal),jpal)=077
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=102 ; rvbpal(3,ncoul(jpal),jpal)=102
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=128
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=153
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=179
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=204
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=230
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=248 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=023
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=141 ; rvbpal(3,ncoul(jpal),jpal)=046
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=153 ; rvbpal(3,ncoul(jpal),jpal)=069
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=250 ; rvbpal(2,ncoul(jpal),jpal)=166 ; rvbpal(3,ncoul(jpal),jpal)=093
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=251 ; rvbpal(2,ncoul(jpal),jpal)=179 ; rvbpal(3,ncoul(jpal),jpal)=116
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=192 ; rvbpal(3,ncoul(jpal),jpal)=139
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=252 ; rvbpal(2,ncoul(jpal),jpal)=204 ; rvbpal(3,ncoul(jpal),jpal)=162
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=253 ; rvbpal(2,ncoul(jpal),jpal)=217 ; rvbpal(3,ncoul(jpal),jpal)=185
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=254 ; rvbpal(2,ncoul(jpal),jpal)=230 ; rvbpal(3,ncoul(jpal),jpal)=209
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=254 ; rvbpal(2,ncoul(jpal),jpal)=242 ; rvbpal(3,ncoul(jpal),jpal)=232
  elseif(jpal == 6) then
    !
    ! -------------------------------------------------
    ! Palette orographique.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=156 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=033 ; rvbpal(2,ncoul(jpal),jpal)=189 ; rvbpal(3,ncoul(jpal),jpal)=024
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=148 ; rvbpal(2,ncoul(jpal),jpal)=165 ; rvbpal(3,ncoul(jpal),jpal)=107
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=180 ; rvbpal(2,ncoul(jpal),jpal)=180 ; rvbpal(3,ncoul(jpal),jpal)=180
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=200
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=231 ; rvbpal(2,ncoul(jpal),jpal)=107 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=239 ; rvbpal(2,ncoul(jpal),jpal)=040 ; rvbpal(3,ncoul(jpal),jpal)=041
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=140 ; rvbpal(2,ncoul(jpal),jpal)=008 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 7) then
    !
    ! -------------------------------------------------
    ! Palette CONTRASTE.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=180
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=042 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=094 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=145 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=196 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=247 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=212
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=160
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=109
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=095 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=147 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=198 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=224 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=249 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Jaune.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=191 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=128 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=064 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=045
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=097
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=149
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
    !
    ! Rose.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 8) then
    !
    ! -------------------------------------------------
    ! Palette arc-en-ciel.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ! 1.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=159
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=060 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=214
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    ! 2.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=046 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=114 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=187 ; rvbpal(3,ncoul(jpal),jpal)=255
    ! 3.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=199
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=135
    ! 4.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=063
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=063 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ! 5.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=131 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=199 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=238 ; rvbpal(3,ncoul(jpal),jpal)=000
    ! 6.
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=178 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=119 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 9) then
    !
    ! -------------------------------------------------
    ! Palette NOIR-BLANC.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 10) then
    !
    ! -------------------------------------------------
    ! Palette TEST.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 11) then
    !
    ! -------------------------------------------------
    ! Palette BLEU-BLANC-ROUGE.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ! ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 12) then
    !
    ! -------------------------------------------------
    ! Palette NOIR-BLEU-ROUGE-VERT-BLANC.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 13) then
    !
    ! -------------------------------------------------
    ! Palette VIOLET-BLANC.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Violet.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=084 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=159
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Orange.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=152 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
  elseif(jpal == 14) then
    !
    ! -------------------------------------------------
    ! Palette BLANC-NOIR.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Blanc.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
  elseif(jpal == 15) then
    !
    ! -------------------------------------------------
    ! Palette JAUNE-ROUGE-ROSE.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=214 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=172 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=131 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=090 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=048 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=034
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=076
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=117
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=158
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=200
  elseif(jpal == 16) then
    !
    ! -------------------------------------------------
    ! Palette BLEU-VERT-NOIR-ROUGE-ROSE.
    ! -------------------------------------------------
    !
    ncoul(jpal)=0
    !
    ! Bleu.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=180
    !
    ! Cyan.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=255
    !
    ! Vert.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=255 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Noir.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=000 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rouge.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=000
    !
    ! Rose.
    !
    ncoul(jpal)=ncoul(jpal)+1 ; rvbpal(1,ncoul(jpal),jpal)=255 ; rvbpal(2,ncoul(jpal),jpal)=000 ; rvbpal(3,ncoul(jpal),jpal)=255
  else
    print*,'img_pal_init/ERREUR: nombre de palettes trop grand!...'
    call exit(1)
  endif
enddo
end
subroutine img_pal(cdpal,cdtype,cdabs,pfrac,krvb)
! --------------------------------------------------------------
! Palette RVB continue.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!    cdpal type de palette: AQUABLUE, TMER, etc...
!    cdtype type de contours colorés: par paliers 'PAL' ou continus 'CONT'
!    cdabs type de grandeur réelle pfrac en entrée:
!        - 'ABS' si grandeur dimensionnelle
!        - 'FRAC' si grandeur adimensionnelle entre 0. et 1..
!    pfrac réel entre 0. et 1. donnant la position entre le mini et le maxi.
! En sortie:
!    krvb(1, ..., 3) les 3 valeurs RVB (entre 0 et 255).
! --------------------------------------------------------------
!
!
use imgyom
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdpal,cdtype,cdabs
character*200 clpal
real(kind=8) :: pfrac,zcoef,zfr,zeps,zreste,ztempe
integer(kind=4) :: krvb(3),ifrac,jfrac,jrvb,iper,ipal,jcoul,ipos
logical llper
zfr=pfrac
!
!-------------------------------------------------
! Si la palette est de type "AQUABLUE.PER4",
! c'est que l'utilisateur veut la palette AQUABLUE,
! mais en passant 4 fois du min au max de couleur
! entre le min et le max de son champ.
!-------------------------------------------------
!
if(index(cdpal,'.PER') == 0) then
  llper=.false.
  clpal=cdpal
else
  llper=.true.
  read(cdpal(index(cdpal,'.PER')+4:len_trim(cdpal)),fmt=*) iper
  clpal=cdpal(1:index(cdpal,'.PER')-1)
endif
!
!-------------------------------------------------
! Lien palette en clair <> indice du tableau rvbpal.
!-------------------------------------------------
!
if(clpal(1:len_trim(clpal)) == 'REF') then
  ipal=1
elseif(clpal(1:len_trim(clpal)) == 'AQUABLUE') then
  ipal=2
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une température
    ! en Kelvin.
    ! -------------------------------------------------
    !
    zfr=(zfr-260.)/(304.-260.)
  endif
elseif(clpal(1:len_trim(clpal)) == 'VEG') then
  ipal=3
elseif(clpal(1:len_trim(clpal)) == 'SAFO_TMER') then
  ipal=4
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une température
    ! en Kelvin.
    ! On la transforme en une fraction
    ! qui vaut 0 si T=5°C, 1 si T=29°C.
    ! -------------------------------------------------
    !
    zfr=(zfr-278.16)/24.
  endif
elseif(clpal(1:len_trim(clpal)) == 'TS') then
  ipal=5
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une température
    ! en Kelvin.
    ! On la transforme en une fraction
    ! qui vaut 0 si T=-30°C, 1 si T=40°C.
    ! -------------------------------------------------
    !
    zfr=(zfr-243.16)/70.
  endif
elseif(clpal(1:len_trim(clpal)) == 'OROG') then
  ipal=6
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une altitude en m.
    ! -------------------------------------------------
    !
    ! zfr=(zfr/2500.)**0.2
    zfr=tanh(zfr/700.)
  endif
elseif(clpal(1:len_trim(clpal)) == 'CONTRASTE') then
  ipal=7
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une température
    ! en Kelvin.
    ! On la transforme en une fraction
    ! qui vaut 0 si T=-50°C, 1 si T=40°C.
    ! -------------------------------------------------
    !
    zfr= 1. -(zfr-233.16)/90.
  endif
elseif(clpal(1:len_trim(clpal)) == 'ARC-EN-CIEL') then
  ipal=8
elseif(clpal(1:len_trim(clpal)) == 'NOIR-BLANC') then
  ipal=9
elseif(clpal(1:len_trim(clpal)) == 'TEST') then
  ipal=10
elseif(clpal(1:len_trim(clpal)) == 'BLEU-BLANC-ROUGE') then
  ipal=11
elseif(clpal(1:len_trim(clpal)) == 'NOIR-BLEU-ROUGE-VERT-BLANC') then
  ipal=12
elseif(clpal(1:len_trim(clpal)) == 'VIOLET-BLANC') then
  ipal=13
elseif(clpal(1:len_trim(clpal)) == 'BLANC-NOIR') then
  ipal=14
elseif(clpal(1:len_trim(clpal)) == 'JAUNE-ROUGE-ROSE') then
  ipal=15
elseif(clpal(1:len_trim(clpal)) == 'BLEU-VERT-ROUGE-ROSE') then
  ipal=16
elseif(clpal(1:len_trim(clpal)) == 'TGRAB') then
  !
  ! -------------------------------------------------
  ! Palette pour le grab des couleurs,
  ! i.e. T=valeur RVB.
  ! -------------------------------------------------
  !
  if(cdabs(1:len_trim(cdabs)) == 'ABS') then
    !
    ! -------------------------------------------------
    ! La grandeur zfr est supposée être une température
    ! en Kelvin.
    ! -------------------------------------------------
    !
    ztempe=zfr-273.16
    if(ztempe >= 0.) then
      krvb(1)=255
      krvb(2)=0
      krvb(3)=min(255,max(0,nint(ztempe)))
    else
      krvb(1)=min(255,max(0,nint(-ztempe)))
      krvb(2)=0
      krvb(3)=255
    endif
  else
    print*,'img_pal/ERREUR: la palette "TXV" n''est utilisable qu''avec le mode absolu "ABS"!...'
    call exit(1)
  endif
  return
else
  print*,'img_pal/ERREUR: palette inconnue: ',clpal(1:len_trim(clpal))
  call exit(1)
endif
if(llper) then
  !
  ! -------------------------------------------------
  ! Palette périodique.
  ! -------------------------------------------------
  !
  zfr=abs(sin(real(iper)*zfr*4.*atan(1.)))
endif
!
!-------------------------------------------------
! Pour éviter les erreurs d'arrondi,
! zfr ne doit pas prendre les valeurs extrêmes 0. et 1..
!-------------------------------------------------
!
zeps=1.e-4 ; zfr=min( 1. -zeps,max(zeps,zfr))
if(cdtype(1:len_trim(cdtype)) == 'CONT') then
  !
  ! -------------------------------------------------
  ! Palette continue.
  ! -------------------------------------------------
  !
  ipos=int(zfr*real(ncoul(ipal)-1))+1 ! ipos est dans [ 1 ; ncoul(ipal)-1 ]
  zreste=modulo(zfr*real(ncoul(ipal)-1), 1. )
  do jcoul=1,3
    krvb(jcoul)=nint(( 1. -zreste)*rvbpal(jcoul,ipos,ipal) &
      &+zreste*rvbpal(jcoul,ipos+1,ipal))
  enddo
else
  !
  ! -------------------------------------------------
  ! Palette par paliers.
  ! -------------------------------------------------
  !
  ipos=int(zfr*real(ncoul(ipal)))+1 ! ipos est dans [ 1 ; ncoul(ipal) ]
  do jcoul=1,3
    krvb(jcoul)=nint(rvbpal(jcoul,ipos,ipal))
  enddo
endif
end
subroutine img_transparence(krvb_fond,krvb_pp,popac,krvb)
! --------------------------------------------------------------
! **** *img_transparence* Surimposition d'une couleur en img_transparence sur une autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! krvb_fond: couleur du fond.
! krvb_pp: couleur du premier plan à surimposer.
! popac: opacité du premier plan: 0. si vitre parfaite, 1. si écran parfait.
! En sortie:
! krvb: couleur résultatnte.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
real(kind=8) :: popac,zopac
integer(kind=4) :: krvb_fond(3),krvb_pp(3),krvb(3)
!
!
!-------------------------------------------------
! Simple interpolation linéaire en RVB.
!-------------------------------------------------
!
zopac=max( 0. ,min( 1. ,popac))
krvb=nint(zopac*real(krvb_pp)+(1.-zopac)*real(krvb_fond))
end
subroutine img_taille(cdfic,kx,ky)
! --------------------------------------------------------------
! **** *img_taille* Lecture du nombre de pixels en X et Y d'une image ppm.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdfic
character*200 clc,clficppm
integer(kind=4) :: kx,ky
logical llex
!
!-------------------------------------------------
! Mise à indef.
!-------------------------------------------------
!
kx=-999
ky=-999
!
!-------------------------------------------------
! Le fichier existe-t-il bien?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llex)
if(.not.llex) then
  write(*,fmt=*) 'img/ERREUR: fichier ',cdfic(1:len_trim(cdfic)),' inexistant!...'
  call exit(1)
endif
!
!-------------------------------------------------
! Conversion éventuelle en ppm.
!-------------------------------------------------
!
call img_2ppm(cdfic,clficppm)
!
!-------------------------------------------------
! Lecture de l'en-tête.
!-------------------------------------------------
!
open(42,file=clficppm,form='formatted')
do
  read(42,fmt='(a)') clc
  if(clc(1:1) == '#') then
    !
    ! -------------------------------------------------
    ! Ligne de commentaire.
    ! -------------------------------------------------
    !
  elseif(clc == ' ') then
    !
    ! -------------------------------------------------
    ! Ligne vide.
    ! -------------------------------------------------
    !
  elseif(clc(1:1) == 'P') then
    !
    ! -------------------------------------------------
    ! Ligne de déclaration de type de ppm.
    ! -------------------------------------------------
    !
  else
    if(kx == -999) then
      read(clc,fmt=*) kx,ky  ! taille X et Y.
    endif
  endif
  if(kx /= -999) exit
enddo
close(42)
end
subroutine img_lec(cdfic,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_lec* Lecture des triplets RVB d'une image ppm.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdfic
integer(kind=4) :: kx,ky,krvb(3,kx,ky),ix,iy,ival
character*200 cltype,clc,clficppm
character*1 cl1
logical llexist
!
!-------------------------------------------------
! Le fichier existe-t-il?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llexist)
if(.not.llexist) then
  write(*,fmt=*) 'img_lec/ERREUR: le fichier image ',cdfic(1:len_trim(cdfic)),' n''existe pas!...'
  call exit(1)
endif
!
!-------------------------------------------------
! Conversion éventuelle en ppm.
!-------------------------------------------------
!
call img_2ppm(cdfic,clficppm)
!
!-------------------------------------------------
! Mise à indef.
!-------------------------------------------------
!
cltype='IIII'
ix=-999
iy=-999
ival=-999
!
!-------------------------------------------------
! Lecture de l'en-tête.
!-------------------------------------------------
!
open(42,file=clficppm,form='formatted')
do
  read(42,fmt='(a)') clc
  if(clc(1:1) == '#') then
    !
    ! -------------------------------------------------
    ! Ligne de commentaire.
    ! -------------------------------------------------
    !
  elseif(clc == ' ') then
    !
    ! -------------------------------------------------
    ! Ligne vide.
    ! -------------------------------------------------
    !
  else
    if(cltype(1:4) == 'IIII') then
      cltype=clc  ! P3, P5, etc...
    elseif(ix == -999) then
      read(clc,fmt=*) ix,iy  ! taille X et Y.
    elseif(ival == -999) then
      read(clc,fmt=*) ival  ! 255.
    endif
  endif
  if(ival /= -999) exit
enddo
!
!-------------------------------------------------
! Tests de cohérence.
!-------------------------------------------------
!
if(ix /= kx.or.iy /= ky) then
  print*,'img_lec/ERREUR: taille de l''image passée en argument' &
    &,' non compatible avec la taille lue dans le fichier ' &
    &,clficppm(1:len_trim(clficppm))
  call exit(1)
endif
if(ival /= 255) then
  print*,'img_lec/ERREUR: image non codée sur 255 couleurs' &
    &,' dans le fichier ',clficppm(1:len_trim(clficppm))
  call exit(1)
endif
!
!-------------------------------------------------
! Lecture des données.
!-------------------------------------------------
!
if(cltype(1:len_trim(cltype)) == 'P3') then
  !
  ! -------------------------------------------------
  ! PPM ASCII.
  ! -------------------------------------------------
  !
  call img_lec_asc(42,ix,iy,krvb)
  close(42)
elseif(cltype(1:len_trim(cltype)) == 'P6') then
  !
  ! -------------------------------------------------
  ! PPM RAW.
  ! -------------------------------------------------
  !
  close(42)
  call img_lec_raw(clficppm,ix,iy,krvb)
elseif(cltype(1:len_trim(cltype)) == 'P2') then
  !
  ! -------------------------------------------------
  ! PGM gris ASCII.
  ! -------------------------------------------------
  !
  print*,'Type de fichier image non encore prévu: ',cltype(1:len_trim(cltype))
  close(42)
  call exit(1)
elseif(cltype(1:len_trim(cltype)) == 'P5') then
  !
  ! -------------------------------------------------
  ! PGM gris RAW.
  ! -------------------------------------------------
  !
  print*,'Type de fichier image non encore prévu: ',cltype(1:len_trim(cltype))
  close(42)
  call exit(1)
else
  print*,'img_lec/ERREUR: format d''image non reconnu: ',cltype(1:len_trim(cltype)) &
    &,' dans le fichier ',clficppm(1:len_trim(clficppm))
  close(42)
  call exit(1)
endif
end
subroutine img_ecr(cdfic,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_ecr* Ecriture des triplets RVB sur un fichier ppm ascii.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdfic
integer(kind=4) :: kx,ky,krvb(3,kx,ky),jx,jy,jcoul,irvb
character*(3*kx*ky) clrvb
character*200 clsuff
character*1500 clexe
integer(kind=4) :: system,ierr
!
!-------------------------------------------------
! On sécurise l'image d'entrée.
!-------------------------------------------------
!
krvb=max(0,min(255,krvb))
!
!-------------------------------------------------
! Ouverture du fichier de sortie.
!-------------------------------------------------
!
open(45,file=cdfic,form='formatted')
!
!-------------------------------------------------
! En-tête.
!-------------------------------------------------
!
write(45,fmt='(a)') 'P6'
write(45,fmt=*) kx,ky
write(45,fmt=*) 255
irvb=0
do jy=1,ky
  do jx=1,kx
    irvb=irvb+1 ; clrvb(irvb:irvb)=char(krvb(1,jx,jy))
    irvb=irvb+1 ; clrvb(irvb:irvb)=char(krvb(2,jx,jy))
    irvb=irvb+1 ; clrvb(irvb:irvb)=char(krvb(3,jx,jy))
  enddo
enddo
write(45,fmt='(a)') clrvb
!
!-------------------------------------------------
! Fermeture du fichier de sortie.
!-------------------------------------------------
!
close(45)
!
!-------------------------------------------------
! Si le fichier de sortie demandé n'est pas un ppm,
! on utilise convert.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Suffixe du fichier.
!-------------------------------------------------
!
if(len_trim(cdfic) >= 3) then
  clsuff=cdfic(len_trim(cdfic)-2:len_trim(cdfic))
else
  clsuff=' '
endif
if(clsuff(1:3) /= 'ppm' .and. index(cdfic,'.') /= 0) then
  !
  ! -------------------------------------------------
  ! Le fichier de sortie n'est pas un PPM et comporte un suffixe.
  ! On lance l'ordre UNIX "convert".
  ! -------------------------------------------------
  !
  write(clexe,fmt='(100a)') 'mv ',cdfic(1:len_trim(cdfic)),' .',cdfic(1:len_trim(cdfic)) &
    & ,'.tmp.int ; convert .',cdfic(1:len_trim(cdfic)),'.tmp.int ',cdfic(1:len_trim(cdfic))
  !write(*,fmt=*) clexe(1:len_trim(clexe))
  ierr=system(clexe)
  if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
endif
end
subroutine img_lec_asc(kul,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_lec_asc* Lecture des triplets RVB d'une image ppm ascii.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
integer(kind=4) :: kx,ky,krvb(3,kx,ky)
integer(kind=4) :: jx,jy,jcoul
character*1 cl1
integer(kind=4) :: icoul
integer(kind=4) :: ientier
integer(kind=4) :: ios
integer(kind=4) :: ipos
integer(kind=4) :: ix
integer(kind=4) :: iy
integer(kind=4) :: kul
integer(kind=4) :: igeom
logical ll_entier
ll_entier=.false.
ipos=0
do
  read(kul,fmt='(a)',iostat=ios,advance='no') cl1
  if(ios == -1) then
    !
    ! -------------------------------------------------
    ! Fin de fichier.
    ! -------------------------------------------------
    !
    cl1='u'
  elseif(ios == -2) then
    !
    ! -------------------------------------------------
    ! Fin de ligne.
    ! -------------------------------------------------
    !
    cl1=char(10)
  elseif(ios == 0) then
    !
    ! -------------------------------------------------
    ! Cas général.
    ! -------------------------------------------------
    !
  else
    !
    ! -------------------------------------------------
    ! Cas non prévu.
    ! -------------------------------------------------
    !
    print*,'Code réponse en lecture non prévu: ',ios
    call exit(1)
  endif
  if(cl1 >= '0'.and.cl1 <= '9') then
    !
    ! -------------------------------------------------
    ! On est dans un entier.
    ! -------------------------------------------------
    !
    if(ll_entier) then
      !
      ! -------------------------------------------------
      ! On était dans un entier.
      ! -------------------------------------------------
      !
      ientier=10*ientier+ichar(cl1)-ichar('0')
    else
      !
      ! -------------------------------------------------
      ! On était hors d'un entier.
      ! -------------------------------------------------
      !
      ientier=ichar(cl1)-ichar('0')
    endif
    ll_entier=.true.
  else
    !
    ! -------------------------------------------------
    ! On est hors d'un entier.
    ! -------------------------------------------------
    !
    if(ll_entier) then
      !
      ! -------------------------------------------------
      ! On était dans un entier.
      ! -------------------------------------------------
      !
      ipos=ipos+1
      icoul=mod(ipos-1,3)+1
      igeom=(ipos-1)/3
      ix=mod(igeom,kx)+1
      iy=igeom/kx+1
      krvb(icoul,ix,iy)=ientier
    else
      !
      ! -------------------------------------------------
      ! On était hors d'un entier.
      ! Rien à faire.
      ! -------------------------------------------------
      !
    endif
    ll_entier=.false.
  endif
  if(cl1 == 'u') exit
enddo
if(ipos /= 3*kx*ky) then
  print*,'img_lec_asc/ERREUR: nombre erroné d''entiers lus sur le ppm!...'
  print*,kx,ky,ipos
  call exit(1)
endif
end
subroutine img_lec_raw(cdfic,kx,ky,krvb)
! --------------------------------------------------------------
! Lecture des triplets RVB d'une image ppm raw.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
integer(kind=4) :: kx,ky,krvb(3,kx,ky),itail,ix,iy,ival,iligne,icar1,icar2
integer(kind=4) :: jx,jy,jcoul,ic,jc,ioctets,jcar
character*(3*kx*ky) clrvb
integer(kind=4) :: kul
character*1, allocatable :: clcar(:)
character*(*) cdfic
character*200 cltype,clc
!
!-------------------------------------------------
! Lecture du fichier sur une chaîne de caractères.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Obtention de la taille du fichier en octets.
!-------------------------------------------------
!
call io_taille(cdfic,itail)
print*,'			',itail,' octets'
!
!-------------------------------------------------
! Allocation du tableau de caractères.
!-------------------------------------------------
!
allocate(clcar(itail))
!
!-------------------------------------------------
! Lecture.
!-------------------------------------------------
!
call io_lec(cdfic,itail,clcar)
!
!-------------------------------------------------
! On va passer la zone d'autodocumentation,
! pour atteindre la zone des triplets RVB.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Mise à indef.
!-------------------------------------------------
!
cltype='IIII'
ix=-999
iy=-999
ival=-999
!
!-------------------------------------------------
! Lecture de l'en-tête.
!-------------------------------------------------
!
iligne=0
do
  iligne=iligne+1
  call io_ligne(iligne,itail,clcar,icar1,icar2,clc)
  if(clc(1:1) == '#') then
    !
    ! -------------------------------------------------
    ! Ligne de commentaire.
    ! -------------------------------------------------
    !
  elseif(clc == ' ') then
    !
    ! -------------------------------------------------
    ! Ligne vide.
    ! -------------------------------------------------
    !
  else
    if(cltype(1:4) == 'IIII') then
      cltype=clc  ! P3, P5, etc...
    elseif(ix == -999) then
      read(clc,fmt=*) ix,iy  ! taille X et Y.
    elseif(ival == -999) then
      read(clc,fmt=*) ival  ! 255.
    endif
  endif
  if(ival /= -999) exit
enddo
!
!-------------------------------------------------
! Lecture de la chaîne sur des entiers.
!-------------------------------------------------
!
ic=icar2+2
do jy=1,ky
  do jx=1,kx
    do jcoul=1,3
      krvb(jcoul,jx,jy)=ichar(clcar(ic))
      ic=ic+1
    enddo
  enddo
enddo
end
subroutine img_lec_carac(kul,cdlec,koctets)
! --------------------------------------------------------------
! Lecture de n octets d'un fichier sur une chaîne de caractères.
! --------------------------------------------------------------
! Sujet:
!
! On lit ici sur un fichier formatté, indépendamment
! des fins de ligne. La fin de lecture intervient
! si on est en fin de fichier ou en fin de chaîne.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul unité logique du fichier d'entrée.
! En sortie:
! cdlec chaîne sur laquelle ont été portés les octets lus.
! koctets nombre d'octets lus.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdlec
integer(kind=4) :: kul,ipos,ios,isize,koctets
ipos=1
do
  read(kul,fmt='(a)',iostat=ios,size=isize,advance='no') cdlec(ipos:len(cdlec))
  if(ios == -2) then
    !
    ! -------------------------------------------------
    ! Fin de ligne.
    ! -------------------------------------------------
    !
    cdlec(ipos+isize:ipos+isize)=char(10)
    ipos=ipos+isize+1
  elseif(ios == -1) then
    !
    ! -------------------------------------------------
    ! Fin de fichier.
    ! -------------------------------------------------
    !
    koctets=ipos+isize-1
    return
  elseif(ios == 0) then
    !
    ! -------------------------------------------------
    ! Fin	de chaîne.
    ! -------------------------------------------------
    !
    koctets=len(cdlec)
    return
  else
    print*,'img_lec_carac/ERREUR: réponse inconnue ios=',ios
    call exit(1)
  endif
enddo
end
subroutine img_to_dataftn(cdficsor,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_to_dataftn* Ecriture d'un source ftn tabulant une image RVB.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! cdficsor: nom du sce ftn de sortie.
! kx, ky: dimensions de l'image.
! krvb: triplets RVB de l'image.
! En sortie:
! écriture du fichier cdficsor
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
integer(kind=4) :: kx,ky,krvb(3,kx,ky),inum,jx,jy,jcoul
character*(*) cdficsor
character*200 clficsor
!
clficsor=cdficsor(1:len_trim(cdficsor))//'.F90'
open(43,file=clficsor,form='formatted')
write(43,fmt='(3a)') 'subroutine ',cdficsor(1:len_trim(cdficsor)),'_taille(kx,ky)'
write(43,fmt=*) 'kx=',kx,' ; ky=',ky
write(43,fmt=*) 'end'
write(43,fmt='(3a)') 'subroutine ',cdficsor(1:len_trim(cdficsor)),'_rvb(kx,ky,krvb)'
write(43,fmt=*) 'integer(kind=4) krvb(3,',kx,',',ky,')'
do jy=1,ky
  do jx=1,kx
    write(43,fmt=*) &
      &'krvb(1,',jx,',',jy,')=',krvb(1,jx,jy) &
      &,' ; krvb(2,',jx,',',jy,')=',krvb(2,jx,jy) &
      &,' ; krvb(3,',jx,',',jy,')=',krvb(3,jx,jy)
  enddo
enddo
write(43,fmt=*) 'end'
close(43)
end
subroutine img_texte(cdtexte,krvb_fond_texte,popac_fond_texte &
  &,krvb_pp_texte,popac_pp_texte,kfonte,kloc,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_texte* Surimposition d'une image RVB contenant un texte avec son fond.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! cdtexte: texte à écrire
! krvb_fond_texte(3): couleur du fond du texte
! popac_fond_texte: opacité du fond du texte
! krvb_pp_texte(3): couleur du pp du texte
! popac_pp_texte: opacité du pp du texte
! kfonte: fonte du texte:
!		- si kfonte=1, fonte 9x15bold
! kloc: localisation du texte dans l'image de sortie
! kx, ky: taille de l'image en E/S
! En entrée/sortie:
! krvb: triplets RVB de l'image sur laquelle on surimpose le texte.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdtexte
integer(kind=4) :: krvb_fond_texte(3),krvb_pp_texte(3),inbcar,ityps(4)
integer(kind=4) :: kfonte,kloc(3),kx,ky,krvb(3,kx,ky),irvb_fond_gol_texte
integer(kind=4) :: ix_texte,iy_texte,jcoul,jx,jy
integer(kind=4), allocatable :: irvb_texte(:,:,:)
real(kind=8) :: popac_fond_texte,popac_pp_texte
!
!
!-------------------------------------------------
! Détermination de la taille de l'image-texte.
! Elle est fonction du nombre de caractères
! et de la fonte utilisée.
!-------------------------------------------------
!
if(kfonte == 1) then
  call img_fonte_9x15bold_taille(cdtexte,ix_texte,iy_texte)
else
  print*,'RVBTEXTE/ERREUR: numéro de fonte inconnu: ',kfonte
  call exit(1)
endif
!
!-------------------------------------------------
! Création de l'image-fond de texte.
!-------------------------------------------------
!
allocate(irvb_texte(3,ix_texte,iy_texte))
do jcoul=1,3
  do jy=1,iy_texte
    do jx=1,ix_texte
      irvb_texte(jcoul,jx,jy)=krvb_fond_texte(jcoul)
    enddo
  enddo
enddo
!
!-------------------------------------------------
! On surimpose cette l'image-fond de texte
! sur l'image en E/S.
!-------------------------------------------------
!
ityps(1)=0
call img_surimpose_image(ix_texte,iy_texte,irvb_texte,popac_fond_texte &
  &,ityps,kloc,kx,ky,krvb)
!
!-------------------------------------------------
! Ecriture du texte pp sous forme d'image RVB.
! Le fond sera d'une couleur différente du pp,
! la valeur elle-même important peu puisque
! par la suite elle sera utilisée en opacité nulle.
!-------------------------------------------------
!
irvb_fond_gol_texte=255-krvb_pp_texte(1)
if(kfonte == 1) then
  call img_fonte_9x15bold_rvb(cdtexte,krvb_pp_texte &
    &,ix_texte,iy_texte,irvb_fond_gol_texte,irvb_texte)
else
  print*,'RVBTEXTE/ERREUR: numéro de fonte inconnu: ',kfonte
  call exit(1)
endif
!
!-------------------------------------------------
! On surimpose cette l'image-fond de texte
! sur l'image en E/S.
!-------------------------------------------------
!
ityps(1)=1
ityps(2)=irvb_fond_gol_texte
ityps(3)=irvb_fond_gol_texte
ityps(4)=irvb_fond_gol_texte
call img_surimpose_image(ix_texte,iy_texte,irvb_texte,popac_pp_texte &
  &,ityps,kloc,kx,ky,krvb)
end
function zlisse(pratio)
! --------------------------------------------------------------
! **** *zlisse* Fonction calculant la cubique de dérivée nulle en 0 et 1, et y valant 0 et 1 respectivement.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	pratio réel quelconque.
! En sortie:
!	zlisse valeur de la cuubique.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
real(kind=8) :: pratio,zlisse
!
if(pratio >  1. ) then
  zlisse= 1.
elseif(pratio <  0. ) then
  zlisse= 0.
else
  zlisse=pratio*pratio*(3.-2.*pratio)
endif
end
subroutine img_2ppm(cdfic,cdficppm)
! --------------------------------------------------------------
! **** *img_2ppm*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdfic nom d'un fichier image.
! En sortie:
!	cdfic nom du fichier PPM équivalent.
!		si le fichier d'entrée était un PPM, cdifcppm=cdfic.
!		sinon, cdficppm est égal à cdfic".ppm" et convert a été appelé pour créer ce fichier.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdfic,cdficppm
character*2 cl2
character*1500 clexe
logical llexist,llrecent
integer(kind=4) :: system,ierr
!
!-------------------------------------------------
! Le fichier existe-t-il?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llexist)
if(.not.llexist) then
  write(*,fmt=*) 'img_lec/ERREUR: le fichier image ',cdfic(1:len_trim(cdfic)),' n''existe pas!...'
  call exit(1)
endif
!
!-------------------------------------------------
! Lecture des 2 premiers octets de cdfic.
!-------------------------------------------------
!
open(42,file=cdfic,form='formatted')
read(42,fmt='(a)',advance='no') cl2
close(42)
if(cl2(1:2) == 'P6') then
  !
  ! -------------------------------------------------
  ! Le fichier est un PPM.
  ! -------------------------------------------------
  !
  cdficppm=cdfic
else
  !
  ! -------------------------------------------------
  ! Le fichier n'est pas un PPM.
  ! -------------------------------------------------
  !
  cdficppm='.'//cdfic(1:len_trim(cdfic))//'.tmp.ppm'
  !
  ! -------------------------------------------------
  ! Le fichier PPM associé existe-t-il?
  ! -------------------------------------------------
  !
  inquire(file=cdficppm,exist=llexist)
  if(llexist) then
    !
    ! -------------------------------------------------
    ! Le fichier PPM associé existe.
    ! Est-il plus récent que le fichier de base?
    ! -------------------------------------------------
    !
    call io_plus_vieux(cdfic,cdficppm,llrecent)
  else
    llrecent=.false.
  endif
  if(.not.llexist .or. (llexist .and. .not.llrecent)) then
    !
    ! -------------------------------------------------
    ! On lance l'ordre UNIX "convert".
    ! -------------------------------------------------
    !
    write(clexe,fmt='(9a)') 'convert ',cdfic(1:len_trim(cdfic)),' ',cdficppm(1:len_trim(cdficppm))
    !write(*,fmt=*) clexe(1:len_trim(clexe))
    ierr=system(clexe)
    if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
  endif
endif
end
subroutine img_legende(pxmin,pxmax,knbprint,cdpal,kpixx,kpixy,krvb_leg)
! --------------------------------------------------------------
! **** *img_legende*
! --------------------------------------------------------------
! Sujet:
!	Création d'une image contenant des réels pointés le long d'un segment
!	coloré selon une palette donnée. Ceci fournit à l'utilisateur la légende des couleurs
!	de la palette utilisée, i.e. le lien entre couleur et valeur réelle.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2003-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	pxmin: minimum des réels.
!	pxmax: maximum des réels.
!	knbprint: nb de valeurs réelles à pointer; si 0, ce nb sera choisi par le logiciel.
!	cdpal: palette de tracé.
!	kpixx: taille en pixels X de l'image-légende.
!	kpixy: taille en pixels Y de l'image-légende; conseillé: kpixy=40.
! En sortie:
!	krvb_leg image-légende.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
character*(*) cdpal
real(kind=8) :: pxmin,pxmax,zfrac
integer(kind=4) :: kpixx,kpixy,krvb_leg(3,kpixx,kpixy),irvb_loc(3),jx,jy,ipixlegx,jcoul
integer(kind=4) :: jval,inval
character*200 cltexte
integer(kind=4) :: ifonte
integer(kind=4) :: iloc(3)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: knbprint
real(kind=8) :: zval
real(kind=8) :: zopac_fond_texte
real(kind=8) :: zopac_pp_texte
!
!-------------------------------------------------
! Initialisation du fond à gris.
!-------------------------------------------------
!
krvb_leg=191
!
!-------------------------------------------------
! Bordure blanche.
!-------------------------------------------------
!
do jy=1,kpixy
  do jx=1,kpixx,kpixx-1
    do jcoul=1,3
      krvb_leg(jcoul,jx,jy)=255
    enddo
  enddo
enddo
do jy=1,kpixy,kpixy-1
  do jx=1,kpixx
    do jcoul=1,3
      krvb_leg(jcoul,jx,jy)=255
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Ecriture des pixels colorés.
!-------------------------------------------------
!
ipixlegx=(kpixx*3)/4
do jx=1,ipixlegx
  zfrac=real(jx-1)/real(ipixlegx-1)
  call img_pal(cdpal,'CONT','FRAC',zfrac,irvb_loc)
  do jy=1,8
    do jcoul=1,3
      krvb_leg(jcoul,jx+(kpixx-ipixlegx)/2,jy+(kpixy*2)/3)=irvb_loc(jcoul)
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Ecriture du texte.
!-------------------------------------------------
!
if(knbprint == 0) then
  inval=max(2,ipixlegx/112) ! nombre de valeurs réelles à pointer.
else
  inval=knbprint
endif
do jval=1,inval
  !
  ! -------------------------------------------------
  ! Ecriture des valeurs réelles.
  ! -------------------------------------------------
  !
  zval=pxmin+real(jval-1)/real(inval-1)*(pxmax-pxmin)
  write(cltexte,fmt='(g16.3)') zval
  !call reecar(zval,-1,0,cltexte,iltexte)
  cltexte=adjustl(cltexte)
  !
  ! -------------------------------------------------
  ! Placage de ce texte sur l'image-légende.
  ! -------------------------------------------------
  !
  irvb_fond_texte=191
  zopac_fond_texte=0.
  irvb_pp_texte=0
  zopac_pp_texte=1.
  ifonte=1
  iloc(1)=10 ! coordonnées centrées.
  iloc(2)=1+(kpixx-ipixlegx)/2+(jval-1)*((ipixlegx)/(inval-1))
  iloc(3)=kpixy/3
  call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
    &,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kpixx,kpixy,krvb_leg)
enddo
end
subroutine img_bordure(krvb,kx,ky,krvb_bord)
! --------------------------------------------------------------
! **** *img_bordure*
! --------------------------------------------------------------
! Sujet:
!	Ajour d'une bordure à l'image, de couleur spécifiée.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2003-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree/sortie:
!	krvb(3,kx,ky): image RVB.
! En entree:
!	kx,ky dimension de l'image.
!	krvb_bord: couleur du bord dont il faut entourer l'image.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
real(kind=8) :: pxmin,pxmax,zfrac
integer(kind=4) :: krvb_bord(3),kx,ky,krvb(3,kx,ky),jx,jy,jc
!
!-------------------------------------------------
! Bordure.
!-------------------------------------------------
!
do jx=1,kx
  do jc=1,3
    krvb(jc,jx,1)=krvb_bord(jc)
    krvb(jc,jx,ky)=krvb_bord(jc)
  enddo
enddo
do jy=1,ky
  do jc=1,3
    krvb(jc,1,jy)=krvb_bord(jc)
    krvb(jc,kx,jy)=krvb_bord(jc)
  enddo
enddo
end
subroutine img_graduation(pval,kval,px,py,krvb_fond_texte,popac_fond_texte &
  &,krvb_pp_texte,popac_pp_texte,kfonte,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_graduation* Pointage des valeurs réelles d'une graduation.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!
! La graduation est donnée comme une ligne brisée de kval points.
! Chaque point est donné par 
! - sa localisation X: px(jval), jval=1,kval.
! - sa localisation Y: py(jval), jval=1,kval.
! - la valeur de la graduation en ce point: pval(jval), jval=1,kval.
! Donc si on veut définir une graduation régulière: kval=2.
! Pour une graduation irrégulière: kval > 2.
! ATTENTION: les valeurs, dans pval, doivent être croissantes!...
!
! krvb_fond_texte(3): couleur du fond du texte
! popac_fond_texte: opacité du fond du texte
! krvb_pp_texte(3): couleur du pp du texte
! popac_pp_texte: opacité du pp du texte
! kfonte: fonte du texte:
!		- si kfonte=1, fonte 9x15bold
! kx, ky: taille de l'image en E/S
!
! En entrée/sortie:
!
! krvb: triplets RVB de l'image sur laquelle on surimpose le texte.
!
! --------------------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
integer(kind=4) :: krvb_fond_texte(3),krvb_pp_texte(3),inbcar,ityps(4)
integer(kind=4) :: kfonte,kx,ky,krvb(3,kx,ky),irvb_fond_gol_texte
integer(kind=4) :: ix_texte,iy_texte,jcoul,jx,jy,iloc(3)
integer(kind=4), allocatable :: irvb_texte(:,:,:)
real(kind=8) :: popac_fond_texte,popac_pp_texte
real(kind=8) :: pval(kval),px(kval),py(kval)
integer(kind=4), parameter :: jpclas=100
real(kind=8) :: zclas(jpclas)
!
!-------------------------------------------------
! Longueur de la ligne brisée en pixels.
!-------------------------------------------------
!
zlong=0
do jval=1,kval-1
  zlong=zlong+sqrt((px(jval+1)-px(jval))**2+(py(jval+1)-py(jval))**2)
enddo
zmin=real(minval(pval))
zmax=real(maxval(pval))
!
!-------------------------------------------------
! Nombre approx. de réels cotables: proportionnel à la longueur de la ligne brisée.
!-------------------------------------------------
!
inb=nint(zlong*12./600.) ! environ 12 cotations sur 600 pixels.
!
!-------------------------------------------------
! Ecartement entre deux cotes.
!-------------------------------------------------
!
zec=(zmax-zmin)/real(inb)
!
!-------------------------------------------------
! Arrondi à un ch. significatif.
!-------------------------------------------------
!
call arrr(zec,1,zec2)
zec=zec2
!
!-------------------------------------------------
! Si l'écart est un nombre "usuel", on l'arrondit à l'entier le plus proche.
!-------------------------------------------------
!
if(zec > 0.5 .and. zec < 100000.) then
  !
  !-------------------------------------------------
  ! On va coter des entiers.
  !-------------------------------------------------
  !
  zec2=real(nint(zec))
  zec=zec2
  llentier=.true.
else
  !
  !-------------------------------------------------
  ! On va coter des réels.
  ! On en met deux fois moins (occupent plus de caractères).
  !-------------------------------------------------
  !
  llentier=.false.
  zec=zec*2.
  call arrr(zec,1,zec2)
  zec=zec2
endif
!
!-------------------------------------------------
! On crée iclas classes à coter.
!-------------------------------------------------
!
llp0=.false.
call cree_classes(zmin,zmax,zec,llp0,zeps,jpclas,iclas,zclas)
!
!-------------------------------------------------
! Ecriture du texte de cotation sur l'image.
!-------------------------------------------------
!
do jclas=1,iclas
  if(llentier) then
    !
    !-------------------------------------------------
    ! Cotation d'entiers.
    !-------------------------------------------------
    !
    write(cltexte,fmt=*) nint(zclas(jclas))
  else
    !
    !-------------------------------------------------
    ! Cotation de réels.
    !-------------------------------------------------
    !
    write(cltexte,fmt='(f10.2)') zclas(jclas)
  endif
  cltexte=adjustl(cltexte)
  !
  !-------------------------------------------------
  ! Ecriture de texte.
  !-------------------------------------------------
  !
  ! Localisation du texte.
  !
  llok=.false.
  do jval=1,kval-1
    if(zclas(jclas) >= pval(jval)  .and. zclas(jclas) < pval(jval+1)) then
      !
      !-------------------------------------------------
      ! La valeur courante à coter est dans le segment
      ! courant de la ligne brisée d'entrée.
      !-------------------------------------------------
      !
      zxpos=px(jval)+(px(jval+1)-px(jval))*(zclas(jclas)-pval(jval))/(pval(jval+1)-pval(jval))
      ixpos=max(1,min(kx,nint(zxpos)))
      zypos=py(jval)+(py(jval+1)-py(jval))*(zclas(jclas)-pval(jval))/(pval(jval+1)-pval(jval))
      iypos=max(1,min(ky,nint(zypos)))
      llok=.true.
    endif
  enddo
  if(llok) then
    iloc(1)=10 ! centrage.
    iloc(2)=ixpos ! X.
    iloc(3)=iypos ! Y.
    call img_texte(cltexte,krvb_fond_texte,popac_fond_texte &
    &,krvb_pp_texte,popac_pp_texte,kfonte,iloc,kx,ky,krvb)
  endif
enddo
end
subroutine cree_classes(pmin,pmax,pec,ldp0,peps,kpclas,kclas,pclas)
! --------------------------------------------------------------
! **** *cree_classes* Fourniture d'un tableau de classes à partir de min et max.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! ----------
! pmin		min
! pmax		max
! pec		écartement imposé, 0. si non fourni
! ldp0		vrai si on ne veut pas de 0 comme classe
! peps		valeur voisine de 0 permettant de l'encadrer sans passer dessus
! kpclas	dimension statique du tableau pclas
! En sortie:
! ----------
! kclas		nombre de réels écrits sur le tableau pclas
! pclas		valeurs des classes
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
real(kind=8) pclas(kpclas)
inomc=26 ! nombre classes désirées maximal.
iclass_max=0
if(pec == 0.) then
  !
  ! L'écart n'a pas été fourni par l'utilisateur.
  ! On le calcule.
  ! Méthode: on recherche quel est l'écart
  ! sur un seul chiffre significatif,
  ! qui confère le nombre de classes le plus
  ! grand, en restant cependant inférieur à inomc.
  !
  do jnomc=1,inomc
    zdx=(pmax-pmin)/real(jnomc)
    !
    ! On arrondit l'écart à un seul chiffre significatif.
    !
    call arrr(zdx,1,zec)
    !
    ! On en déduit combien de classes il y a.
    !
    iclass=nint((pmax-pmin)/zec)
    if(iclass <= inomc .and. iclass > iclass_max) then
      pec=zec
      iclass_max=iclass
    endif
  enddo
endif
!
! Calcul des min et max de tracé en fonction des min et max réels.
! On veut des multiples de l'écart demandé.
!
zmint=pec*nint(pmin/pec+0.5)
if(zmint > pmin+0.9*pec) zmint=zmint-pec
zmaxt=pec*nint(pmax/pec-0.5)
if(zmaxt < pmax-pec*.9) zmaxt=zmaxt+pec
!
! Nombre de classes.
!
!print*,'	Minimum de tracé: ',zmint,' max: ',zmaxt,' écart : ',pec
i1=nint(zmint/pec)
i2=nint(zmaxt/pec)
!
! Ecriture du tableau de sortie.
!
kclas=0
do jclas=i1,i2
  if(jclas == 0.and.ldp0) then
    !
    ! On ne veut pas du zéro.
    !
    if(peps >= pec.or.peps == 0.) then
      !
      ! L'epsilon fourni par l'utilisateur
      ! est plus grand que l'écart!...
      ! On ne met rien du tout.
      !
    else
      kclas=kclas+1
      pclas(kclas)=-peps
      kclas=kclas+1
      pclas(kclas)=peps
    endif
  else
    kclas=kclas+1
    pclas(kclas)=real(jclas)*pec
  endif
enddo
end
subroutine arrr(px,kncs,px2)
! --------------------------------------------------------------------------
! **** *ARRONDI*  - Arrondi de reel.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:  J.M.Piriou
! -------
! Original : 92-02
! ----------
! Modifications:
! --------------------------------------------------------------------------
! En Entree       :
! px          : reel a convertir
! kncs        : nombre de chiffres significatifs desires
! En Sortie       :
! px2         : reel arrondi a kncs chiffres significatifs.
! --------------------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
if(kncs < 1.or.kncs > 10) then
  ! Cas nombre de chiffres significatifs errone
  px2=0.
elseif(px == 0.) then
  ! Cas nombre nul
  px2=0.
else
  ! Cas general
  zx=px
  if(zx < 0.) then
    zx=-zx
    isign=-1
  else
    isign=1
  endif
  zlog=log(zx)/log(10.)
  ilog=nint(zlog-.5)
  ! On calcule la mantisse de zx, soit un nombre
  ! zmant tel que 1.<=zmant<10.
  zmant=zx/10.**float(ilog)
  if(zmant >= 10.) then
    zmant=zmant/10.
    ilog=ilog+1
  elseif(zmant < 1.) then
    zmant=zmant*10.
    ilog=ilog-1
  endif
  ! On ne conserve que kncs chiffres significatifs
  z10=10.**float(kncs-1)
  imant=nint(zmant*z10)
  if(imant == 3) then
    ! On ne veut pas d'un arrondi au chiffre 3.
    !imant=2
  elseif(imant == 4) then
    ! On ne veut pas d'un arrondi au chiffre 4.
    !imant=5
  elseif(imant == 6) then
    ! On ne veut pas d'un arrondi au chiffre 6.
    !imant=5
  elseif(imant == 7) then
    ! On ne veut pas d'un arrondi au chiffre 7.
    !imant=5
  elseif(imant == 8) then
    ! On ne veut pas d'un arrondi au chiffre 8.
    imant=10
  elseif(imant == 9) then
    ! On ne veut pas d'un arrondi au chiffre 9.
    imant=10
  else
  endif
  zmant=float(imant)/z10
  px2=isign*zmant*10.**float(ilog)
endif
end
subroutine img_trac2d(pval,kx,ky,pmin,pmax,cdpal,cdtitre,ldlegende,kximage,kyimage,cdficppm)
! --------------------------------------------------------------
! **** *trac2d* Ecriture d'un fichier image, contenant le tracé 2D d'un tableau 2D de réels.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! 	pval: tableau de réels à tracer.
!	kx,ky: dimensions de pval.
!	pmin,pmax: valeurs de pval à associer au minimum (resp. maximum) de la palette de couleurs de sortie.
!	cdpal: palette de couleurs demandée.
!	cdtitre: titre du graphique de sortie.
!	ldlegende: vrai si on veut une légende du lien entre couleurs et valeurs réelles.
!	kximage,kyimage,cdficppm: tailles X,Y et nom de l'image de sortie.
! En sortie:
!	Ecriture du fichier cdficppm.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
real(kind=8) pval(kx,ky),zfrac,zopac_fond_texte
character*(*) cdpal,cdtitre,cdficppm
integer(kind=4) :: irvb(3,kximage,kyimage)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: iloc(3)
integer(kind=4) :: ityps(4)
integer(kind=4), allocatable :: irvb_leg(:,:,:)
!
!-------------------------------------------------
! Initialisation à gris.
!-------------------------------------------------
!
irvb=191
!
!-------------------------------------------------
! Taille de l'image de sortie affectée aux titres et légendes.
!-------------------------------------------------
!
if(ldlegende .and. cdtitre /= ' ') then
  !
  ! Légende et titre.
  !
  iytitleg=50
  lltitre=.true.
elseif(ldlegende .and. cdtitre == ' ') then
  !
  ! Légende et pas de titre.
  !
  iytitleg=25
  lltitre=.false.
elseif(.not. ldlegende .and. cdtitre == ' ') then
  !
  ! Pas de légende et pas de titre.
  !
  iytitleg=0
  lltitre=.false.
else
  !
  ! Pas de légende et titre.
  !
  iytitleg=25
  lltitre=.true.
endif
!
!-------------------------------------------------
! Calcul de l'image de sortie.
!-------------------------------------------------
!
call img_pal_init
iy=kyimage-iytitleg
ix=kximage
if(pmin >= pmax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac2D/ERREUR: le minimum doit être < au maximum!...'
  write(*,fmt=*) trim(cdtitre),pmin,pmax
  call exit(1)
endif
do jy=1,iy
  do jx=1,ix
    zfx=(real(jx)-0.5)/real(ix)
    zfy=(real(iy-jy+1)-0.5)/real(iy)
    itabx=int(real(kx)*zfx)+1
    itaby=int(real(ky)*zfy)+1
    zfrac=(pval(itabx,itaby)-pmin)/(pmax-pmin)
    call img_pal(cdpal,'CONT','FRAC',zfrac,irvb(1,jx,jy))
  enddo
enddo
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
iloc(3)=kyimage-nint(0.75*real(iytitleg))
call img_texte(cdtitre,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Légende. On crée la légende d'un champ entre zxmin et zxmax,
! avec la palette clpal, légende dont la taille pixels est (ipixx,ipixy).
!-------------------------------------------------
!
ipixx=kximage
ipixy=iytitleg/2
allocate(irvb_leg(3,ipixx,ipixy))
zmin=pmin
zmax=pmax
inbprint=0
clpal=cdpal
call img_legende(zmin,zmax,inbprint,clpal,ipixx,ipixy,irvb_leg)
!
!-------------------------------------------------
! On surimpose cette l'image-fond de texte
! sur l'image en E/S.
!-------------------------------------------------
!
zopac=1.
ityps(1)=0
iloc(1)=1
call img_surimpose_image(ipixx,ipixy,irvb_leg,zopac &
  &,ityps,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Ecriture du fichier-image.
!-------------------------------------------------
!
call img_ecr(cdficppm,kximage,kyimage,irvb)
end
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
! Auteur:   2004-07, J.M. Piriou.
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
! En sortie:
!	Ecriture du fichier cdficppm.
! --------------------------------------------------------------
!
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
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
integer(kind=4), parameter :: jpcoul=5 ! nombre max. de couleurs possibles, et donc de courbes gérables.
integer(kind=4) :: irvb_courbes(3,jpcoul)
real(kind=8) zopac_fond_texte
!
!-------------------------------------------------
! Initialisation des couleurs de courbes.
!-------------------------------------------------
!
ic=0
ic=ic+1 ; irvb_courbes(1,ic)=255 ; irvb_courbes(2,ic)=255 ; irvb_courbes(3,ic)=255 ! blanc.
ic=ic+1 ; irvb_courbes(1,ic)=255 ; irvb_courbes(2,ic)=000 ; irvb_courbes(3,ic)=000 ! rouge.
ic=ic+1 ; irvb_courbes(1,ic)=000 ; irvb_courbes(2,ic)=255 ; irvb_courbes(3,ic)=000 ! vert.
ic=ic+1 ; irvb_courbes(1,ic)=000 ; irvb_courbes(2,ic)=255 ; irvb_courbes(3,ic)=255 ! cyan.
ic=ic+1 ; irvb_courbes(1,ic)=200 ; irvb_courbes(2,ic)=200 ; irvb_courbes(3,ic)=200 ! gris clair.
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
! Initialisation à gris sombre.
!-------------------------------------------------
!
irvb=050
!
!-------------------------------------------------
! Taille de l'image de sortie affectée aux titres et légendes.
!-------------------------------------------------
!
!
! Légende et titre.
!
iytitleg=50
lltitre=.true.
!
!-------------------------------------------------
! Initialisation de la partie légende à gris clair.
!-------------------------------------------------
!
do jx=1,kximage
  do jy=1,iytitleg
    do jc=1,3
      irvb(jc,jx,jy+kyimage-iytitleg)=191
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Calcul de l'image de sortie.
!-------------------------------------------------
!
iy=kyimage-iytitleg
ix=kximage
if(pxmin >= pxmax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac1D/ERREUR: le minimum de X doit être < au maximum!...'
  write(*,fmt=*) pxmin,pxmax
  call exit(1)
endif
if(pymin >= pymax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac1D/ERREUR: le minimum de Y doit être < au maximum!...'
  write(*,fmt=*) pymin,pymax
  call exit(1)
endif
do jcourbes=1,kcourbes
  do jbris=1,kbris-1
    !
    !-------------------------------------------------
    ! Saut si donnée manquante ou saut imposé.
    !-------------------------------------------------
    !
    if(px(jcourbes,jbris) == pindef .or. px(jcourbes,jbris) == psaut) cycle
    if(py(jcourbes,jbris) == pindef .or. py(jcourbes,jbris) == psaut) cycle
    !
    !-------------------------------------------------
    ! Coordonnées du pixel associé au jbris ième point de la ligne brisée.
    !-------------------------------------------------
    !
    itabx0=int((px(jcourbes,jbris)-pxmin)/(pxmax-pxmin)*real(ix))
    itaby0=int((1.-(py(jcourbes,jbris)-pymin)/(pymax-pymin))*real(iy))
    if(itabx0 > ix .or. itabx0 < 1) cycle
    if(itaby0 > iy .or. itaby0 < 1) cycle
    !
    !-------------------------------------------------
    ! Coordonnées du pixel associé au (jbris+1) ième point de la ligne brisée.
    !-------------------------------------------------
    !
    itabx1=int((px(jcourbes,jbris+1)-pxmin)/(pxmax-pxmin)*real(ix))
    itaby1=int((1.-(py(jcourbes,jbris+1)-pymin)/(pymax-pymin))*real(iy))
    if(itabx1 > ix .or. itabx1 < 1) cycle
    if(itaby1 > iy .or. itaby1 < 1) cycle
    !
    !-------------------------------------------------
    ! Tracé d'un trait de largeur kpix_largeur pixels reliant ces points successifs.
    !-------------------------------------------------
    !
    do jc=1,3
      irvb_trait(jc)=irvb_courbes(jc,modulo(jcourbes-1,jpcoul)+1)
    enddo
    call img_trait(itabx0,itaby0,itabx1,itaby1,kpix_largeur,irvb_trait,irvb,kximage,kyimage)
  enddo
enddo
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
iloc(3)=kyimage-nint(0.75*real(iytitleg))
call img_texte(cdtitre,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Ecriture du fichier-image.
!-------------------------------------------------
!
call img_ecr(cdficppm,kximage,kyimage,irvb)
end
subroutine img_trait(ktabx0,ktaby0,ktabx1,ktaby1,kpix_largeur,krvb_trait,krvb,kximage,kyimage)
! --------------------------------------------------------------
! **** *img_trait* Tracé d'un trait de largeur donnée, d'un point A à un point B.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!		Pour chaque point à l'intérieur du rectangle dont une diagonale
!		est le segment [A,B] allongé de 2*kpix_largeur, on calcule la distance
!		au segment [A,B]. Si cette distance est inférieure à kpix_largeur,
!		on affecte au point la couleur krvb_trait.
! Externes:
! Auteur:   2004-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entrée:
!	ktabx0,ktaby0: coordonnées pixels du point A.
!	ktabx1,ktaby1: coordonnées pixels du point B.
!	kpix_largeur: largeur du trait en pixels.
!	krvb_trait(3): couleur du trait désiré.
! En entrée/sortie:
!	krvb(3,kximage,kyimage): image sur laquelle tracer le trait.
! --------------------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
integer(kind=4), intent(in) :: krvb_trait(3)
integer(kind=4), intent(inout) :: krvb(3,kximage,kyimage)
!
!-------------------------------------------------
! Coin bas gauche du rectangle dont une diagonale
! est le segment [A,B] allongé de 2*kpix_largeur.
!-------------------------------------------------
!
ixbg=max(1,min(ktabx0-kpix_largeur/2,ktabx1-kpix_largeur/2)) ; iybg=max(1,min(ktaby0-kpix_largeur/2,ktaby1-kpix_largeur))
!
!-------------------------------------------------
! Coin haut droit du rectangle dont une diagonale
! est le segment [A,B] allongé de 2*kpix_largeur.
!-------------------------------------------------
!
ixhd=min(kximage,max(ktabx0+kpix_largeur/2,ktabx1+kpix_largeur/2))
iyhd=min(kyimage,max(ktaby0+kpix_largeur/2,ktaby1+kpix_largeur/2))
!
!-------------------------------------------------
! Equation de la diagonale D: sin(theta)x-cos(theta)*y=b.
!-------------------------------------------------
!
zxpente=real(ktabx1-ktabx0)
zypente=real(-ktaby1+ktaby0)
call recpol(zxpente,zypente,zr,ztheta)
zb=sin(ztheta)*(real(ktabx0)-0.5)-cos(ztheta)*(real(-ktaby0)-0.5)
zpi=4.*atan(1.)
!
!-------------------------------------------------
! Boucle sur tous les points de ce rectangle.
!-------------------------------------------------
!
do jx=ixbg,ixhd
  do jy=iybg,iyhd
    !
    !-------------------------------------------------
    ! Coordonnées du point courant C.
    !-------------------------------------------------
    !
    zx=real(jx)-0.5
    zy=-real(jy)+0.5
    !
    !-------------------------------------------------
    ! Equation de la droite E passant par C, perpendiculaire à D.
    !-------------------------------------------------
    !
    ztheta_per=ztheta+0.5*zpi
    zb_per=sin(ztheta_per)*zx-cos(ztheta_per)*zy
    !
    !-------------------------------------------------
    ! Intersection I de D et E.
    !-------------------------------------------------
    !
    zxinter=(-zb*cos(ztheta_per)+zb_per*cos(ztheta))
    zyinter=(sin(ztheta)*zb_per-sin(ztheta_per)*zb)
    !
    !-------------------------------------------------
    ! Distance de C à la droite D: c'est la distance
    ! de C à I.
    !-------------------------------------------------
    !
    zdist=sqrt((zx-zxinter)**2+(zy-zyinter)**2)
    if(zdist < 0.5*real(kpix_largeur)) then
      !
      !-------------------------------------------------
      ! Le point courant C est à une distance du segment
      ! inférieure à kpix_largeur/2. On le colorie.
      !-------------------------------------------------
      !
      do jc=1,3
        krvb(jc,jx,jy)=krvb_trait(jc)
      enddo
    endif
  enddo
enddo
end
