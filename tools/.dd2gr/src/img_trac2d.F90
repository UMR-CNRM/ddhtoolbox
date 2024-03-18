subroutine img_trac2d(pval,kx,ky,pmin,pmax,cdcoord,cdpal &
&,cdfdc,cdtitre,ldlegende,ldlegendexy,cdlegx,cdlegy,kximage &
&,kyimage,pindef,krvb_indef,cdficppm)
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
!   2012-09, J.M. Piriou: palette irrégulière par paliers spécifiée par l'utilisateur (#PALETTE=SPECIF).
!   2013-02, J.M. Piriou: ajout d'une 2ème bordure noire verticale à D de la légende.
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
!	kximage,kyimage,cdficppm: tailles X,Y et nom de l'image de sortie.
!  pindef: valeur réelle que prend le tableau pval, lorsque l'utilisateur veut spécifier une valeur manquante.
!  krvb_indef(3): couleur que l'utilisateur veut voir associée aux valeurs manquantes.
! En sortie:
!	Ecriture du fichier cdficppm.
! --------------------------------------------------------------
!
use parametres, only : cgpalspec,npalspec,rindef
#include"implicit_r8i4.h"
!
real(kind=8), intent(in) :: pval(kx,ky)
character(len=*), intent(in) :: cdpal,cdtitre,cdficppm,cdfdc,cdcoord,cdlegx,cdlegy
integer(kind=4), intent(in) :: krvb_indef(3)
integer(kind=4) :: irvb(3,kximage,kyimage)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: iloc(3)
integer(kind=4) :: ityps(4)
integer(kind=4) :: irvb_legendexy_pp(3)
integer(kind=4) :: irvb_legendexy_fond(3)
integer(kind=4), allocatable :: irvb_leg(:,:,:)
real(kind=8) :: zval_gradu(2) ! valeurs pour les graduations en X et Y.
real(kind=8) :: zxpix_graduation(2), zypix_graduation(2)
character(len=180) clmot(40)
!
!-------------------------------------------------
! Initialisation à blanc.
!-------------------------------------------------
!
irvb=255
!
!-------------------------------------------------
! Taille de l'image de sortie affectée aux titres et légendes.
!-------------------------------------------------
!
if(ldlegende .and. cdtitre /= ' ') then
  !
  ! Légende (à droite), titre (1ère ligne) et min/max (2ème ligne).
  !
  ixleg=120 ! taille X en pixels de la légende.
  iytitleg=50
  lltitre=.true.
  llminmax=.true.
elseif(ldlegende .and. cdtitre == ' ') then
  !
  ! Légende (à droite) et min/max (1ère ligne), pas de titre.
  !
  ixleg=120 ! taille X en pixels de la légende.
  iytitleg=25
  lltitre=.false.
  llminmax=.true.
elseif(.not. ldlegende .and. cdtitre == ' ') then
  !
  ! Pas de légende (à droite), pas de titre, pas de min/max.
  !
  ixleg=0 ! taille X en pixels de la légende.
  iytitleg=0
  lltitre=.false.
  llminmax=.false.
else
  !
  ! Pas de légende (à droite), présence d'un titre et min/max.
  !
  ixleg=0 ! taille X en pixels de la légende.
  iytitleg=50
  lltitre=.true.
  llminmax=.true.
endif
if(ldlegendexy) then
  !
  !-------------------------------------------------
  ! L'utilisateur veut la légende des axes X et Y.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Taille en pixels du bord blanc laissé à droite, à gauche, en haut, en bas du tracé des valeurs 2D.
  !-------------------------------------------------
  !
  ibordblcd=0 
  ibordblcg=65 
  ibordblch=15 
  ibordblcb=30
else
  ibordblcd=0 
  ibordblcg=0 
  ibordblch=0 
  ibordblcb=0 
endif
!
!-------------------------------------------------
! Taille en pixels de la zone qui va recevoir
! le tracé coloré des valeurs.
!-------------------------------------------------
!
ixpix=kximage-ibordblcg-ibordblcd-ixleg
iypix=kyimage-iytitleg-ibordblch-ibordblcb
!
!
!-------------------------------------------------
! Calcul des couleurs du champ, et imposition sur l'image de sortie.
!-------------------------------------------------
!
call img_pal_init
if(pmin >= pmax) then
  write(*,fmt=*) 
  write(*,fmt=*) 'img_trac2D/ERREUR: le minimum doit être < au maximum!...'
  write(*,fmt=*) trim(cdtitre),pmin,pmax
  call exit(1)
endif
!write(*,fmt=*) '	img_trac2D/palette = ',trim(cdpal)
clmethode='INTERPOLATION'
clmethode='ECHANTILLONNAGE'
zmin=rindef
do jy=1,iypix
  do jx=1,ixpix
    if(trim(clmethode) == 'ECHANTILLONNAGE') then
      !
      !-------------------------------------------------
      ! Pour passer de la grille du champ interpolé
      ! à celle de l'image on fait un simple échantillonnage
      ! (sur ou sous-échantillonnage) .
      !-------------------------------------------------
      !
      zfx=(real(jx)-0.5)/real(ixpix)
      zfy=(real(iypix-jy+1)-0.5)/real(iypix)
      itabx=max(1,min(kx,int(real(kx)*zfx)+1))
      itaby=max(1,min(ky,int(real(ky)*zfy)+1))
      zval=pval(itabx,itaby)
    else
      !
      !-------------------------------------------------
      ! Pour passer de la grille du champ interpolé
      ! à celle de l'image on fait une interpolation ou une extrapolation.
      !-------------------------------------------------
      !
      zfx=(real(jx)-0.5)/real(ixpix)
      zfy=(real(iypix-jy+1)-0.5)/real(iypix)
      itabx=max(1,min(kx,nint(real(kx)*zfx))) ; zfxcible=(real(itabx)-0.5)/real(kx)
      itaby=max(1,min(ky,nint(real(ky)*zfy))) ; zfycible=(real(itaby)-0.5)/real(ky)
      zpoidsx=(zfx-zfxcible)*real(kx)
      zpoidsy=(zfy-zfycible)*real(ky)
      zmoyx=(1.-zpoidsx)*pval(itabx,itaby)+zpoidsx*pval(min(kx,itabx+1),itaby)
      zmoyy=(1.-zpoidsy)*pval(itabx,itaby)+zpoidsy*pval(itabx,min(ky,itaby+1))
      zval=0.5*(zmoyx+zmoyy)
    endif
    if(llminmax) then
      !
      !-------------------------------------------------
      ! Quelques statistiques sur le champ interpolé.
      !-------------------------------------------------
      !
      if(zmin == rindef .and. zval /= rindef) then
        zmin=zval
        zmax=zval
        zmoy=0.
        zsx2=0.
        isomme=0
      elseif(zval /= rindef) then
        zmin=min(zmin,zval)
        zmax=max(zmax,zval)
        zmoy=zmoy+zval
        zsx2=zsx2+zval*zval
        isomme=isomme+1
      endif
  
    endif
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
          call exit(1)    
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
              irvb(1,jx+ibordblcg,jy+ibordblch)=irspec
              irvb(2,jx+ibordblcg,jy+ibordblch)=ivspec
              irvb(3,jx+ibordblcg,jy+ibordblch)=ibspec
              lloktrav=.true.
              exit
            endif
          elseif(trim(clmot(1)) == '>') then
            irvb(1,jx+ibordblcg,jy+ibordblch)=irspec
            irvb(2,jx+ibordblcg,jy+ibordblch)=ivspec
            irvb(3,jx+ibordblcg,jy+ibordblch)=ibspec
            lloktrav=.true.
            exit
          else
            write(*,fmt=*) 
            write(*,fmt=*) 'img_trac2D/ERREUR: syntaxe de la spécification de palette:'
            write(*,fmt=*) trim(cgpalspec(jpalspec))
            write(*,fmt=*) 'on doit trouver un signe < ou > .'
            call exit(1)           
          endif
        enddo
        if(.not.lloktrav) then
          write(*,fmt=*) 
          write(*,fmt=*) 'img_trac2D/ERREUR: la dernière spécification de seuil #PAL_SPECIF doit commencer par le signe > !'
          write(*,fmt=*) zval,npalspec,trim(clmot(1)),irspec,ivspec,ibspec
          call exit(1)   
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
        call img_pal(cdpal,'CONT','FRAC',zfrac,irvb(1,jx+ibordblcg,jy+ibordblch))
      else
        !
        !-------------------------------------------------
        ! L'utilisateur veut une palette automatique.
        ! C'est une palette qui sera différente suivant 
        ! que le champ réel embrasse 0 ou non,
        ! et une palette discrète.
        !-------------------------------------------------
        !
        call img_pal(cdpal,cltype,clabs,zval,irvb(1,jx+ibordblcg,jy+ibordblch))
      endif
    else
      !
      !-------------------------------------------------
      ! Valeur manquante.
      !-------------------------------------------------
      !
      irvb(1,jx+ibordblcg,jy+ibordblch)=krvb_indef(1)
      irvb(2,jx+ibordblcg,jy+ibordblch)=krvb_indef(2)
      irvb(3,jx+ibordblcg,jy+ibordblch)=krvb_indef(3)
    endif
  enddo
enddo
!
!-------------------------------------------------
! Superposition du fond de carte.
!-------------------------------------------------
!
if(trim(cdfdc) /= 'non') then
  call img_fdc(cdfdc,cdcoord,kximage,kyimage,ibordblcg,ibordblch,ixpix,iypix,irvb)
endif
!
!-------------------------------------------------
! On initialise à gris la zone "titre + légende".
!-------------------------------------------------
!
do jx=1,kximage
  do jy=kyimage-iytitleg,kyimage
    do jc=1,3
      irvb(jc,jx,jy)=191
    enddo
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
ipixx=ixleg
ipixy=iypix
allocate(irvb_leg(3,ipixx,ipixy))
inbprint=0
clpal=cdpal
call img_legende(pmin,pmax,inbprint,clpal,ipixx,ipixy,irvb_leg)
!
!-------------------------------------------------
! On surimpose cette image-fond de texte
! sur l'image en E/S.
!-------------------------------------------------
!
zopac=1.
ityps(1)=0
iloc(1)=0 ; iloc(2)=kximage-ixleg+1 ; iloc(3)=ibordblch+1
call img_surimpose_image(ipixx,ipixy,irvb_leg,zopac &
  &,ityps,iloc,kximage,kyimage,irvb)
!
!-------------------------------------------------
! Légende des axes X et Y.
!-------------------------------------------------
!
if(ldlegendexy) then
  !
  !-------------------------------------------------
  ! Légende des X et Y demandée.
  !-------------------------------------------------
  !
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
  ixdepart=ibordblcg ; iydepart=ibordblch
  ixarrivee=kximage-ibordblcd+1 ; iyarrivee=iydepart
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
  !
  ! Trait bas.
  !
  ixdepart=ibordblcg ; iydepart=kyimage-iytitleg-ibordblcb+1
  ixarrivee=kximage-ibordblcd+1 ; iyarrivee=iydepart
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
  !
  ! Trait G.
  !
  ixdepart=ibordblcg ; iydepart=ibordblch
  ixarrivee=ixdepart ; iyarrivee=kyimage-iytitleg-ibordblcb+1
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
  !
  ! Trait D.
  !
  ixdepart=kximage-ibordblcd-ixleg+1 ; iydepart=ibordblch
  ixarrivee=ixdepart ; iyarrivee=kyimage-iytitleg-ibordblcb+1
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
  !
  ! 2ème trait D: celui à D de la légende.
  !
  ixdepart=kximage ; iydepart=ibordblch
  ixarrivee=ixdepart ; iyarrivee=kyimage-iytitleg-ibordblcb+1
  call img_trait(ixdepart,iydepart,ixarrivee,iyarrivee,ipix_largeur,irvb_legendexy_pp,irvb,kximage,kyimage)
  !
  !-------------------------------------------------
  ! Graduation X.
  !-------------------------------------------------
  !
  if(cdcoord == ' ') then
    write(*,fmt=*) 
    write(*,fmt=*) 'img_trac2d/ERREUR: si tracé des axes X et Y, alors cdcoord doit être fourni!...'
    write(*,fmt=*) 
    call exit(1)
  endif
  read(cdcoord,fmt=*) zxmin,zxmax,zymin,zymax
  ival=2
  zval_gradu(1)=zxmin ; zval_gradu(2)=zxmax
  zxpix_graduation(1)=real(ibordblcg)
  zxpix_graduation(2)=real(kximage-ibordblcd-ixleg+1)
  zypix_graduation(1)=real(ibordblch+iypix+ibordblcb/2)
  zypix_graduation(2)=zypix_graduation(1)
  zopac_fond=0.
  zopac_pp=1.
  ifonte=1
  call img_graduation(zval_gradu,ival,zxpix_graduation,zypix_graduation,irvb_legendexy_fond,zopac_fond &
    &,irvb_legendexy_pp,zopac_pp,1,kximage,kyimage,irvb)
  !
  !-------------------------------------------------
  ! Graduation Y.
  !-------------------------------------------------
  !
  ival=2
  zval_gradu(1)=zymin ; zval_gradu(2)=zymax
  zxpix_graduation(1)=real(ibordblcg/2)
  zxpix_graduation(2)=zxpix_graduation(1)
  zypix_graduation(1)=real(ibordblch+iypix)
  zypix_graduation(2)=real(ibordblch)
  call img_graduation(zval_gradu,ival,zxpix_graduation,zypix_graduation,irvb_legendexy_fond,zopac_fond &
    &,irvb_legendexy_pp,zopac_pp,1,kximage,kyimage,irvb)
  !
  !-------------------------------------------------
  ! Nom et unité de l'axe X.
  !-------------------------------------------------
  !
  if(cdlegx /= ' ') then
    iloc(1)=0 ! coordonnées alignées à gauche.
    iloc(2)=kximage-ibordblcd-ixleg-9*(len_trim(cdlegx))-17
    iloc(3)=kyimage-iytitleg-ibordblcb-19
    zopac_fond=0.5
    zopac_pp=1.
    call img_texte(cdlegx,irvb_legendexy_fond,zopac_fond &
    &,irvb_legendexy_pp,zopac_pp,ifonte,iloc,kximage,kyimage,irvb)
  endif
  !
  !-------------------------------------------------
  ! Nom et unité de l'axe Y.
  !-------------------------------------------------
  !
  if(cdlegy /= ' ') then
    iloc(1)=0 ! coordonnées alignées à gauche.
    iloc(2)=ibordblcg+1
    iloc(3)=ibordblch+3
    zopac_fond=0.5
    zopac_pp=1.
    call img_texte(cdlegy,irvb_legendexy_fond,zopac_fond &
    &,irvb_legendexy_pp,zopac_pp,ifonte,iloc,kximage,kyimage,irvb)
  endif
endif
if(llminmax) then
  !
  !-------------------------------------------------
  ! Quelques statistiques.
  !-------------------------------------------------
  !
  if(isomme /= 0) then
    zmoy=zmoy/real(isomme)
    zrcm=sqrt(max(0.,zsx2/real(isomme)))
    call reecar(zmin,-1,3,clmin,iltmp)
    call reecar(zmax,-1,3,clmax,iltmp)
    call reecar(zmoy,-1,3,clmoy,iltmp)
    call reecar(zrcm,-1,3,clrcm,iltmp)
    clminmax='Min =  '//trim(clmin)//', Max = '//trim(clmax) &
      &//', Moy = '//trim(clmoy)//', Rcm = '//trim(clrcm)
    print*,'  Statistiques sur la grille de tracé final (',ixpix,' * ',iypix,' pixels):'
    print*,'    ',trim(clminmax)
  else
    print*,'  Toutes les valeurs sont manquantes!...'
    clminmax=' '
  endif
  
  !
  !-------------------------------------------------
  ! Ecriture du texte clminmax sur l'image.
  !-------------------------------------------------
  !
  irvb_fond_texte=191
  zopac_fond_texte=0.
  irvb_pp_texte=0
  zopac_pp_texte=1.
  ifonte=1
  iloc(1)=10 ! coordonnées centrées.
  iloc(2)=kximage/2
  iloc(3)=kyimage-nint(0.75*real(iytitleg))+25
  call img_texte(clminmax,irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kximage,kyimage,irvb)
endif
!
!-------------------------------------------------
! Ecriture du fichier-image.
!-------------------------------------------------
!
call img_ecr(cdficppm,kximage,kyimage,irvb)
end
