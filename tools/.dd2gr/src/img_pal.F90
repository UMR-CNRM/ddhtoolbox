recursive subroutine img_pal(cdpal,cdtype,cdabs,pfrac,krvb)
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
use parametres
!
#include"implicit_r8i4.h"
!
character*(*) cdpal,cdtype,cdabs
character*400 clpal
real(kind=8) :: pfrac,zcoef,zfr,zeps,zreste,ztempe
integer(kind=4) :: krvb(3)
integer(kind=4) :: ifrac,jfrac,jrvb,iper,ipal,jcoul,ipos
logical llper
real(kind=8) :: ztab_ad_hoc(0:10)
zfrac=pfrac
zfr=zfrac
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
if(clpal(1:4) == 'AUTO') then
  zfr=0. ! cette initialisation sert seulement à éviter l'usage éventuel d'indefs.
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
elseif(clpal(1:len_trim(clpal)) == 'TbIR10.8µm') then
  ipal=19
  !
  !-------------------------------------------------
  ! Egalisation d'histogramme: on a tabulé ci-dessous, telle quelle,  la courbe de répartition
  ! des Tb IR 10.8 µm d'une image METEOSAT, en échantillonnant 11 valeurs.
  !-------------------------------------------------
  !
  ztab_ad_hoc(0)=0.00
  ztab_ad_hoc(1)=0.025
  ztab_ad_hoc(2)=0.04
  ztab_ad_hoc(3)=0.06
  ztab_ad_hoc(4)=0.10
  ztab_ad_hoc(5)=0.165
  ztab_ad_hoc(6)=0.23
  ztab_ad_hoc(7)=0.38
  ztab_ad_hoc(8)=0.54
  ztab_ad_hoc(9)=0.88
  ztab_ad_hoc(10)=1.00
  !
  !-------------------------------------------------
  ! La courbe de répartition ztab_ad_hoc ci-dessus est utilisée maintenant
  ! pour convoluer la palette de couleur: la fraction zfrac entre 0 et 1,
  ! est transformée une autre valeur zfrac dépendant de cette courbe de
  ! répartion. On fait ici une égalisation d'histogramme, ce qui est simple
  ! car cela consiste à faire zfrac=f(zfrac), où f est la fonction de
  ! répartition des données ci--dessus (donnée ici par ztab_ad_hoc).
  !
  ! Remarque: il est alors conseillé de mettre les extrêmes de tracé suivants:
  ! #VMIN=225.
  ! #VMAX=300.
  !
  !-------------------------------------------------
  !
  zfrac=max(0.00001,min(0.999,zfrac))
  itab=1+int(zfrac*10.)
  zfractab=1.+zfrac*10.-real(itab)
  zfrac=ztab_ad_hoc(itab-1)*(1.-zfractab)+ztab_ad_hoc(itab)*zfractab
  zfr=zfrac
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
  ipal=indice_palette(clpal)
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
if(clpal(1:4) == 'AUTO') then
  !
  !-------------------------------------------------
  ! Palette automatique.
  !-------------------------------------------------
  !
  read(clpal(5:),fmt=*) zec,zmin,zmax
  if(zmin*zmax < 0.) then
    !
    !-------------------------------------------------
    ! Le champ comporte à la fois des valeurs positives et négatives.
    !-------------------------------------------------
    !
    if(abs(zfrac) < zec) then
      !
      !-------------------------------------------------
      ! Valeurs négligeables.
      !-------------------------------------------------
      !
      krvb(1)=nrvb_negl(1) ; krvb(2)=nrvb_negl(2) ; krvb(3)=nrvb_negl(3)
    elseif(zfrac > 0.) then
      !
      !-------------------------------------------------
      ! Valeurs positives.
      !-------------------------------------------------
      !
      zfr=(zfrac- 0. )/( zmax - 0. )
      zeps=1.e-4 ; zfr=min( 1. -zeps,max(zeps,zfr))
      ipal=indice_palette(cgpaldef_pos)
      !
      ! -------------------------------------------------
      ! Palette continue.
      ! -------------------------------------------------
      !
      ipos=max(1,min(ncoul(ipal)-1,int(zfr*real(ncoul(ipal)-1))+1)) ! ipos est dans [ 1 ; ncoul(ipal)-1 ]
      zreste=modulo(zfr*real(ncoul(ipal)-1), 1. )
      do jcoul=1,3
        krvb(jcoul)=nint(( 1. -zreste)*rvbpal(jcoul,ipos,ipal) &
          &+zreste*rvbpal(jcoul,ipos+1,ipal))
      enddo
    else
      !
      !-------------------------------------------------
      ! Valeurs négatives.
      !-------------------------------------------------
      !
      zfr=(zfrac-zmin)/( 0. -zmin)
      zeps=1.e-4 ; zfr=min( 1. -zeps,max(zeps,zfr))
      ipal=indice_palette(cgpaldef_neg)
      !
      ! -------------------------------------------------
      ! Palette continue.
      ! -------------------------------------------------
      !
      ipos=max(1,min(ncoul(ipal)-1,int(zfr*real(ncoul(ipal)-1))+1)) ! ipos est dans [ 1 ; ncoul(ipal)-1 ]
      zreste=modulo(zfr*real(ncoul(ipal)-1), 1. )
      do jcoul=1,3
        krvb(jcoul)=nint(( 1. -zreste)*rvbpal(jcoul,ipos,ipal) &
          &+zreste*rvbpal(jcoul,ipos+1,ipal))
      enddo
    endif
  else
    !
    !-------------------------------------------------
    ! Le champ est de signe constant (que des valeurs positives,
    ! ou bien que des valeurs négatives).
    !-------------------------------------------------
    !
    if(trim(cgtyppal) == 'LIN') then
      zfr=(zfrac-zmin)/(zmax-zmin)
    else
      if(zfrac <= 0. .or. zmin <= 0. .or. zmax <= 0.) then
        !
        ! Cas d'une valeur négative, pas traitable dans une approche log.
        ! Pour ne pas faire planter le tracé on affecte la valeur minimale.
        !
        zfr=0.
      else
        zfr=log(zfrac/zmin)/log(zmax/zmin)
      endif
    endif
    zeps=1.e-4 ; zfr=min( 1. -zeps,max(zeps,zfr))
    !ipal=17 ! palette BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE.
    !ipal=20 ! palette BLANC-BLEU-VERT-ROUGE.
    ipal=indice_palette(cgpaldef_cst)
    !ipal=indice_palette('BLANC-BLEU-VERT-ROUGE')
    !
    ! -------------------------------------------------
    ! Palette continue.
    ! -------------------------------------------------
    !
    ipos=max(1,min(ncoul(ipal)-1,int(zfr*real(ncoul(ipal)-1))+1)) ! ipos est dans [ 1 ; ncoul(ipal)-1 ]
    zreste=modulo(zfr*real(ncoul(ipal)-1), 1. )
    do jcoul=1,3
      krvb(jcoul)=nint(( 1. -zreste)*rvbpal(jcoul,ipos,ipal) &
        &+zreste*rvbpal(jcoul,ipos+1,ipal))
    enddo
  endif
elseif(cdtype(1:len_trim(cdtype)) == 'CONT') then
  !
  ! -------------------------------------------------
  ! Palette continue.
  ! -------------------------------------------------
  !
  ipos=max(1,min(ncoul(ipal)-1,int(zfr*real(ncoul(ipal)-1))+1)) ! ipos est dans [ 1 ; ncoul(ipal)-1 ]
  zreste=modulo(zfr*real(ncoul(ipal)-1), 1. )
  do jcoul=1,3
    krvb(jcoul)=nint(( 1. -zreste)*rvbpal(jcoul,ipos,ipal) &
      &+zreste*rvbpal(jcoul,ipos+1,ipal))
  enddo
elseif(cdtype(1:len_trim(cdtype)) == 'PAL') then
  !
  ! -------------------------------------------------
  ! Palette par paliers.
  ! -------------------------------------------------
  !
  ipos=max(1,min(ncoul(ipal),int(zfr*real(ncoul(ipal)))+1)) ! ipos est dans [ 1 ; ncoul(ipal) ]
  do jcoul=1,3
    krvb(jcoul)=nint(rvbpal(jcoul,ipos,ipal))
  enddo
else
  write(*,fmt=*) 
  write(*,fmt=*) 'img_pal/ERREUR: type de choix CONT / PAL / AUTO incorrect!...'
  write(*,fmt=*) 
  call exit(1)
endif
end
