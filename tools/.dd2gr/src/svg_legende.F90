subroutine svg_legende(cdpal)
! --------------------------------------------------------------
! **** *svg_legende*
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
! Auteur:   2014-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdpal: palette de tracé.
! En sortie:
!	écriture sur le fichier SVG, d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
character*(*) cdpal
integer(kind=4) :: iloc(3)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: irvb(3)
real(kind=8), allocatable :: zseuil(:)
integer(kind=4), allocatable :: irvb_palette(:,:)
character(len=180) clmot(40)
real(kind=8) zprinc(jplignes)
real(kind=8) zsec(jplignes)
!
!-------------------------------------------------
! Test du type de palette.
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
    write(*,fmt=*) 'svg_legende/ERREUR: palette spécifiée par l''utilisateur, or aucune spécification de plages de réels effectuée.'
    call exit(1)    
  endif
  !
  !-------------------------------------------------
  ! Nombre de seuils-réels et de palettes de couleur:
  ! une palette colorée de plus que de seuils-réels.
  !-------------------------------------------------
  !
  iseuil=npalspec-1
  allocate(zseuil(iseuil))
  allocate(irvb_palette(3,iseuil+1))
  !
  !-------------------------------------------------
  ! Remplissage des valeurs de seuils et de couleur RVB de la palette par paliers.
  !-------------------------------------------------
  !
  do jpalspec=1,iseuil+1
    call casc(cgpalspec(jpalspec),1,clmot,ilmot)
    read(clmot(2),fmt=*) zseuil(min(iseuil,jpalspec))
    read(clmot(3),fmt=*) irvb_palette(1,jpalspec)
    read(clmot(4),fmt=*) irvb_palette(2,jpalspec)
    read(clmot(5),fmt=*) irvb_palette(3,jpalspec)
  enddo
  !
  !-------------------------------------------------
  ! Unité.
  !-------------------------------------------------
  !
  clunite=cgunite
  !
  !-------------------------------------------------
  ! Appel à la routine de tracé de la légende discrète.
  !-------------------------------------------------
  !
  call svg_legende_discrete(iseuil,irvb_palette,zseuil,clunite)
else
  !
  !-------------------------------------------------
  ! Cas général. Palette continue.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Etendue verticale de la palette.
  !-------------------------------------------------
  !
  rlxpal=0.05*rx_legcour
  rlypal=0.80*rlyt
  !
  !-------------------------------------------------
  ! Coin haut-gauche de la palette.
  !-------------------------------------------------
  !
  if(trim(cgformat) == 'HAS' .or. trim(cgformat) == 'HAA') then
    !
    !-------------------------------------------------
    ! Cas particulier des tracés type Hayashi. Besoin de décaler la légende à droite pour coter les jours sur l'axe de droite.
    !-------------------------------------------------
    !
    rxpal=rxt+rlxt+0.50*rx_legcour
  else
    !
    !-------------------------------------------------
    ! Cas général.
    !-------------------------------------------------
    !
    rxpal=rxt+rlxt+0.25*rx_legcour
  endif
  rypal=ryt+0.3*(rlyt-rlypal)
  !
  !-------------------------------------------------
  ! Rectangle coloré de la palette.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(a)') ' '
  write(nulsvg,fmt='(a)') '<!-- Rectangles colorés de la légende de palette de couleurs. -->'
  ipal=300
  do jpal=0,ipal
    zfrac=real(jpal)/real(ipal)
    if(cdpal(1:4) /= 'AUTO') then
      !
      !-------------------------------------------------
      ! L'utilisateur ne veut pas une palette automatique.
      ! C'est qu'il a choisi une palette telle ARC-EN-CIEL,
      ! et qu'il la veut continue.
      !-------------------------------------------------
      !
      if(trim(cg_sens_palette) /= 'direct') then
        !
        !-------------------------------------------------
        ! L'utilisateur veut un sens de palette inverse.
        !-------------------------------------------------
        !
        zfrac=1.-zfrac
      endif
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
      if(trim(cgtyppal) == 'LIN') then
        zval=rcmin+(rcmax-rcmin)*zfrac
      else
        if(rcmax < 0. .or. rcmin <= 0.) then
          write(*,fmt=*)
          write(*,fmt=*) 'dd2gr/svg_legende/ERREUR: pour la palette de couleur logarithmique le min des données ne saurait être négatif !...'
          write(*,fmt=*) 'rcmin = ',rcmin
          write(*,fmt=*) 'rcmax = ',rcmax
          call exit(1)
        endif
        zval=rcmin*(rcmax/rcmin)**zfrac
      endif
      if(trim(cg_sens_palette) /= 'direct') then
        !
        !-------------------------------------------------
        ! L'utilisateur veut un sens de palette inverse.
        ! On opère une symétrie versus (pmin+pmax)/2.
        !-------------------------------------------------
        !
        if(trim(cgtyppal) == 'LIN') then
          zval=rcmin+rcmax-zval
        else
          zval=rcmin*rcmax/zval
        endif
      endif
      if(lgdebu) write(*,fmt=*) 'svg_legende: rcmin,rcmax=',rcmin,rcmax
      !
      !-------------------------------------------------
      ! On est dans le cas où cdpal(1:4)='AUTO'. En ce cas cltype et clabs ne
      ! seront pas utilisées par img_pal.
      !-------------------------------------------------
      !
      cltype='gol'
      clabs='gol'
      call img_pal(cdpal,cltype,clabs,zval,irvb)
      if(lgdebu) write(*,fmt=*) 'svg_legende: ',jpal,zfrac,zval,irvb,trim(cdpal)
    endif
    !
    !-------------------------------------------------
    ! Pour remplir le rectangle de la légende de palette colorée
    ! on crée des rectangles colorés dont la couleur est tabulée 
    ! entre jpal points de passage (ceux de la boucle sur jpal ci-dessus).
    !-------------------------------------------------
    !
    !-------------------------------------------------
    ! Définition du pavé coloré en SVG.
    !-------------------------------------------------
    !
    zwidth_bord_pave=3.00e-4*rlxsvg
    zypave=rypal+rlypal-rlypal/real(ipal+1)*real(jpal+1)
    iylpave=nint(rlypal/real(ipal+1)+1.)
    if(jpal == ipal) iylpave=iylpave-1
    write(clrect,fmt='(3(a,f8.2),4(a,i3.3),a)') '<rect x="',rxpal,'" y="',zypave,'" width="',rlxpal &
    &,'" height="',iylpave,'" style="fill:rgb(',irvb(1),',',irvb(2),',',irvb(3),')" />'
    clrect=cl_nettoie_blancs(clrect)
    write(nulsvg,fmt='(a)') trim(clrect)
  enddo
  !
  !-------------------------------------------------
  ! On crée un rectangle noir au bord de la légende.
  !-------------------------------------------------
  !
  ilypal=nint(rlypal+1.)
  write(clrect,fmt='(3(a,f8.2),4(a,i3.3),a)') '<rect x="',rxpal,'" y="',rypal,'" width="',rlxpal &
  &,'" height="',ilypal,'" style="fill:none;stroke: #000000; stroke-width: 1;" />'
  clrect=cl_nettoie_blancs(clrect)
  write(nulsvg,fmt='(a)') trim(clrect)
  !
  !-------------------------------------------------
  ! Ecriture des nombres le long de cette palette colorée.
  !-------------------------------------------------
  !
  zmin=rcmin
  zmax=rcmax
  if(lgdebu) write(*,fmt=*) '----------------------------------------------------'
  if(lgdebu) write(*,fmt=*) 'Axe de la légende de palette colorée.'
  if(lgdebu) write(*,fmt=*) ' '
  if(trim(cgtyppal) == 'LIN') then
    !
    ! Palette de couleurs linéaire.
    !
    call lega(zmin,zmax,rindef,iprinc,zprinc,isec,zsec) ! axe de la légende de palette colorée.
    call tralps(iprinc,zprinc,isec,zsec,'L') ! tracé des lignes principales et secondaires de l'axe de légende.
  else
    !
    ! Palette de couleurs logarithmique.
    !
    call svg_axe_log(zmin,zmax,'L')
  endif
endif
end
