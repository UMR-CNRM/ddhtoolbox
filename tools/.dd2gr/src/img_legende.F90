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
!	kpixy: taille en pixels Y de l'image-légende.
! En sortie:
!	krvb_leg image-légende.
! --------------------------------------------------------------
!
!
use parametres, only : cgpalspec,npalspec
#include"implicit_r8i4.h"
!
character*(*) cdpal
real(kind=8) :: pxmin,pxmax,zfrac
integer(kind=4) :: kpixx,kpixy,krvb_leg(3,kpixx,kpixy),irvb_loc(3),jx,jy,ipixlegx,jcoul
integer(kind=4) :: jval,inval
character*400 cltexte
integer(kind=4) :: ifonte
integer(kind=4) :: iloc(3)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
integer(kind=4) :: knbprint
real(kind=8) :: zval
real(kind=8) :: zopac_fond_texte
real(kind=8) :: zopac_pp_texte
real(kind=8), allocatable :: zseuil(:)
integer(kind=4), allocatable :: irvb_palette(:,:)
character(len=180) clmot(40)
!
!-------------------------------------------------
! Initialisation du fond à gris.
!-------------------------------------------------
!
krvb_leg=223
!
!-------------------------------------------------
! Taille de la zone colorée de la légende.
!-------------------------------------------------
!
ipixlegx=kpixx/18
ipixlegy=max(2,nint(real(kpixy)*0.9))
ibord=kpixx/8 ! taille en X et Y du décalage entre le coin bas gauche de la zone colorée et le bord de l'image-légende.
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
    write(*,fmt=*) 'img_legende/ERREUR: palette spécifiée par l''utilisateur, or aucune spécification de plages de réels effectuée.'
    call exit(1)    
  endif
  !
  !-------------------------------------------------
  ! Premier plan et fond..
  !-------------------------------------------------
  !
  irvb_fond_texte=223 ! fond gris.
  irvb_pp_texte=0 ! premier plan noir.
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
  ! Unité: mise à blanc car indiquée plutôt sur le titre.
  !-------------------------------------------------
  !
  clunite=' '
  !
  !-------------------------------------------------
  ! Appel à la routine de tracé de la légende discrète.
  !-------------------------------------------------
  !
  call img_legende_discrete(kpixx,kpixy,irvb_fond_texte,irvb_pp_texte,iseuil &
  &,irvb_palette,zseuil,clunite,krvb_leg)
else
  !
  !-------------------------------------------------
  ! Cas général. Palette continue.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Ecriture des pixels colorés.
  !-------------------------------------------------
  !
  do jx=1,ipixlegx
    do jy=1,ipixlegy
      if(cdpal(1:4) /= 'AUTO') then
        zfrac=real(jy-1)/real(ipixlegy-1)
      else
        read(cdpal(5:),fmt=*) zec,zmin,zmax
        zfrac=zmin+(zmax-zmin)*real(jy-1)/real(ipixlegy-1)
      endif
      call img_pal(cdpal,'CONT','FRAC',zfrac,irvb_loc)
      do jcoul=1,3
        krvb_leg(jcoul,jx+ibord,kpixy-ibord-jy)=irvb_loc(jcoul)
      enddo
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Ecriture du texte.
  !-------------------------------------------------
  !
  if(knbprint == 0) then
    inval=max(2,ipixlegy/40) ! nombre de valeurs réelles à pointer.
    !inval=9 ! forçage ad-hoc du nombre de valeurs réelles à pointer.
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
    !write(cltexte,fmt='(g16.3)') zval
    call reecar(zval,-1,3,cltexte,iltexte)
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
    iloc(2)=ibord+ipixlegx+nint(real(kpixx-2*ibord-ipixlegx)*0.5)
    iloc(3)=kpixy-ibord-nint(real(ipixlegy)*(zval-pxmin)/(pxmax-pxmin))
    call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
      &,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,kpixx,kpixy,krvb_leg)
  enddo
endif
end
