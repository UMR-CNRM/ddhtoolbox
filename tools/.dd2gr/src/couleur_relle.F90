subroutine couleur_relle(pval,pmin,pmax,pindef,cdpal,krvb_indef,krvb)
! --------------------------------------------------------------
! **** *couleur_reelle* Calcul de la couleur en RVB associée à une valeur réelle donnée.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2015-05, J.M. Piriou.
! Modifications:
!           2018-08, J.M. Piriou: échelle log de couleur.
! --------------------------------------------------------------
! En entree:
!     pval : valeur réelle dont on veut la couleur.
!     pmin, pmax : valeurs réelles extrêmes de la palette.
!     cdpal : choix de palette.
! En sortie:
!     krvb: triplet RVB de la couleur de sortie.
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
!
character(len=*), intent(in) :: cdpal
integer(kind=4), intent(in) :: krvb_indef(3)
integer(kind=4), intent(out) ::krvb(3)
character(len=180) clmot(40)
!
!-------------------------------------------------
! Couleur du pavé.
!-------------------------------------------------
!
if(abs(pval+999.99_8) < 1.e-2) then
  !
  !-------------------------------------------------
  ! Valeur de champ "-999.999" traitée en couleur "valeur manquante".
  !-------------------------------------------------
  !
  krvb(1)=krvb_indef(1)
  krvb(2)=krvb_indef(2)
  krvb(3)=krvb_indef(3)
elseif(pval /= pindef) then
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
        if(pval < zspec_seuil) then
          krvb(1)=irspec
          krvb(2)=ivspec
          krvb(3)=ibspec
          lloktrav=.true.
          exit
        endif
      elseif(trim(clmot(1)) == '>') then
        krvb(1)=irspec
        krvb(2)=ivspec
        krvb(3)=ibspec
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
      write(*,fmt=*) pval,npalspec,trim(clmot(1)),irspec,ivspec,ibspec
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
    if(trim(cgtyppal) == 'LIN') then
      !
      ! Echelle de couleurs linéaire.
      !
      zfrac=(pval-pmin)/(pmax-pmin)
      if(trim(cg_sens_palette) /= 'direct') zfrac=1.-zfrac ! L'utilisateur veut un sens de palette inverse.
      call img_pal(cdpal,'CONT','FRAC',zfrac,krvb)
    else
      !
      ! Echelle de couleurs logarithmique.
      !
      if(pval >= pmin .and. pval <= pmax) then
        zfrac=log(pval/pmin)/log(pmax/pmin)
        if(trim(cg_sens_palette) /= 'direct') zfrac=1.-zfrac ! L'utilisateur veut un sens de palette inverse.
        call img_pal(cdpal,'CONT','FRAC',zfrac,krvb)
      elseif(pval == 0.) then
        krvb(1)=255 ; krvb(2)=255 ; krvb(3)=255 ! gris.
      elseif(pval < pmin) then
        krvb(1)=132 ; krvb(2)=103 ; krvb(3)=215 ! violet moyen.
      elseif(pval > pmax) then
        krvb(1)=255 ; krvb(2)=000 ; krvb(3)=000 ! rouge.
      else
        write(*,fmt=*)
        write(*,fmt=*) 'dd2gr/couleur_relle/ERREUR: cas de pval non prévu !...'
        write(*,fmt=*) pval,pmin,pmax
        call exit(1)
      endif
    endif
  else
    !
    !-------------------------------------------------
    ! L'utilisateur veut une palette automatique.
    ! C'est une palette qui sera différente suivant 
    ! que le champ réel embrasse 0 ou non,
    ! et une palette discrète.
    !-------------------------------------------------
    !
    if(trim(cg_sens_palette) /= 'direct') then
      !
      !-------------------------------------------------
      ! L'utilisateur veut un sens de palette inverse.
      ! On opère une symétrie versus (pmin+pmax)/2.
      !-------------------------------------------------
      !
      if(trim(cgtyppal) == 'LIN') then
        !
        ! Echelle de couleurs linéaire.
        !
        pval=pmin+pmax-pval
      else
        !
        ! Echelle de couleurs logarithmique.
        !
        pval=pmin*pmax/pval
      endif
    endif
    !
    !-------------------------------------------------
    ! On est dans le cas où cdpal(1:4)='AUTO'. En ce cas cltype et clabs ne
    ! seront pas utilisées par img_pal.
    !-------------------------------------------------
    !
    cltype='gol'
    clabs='gol'
    call img_pal(cdpal,cltype,clabs,pval,krvb)
  endif
else
  !
  !-------------------------------------------------
  ! Valeur manquante.
  !-------------------------------------------------
  !
  krvb(1)=krvb_indef(1)
  krvb(2)=krvb_indef(2)
  krvb(3)=krvb_indef(3)
endif
end
