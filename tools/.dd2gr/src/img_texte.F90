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
#include"implicit_r8i4.h"
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
#include"implicit_r8i4.h"
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
