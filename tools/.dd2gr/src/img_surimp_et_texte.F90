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
#include"implicit_r8i4.h"
integer(kind=4), parameter :: jpx=600
integer(kind=4), parameter :: jpy=300
integer(kind=4) :: irvb(3,jpx,jpy),jx,jy,iloc(3),ityps(4)
integer(kind=4) :: irvb_fond_texte(3)
integer(kind=4) :: irvb_pp_texte(3)
character*400 clfic,clfic_ppm,cltexte
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
