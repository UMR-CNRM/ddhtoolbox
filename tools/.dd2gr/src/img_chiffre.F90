subroutine img_chiffre(kx,ky,krvb,kx1,ky1,kx2,ky2,kchiffre)
! --------------------------------------------------------------
! **** *img_chiffre* retourne un chiffre (entre 0 et 9) à partir d'une image-pixel du dessin de ce chiffre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2006-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   krvb(3,kx,ky): image RVB d'entrée.
!   kx1, kx2, ky1, ky2: coordonnées du rectangle au sein duquel chercher à décrypter un chiffre.
! En sortie:
!   kchiffre: chiffre lu.
! --------------------------------------------------------------
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"implicit_r8i4.h"
integer(kind=4), intent(in) :: kx,ky
integer(kind=4), intent(in) :: krvb(3,kx,ky),kx1,kx2,ky1,ky2
integer(kind=4), intent(out) :: kchiffre
integer(kind=4), parameter :: jpdata=100
real(kind=8) :: zxfrac(0:9,jpdata), zyfrac(0:9,jpdata), zscore(0:9), znb(0:9)
integer(kind=4), allocatable :: irvbtmp(:,:,:)
!
!-------------------------------------------------
! Seuil en niveau de gris (entre 0 et 255) séparant le fond du pp.
!-------------------------------------------------
!
iseuil=200
!
!-------------------------------------------------
! On cadre le chiffre au plus serré sur l'image:
! on cherche quel plus petit cadre contient tous les points
! de l'image supérieurs au seuil.
!-------------------------------------------------
!
ix1=kx2
iy1=ky2
ix2=0
iy2=0
do jx=kx1,kx2
  do jy=ky1,ky2
    irvb=(krvb(1,jx,jy)+krvb(2,jx,jy)+krvb(3,jx,jy))/3
    if(irvb > iseuil) then
      ix1=min(ix1,jx)
      ix2=max(ix2,jx)
      iy1=min(iy1,jy)
      iy2=max(iy2,jy)
    endif
  enddo
enddo
!
!-------------------------------------------------
! Ecriture de l'image sur un fichier:
! permet d'écrire sous forme d'un fichier-image
! la partie de l'image ne contenant que le chiffre;
! utile pour pixelliser une fonte de chiffre, 
! i.e. pour générer les data des chiffres 0 à 9 ci-dessous.
!-------------------------------------------------
!
!iecrx=ix2-ix1+1
!iecry=iy2-iy1+1
!allocate(irvbtmp(3,iecrx,iecry))
!irvbtmp=0
!do jx=1,iecrx
!	do jy=1,iecry
!		do jc=1,3
!			irvbtmp(jc,jx,jy)=krvb(jc,jx-1+ix1,jy-1+iy1)
!		enddo
!	enddo
!enddo
!write(clfic,fmt='(a,i3.3,a)') 'F',ix1,'.tmp.ppm'
!call img_ecr(clfic,iecrx,iecry,irvbtmp)
!
!-------------------------------------------------
! Data définissant la forme des chiffres.
!-------------------------------------------------
!
imanq=999
zxfrac=imanq
zyfrac=imanq
!
!-------------------------------------------------
! Chiffre 0.
!-------------------------------------------------
!
idata=0 ; ichiffre=0
idata=idata+1 ; zxfrac(ichiffre,idata)=0.45 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.55 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.55 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.45 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.89
!
!-------------------------------------------------
! Chiffre 1.
!-------------------------------------------------
!
idata=0 ; ichiffre=1
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.81 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.69 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.56 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.44 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.31 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.19 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.06 ; zyfrac(ichiffre,idata)=0.39
!
!-------------------------------------------------
! Chiffre 2.
!-------------------------------------------------
!
idata=0 ; ichiffre=2
idata=idata+1 ; zxfrac(ichiffre,idata)=0.05 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.95 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.25
!
!-------------------------------------------------
! Chiffre 3.
!-------------------------------------------------
!
idata=0 ; ichiffre=3
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.25
!
!-------------------------------------------------
! Chiffre 4.
!-------------------------------------------------
!
idata=0 ; ichiffre=4
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.63 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.54 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.46 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.38 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.29 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.21 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.13 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.04 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.04 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.04 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.13 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.21 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.29 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.38 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.46 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.54 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.63 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.71 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.79 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.88 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.96 ; zyfrac(ichiffre,idata)=0.68
!
!-------------------------------------------------
! Chiffre 5.
!-------------------------------------------------
!
idata=0 ; ichiffre=5
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.04
!
!-------------------------------------------------
! Chiffre 6.
!-------------------------------------------------
!
idata=0 ; ichiffre=6
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.95 ; zyfrac(ichiffre,idata)=0.25
!
!-------------------------------------------------
! Chiffre 7.
!-------------------------------------------------
!
idata=0 ; ichiffre=7
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.05 ; zyfrac(ichiffre,idata)=0.04
!
!-------------------------------------------------
! Chiffre 8.
!-------------------------------------------------
!
idata=0 ; ichiffre=8
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.45 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.55 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.96
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.85 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.75 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.55 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.45 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.45 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.55 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.65 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.15 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.25 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.35 ; zyfrac(ichiffre,idata)=0.89
!
!-------------------------------------------------
! Chiffre 9.
!-------------------------------------------------
!
idata=0 ; ichiffre=9
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.89
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.82
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.75
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.68
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.61
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.04
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.11
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.18
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.25
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.32
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.39
idata=idata+1 ; zxfrac(ichiffre,idata)=0.14 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.23 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.32 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.41 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.50 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.59 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.68 ; zyfrac(ichiffre,idata)=0.54
idata=idata+1 ; zxfrac(ichiffre,idata)=0.77 ; zyfrac(ichiffre,idata)=0.46
idata=idata+1 ; zxfrac(ichiffre,idata)=0.86 ; zyfrac(ichiffre,idata)=0.39
!
!-------------------------------------------------
! Repérage du chiffre.
!-------------------------------------------------
!
do jchiffre=0,9
  do jdata=1,jpdata
    if(zxfrac(jchiffre,jdata) /= imanq) then
      ilocx=max(ix1,min(ix2,nint(real(ix1)+real(ix2-ix1)*zxfrac(jchiffre,jdata))))
      ilocy=max(iy1,min(iy2,nint(real(iy1)+real(iy2-iy1)*zyfrac(jchiffre,jdata))))
      irvb=(krvb(1,ilocx,ilocy)+krvb(2,ilocx,ilocy)+krvb(3,ilocx,ilocy))/3
      if(irvb > iseuil) then
        zscore(jchiffre)=zscore(jchiffre)+1.
      endif
      znb(jchiffre)=znb(jchiffre)+1.
    endif
  enddo
enddo
!
!-------------------------------------------------
! Calcul du score de chaque chiffre.
!-------------------------------------------------
!
zscorex=0.
kchiffre=4
do jchiffre=0,9
  if(znb(jchiffre) > 0.5) then
    zscore(jchiffre)=zscore(jchiffre)/znb(jchiffre)
  else
    zscore(jchiffre)=0.
  endif
  if(zscore(jchiffre) > zscorex) then
    kchiffre=jchiffre
    zscorex=zscore(jchiffre)
  endif
enddo
end
