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
#include"implicit_r8i4.h"
!
character*(*) cdfic
integer(kind=4) :: kx,ky,krvb(3,kx,ky),jx,jy,jcoul
character*400 clsuff
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
do jy=1,ky
  do jx=1,kx
    write(45,fmt='(a1,$)') char(krvb(1,jx,jy))
    write(45,fmt='(a1,$)') char(krvb(2,jx,jy))
    write(45,fmt='(a1,$)') char(krvb(3,jx,jy))
  enddo
enddo
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
