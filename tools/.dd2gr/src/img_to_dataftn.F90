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
#include"implicit_r8i4.h"
!
integer(kind=4) :: kx,ky,krvb(3,kx,ky),inum,jx,jy,jcoul
character*(*) cdficsor
character*400 clficsor
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
