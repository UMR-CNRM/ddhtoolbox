subroutine img_lec_raw(cdfic,kx,ky,krvb)
! --------------------------------------------------------------
! Lecture des triplets RVB d'une image ppm raw.
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
integer(kind=4) :: kx,ky,krvb(3,kx,ky),itail,ix,iy,ival,iligne,icar1,icar2
integer(kind=4) :: jx,jy,jcoul,ic,jc,ioctets,jcar
character*(3*kx*ky) clrvb
integer(kind=4) :: kul
character*1, allocatable :: clcar(:)
character*(*) cdfic
character*400 cltype,clc
!
!-------------------------------------------------
! Lecture du fichier sur une chaîne de caractères.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Obtention de la taille du fichier en octets.
!-------------------------------------------------
!
call io_taille(cdfic,itail)
!print*,'			',itail,' octets'
!
!-------------------------------------------------
! Allocation du tableau de caractères.
!-------------------------------------------------
!
allocate(clcar(itail))
!
!-------------------------------------------------
! Lecture.
!-------------------------------------------------
!
call io_lec(cdfic,itail,clcar)
!
!-------------------------------------------------
! On va passer la zone d'autodocumentation,
! pour atteindre la zone des triplets RVB.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Mise à indef.
!-------------------------------------------------
!
cltype='IIII'
ix=-999
iy=-999
ival=-999
!
!-------------------------------------------------
! Lecture de l'en-tête.
!-------------------------------------------------
!
iligne=0
do
  iligne=iligne+1
  call io_ligne(iligne,itail,clcar,icar1,icar2,clc)
  if(clc(1:1) == '#') then
    !
    ! -------------------------------------------------
    ! Ligne de commentaire.
    ! -------------------------------------------------
    !
  elseif(clc == ' ') then
    !
    ! -------------------------------------------------
    ! Ligne vide.
    ! -------------------------------------------------
    !
  else
    if(cltype(1:4) == 'IIII') then
      cltype=clc  ! P3, P5, etc...
    elseif(ix == -999) then
      read(clc,fmt=*) ix,iy  ! taille X et Y.
    elseif(ival == -999) then
      read(clc,fmt=*) ival  ! 255.
    endif
  endif
  if(ival /= -999) exit
enddo
!
!-------------------------------------------------
! Lecture de la chaîne sur des entiers.
!-------------------------------------------------
!
ic=icar2+2
do jy=1,ky
  do jx=1,kx
    do jcoul=1,3
      krvb(jcoul,jx,jy)=ichar(clcar(ic))
      ic=ic+1
    enddo
  enddo
enddo
end
