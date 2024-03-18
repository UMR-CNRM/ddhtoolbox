subroutine img_lec(cdfic,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_lec* Lecture des triplets RVB d'une image ppm.
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
integer(kind=4) :: kx,ky,krvb(3,kx,ky),ix,iy,ival
character*400 cltype,clc,clficppm
character*1 cl1
logical llexist
!
!-------------------------------------------------
! Le fichier existe-t-il?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llexist)
if(.not.llexist) then
  write(*,fmt=*) 'img_lec/ERREUR: le fichier image ',cdfic(1:len_trim(cdfic)),' n''existe pas!...'
  call exit(1)
endif
!
!-------------------------------------------------
! Conversion éventuelle en ppm.
!-------------------------------------------------
!
call img_2ppm(cdfic,clficppm)
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
open(42,file=clficppm,form='formatted')
do
  read(42,fmt='(a)') clc
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
! Tests de cohérence.
!-------------------------------------------------
!
if(ix /= kx.or.iy /= ky) then
  print*,'img_lec/ERREUR: taille de l''image passée en argument' &
    &,' non compatible avec la taille lue dans le fichier ' &
    &,clficppm(1:len_trim(clficppm))
  call exit(1)
endif
if(ival /= 255) then
  print*,'img_lec/ERREUR: image non codée sur 255 couleurs' &
    &,' dans le fichier ',clficppm(1:len_trim(clficppm))
  call exit(1)
endif
!
!-------------------------------------------------
! Lecture des données.
!-------------------------------------------------
!
if(cltype(1:len_trim(cltype)) == 'P3') then
  !
  ! -------------------------------------------------
  ! PPM ASCII.
  ! -------------------------------------------------
  !
  call img_lec_asc(42,ix,iy,krvb)
  close(42)
elseif(cltype(1:len_trim(cltype)) == 'P6') then
  !
  ! -------------------------------------------------
  ! PPM RAW.
  ! -------------------------------------------------
  !
  close(42)
  call img_lec_raw(clficppm,ix,iy,krvb)
elseif(cltype(1:len_trim(cltype)) == 'P2') then
  !
  ! -------------------------------------------------
  ! PGM gris ASCII.
  ! -------------------------------------------------
  !
  print*,'Type de fichier image non encore prévu: ',cltype(1:len_trim(cltype))
  close(42)
  call exit(1)
elseif(cltype(1:len_trim(cltype)) == 'P5') then
  !
  ! -------------------------------------------------
  ! PGM gris RAW.
  ! -------------------------------------------------
  !
  print*,'Type de fichier image non encore prévu: ',cltype(1:len_trim(cltype))
  close(42)
  call exit(1)
else
  print*,'img_lec/ERREUR: format d''image non reconnu: ',cltype(1:len_trim(cltype)) &
    &,' dans le fichier ',clficppm(1:len_trim(clficppm))
  close(42)
  call exit(1)
endif
end
