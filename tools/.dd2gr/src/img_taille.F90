subroutine img_taille(cdfic,kx,ky)
! --------------------------------------------------------------
! **** *img_taille* Lecture du nombre de pixels en X et Y d'une image ppm.
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
character*400 clc,clficppm
integer(kind=4) :: kx,ky
logical llex
!
!-------------------------------------------------
! Mise à indef.
!-------------------------------------------------
!
kx=-999
ky=-999
!
!-------------------------------------------------
! Le fichier existe-t-il bien?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llex)
if(.not.llex) then
  write(*,fmt=*) 'img/ERREUR: fichier ',cdfic(1:len_trim(cdfic)),' inexistant!...'
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
  elseif(clc(1:1) == 'P') then
    !
    ! -------------------------------------------------
    ! Ligne de déclaration de type de ppm.
    ! -------------------------------------------------
    !
  else
    if(kx == -999) then
      read(clc,fmt=*) kx,ky  ! taille X et Y.
    endif
  endif
  if(kx /= -999) exit
enddo
close(42)
end
