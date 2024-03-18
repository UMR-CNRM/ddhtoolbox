module imgyom
! --------------------------------------------------------------
! Module des palettes de couleur.
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
save
!
integer(kind=4), parameter :: jppal=22 ! nombre de palettes.
integer(kind=4), parameter :: jpcoul=80 !nombre max. de couleurs par palette.
integer(kind=4) :: ncoul(jppal) ! nombre de couleurs de chaque palette.
real(kind=8) :: rvbpal(3,jpcoul,jppal) ! valeurs RVB de chaque couleur.
character(len=100) :: cgcoulc(jppal) ! nom en clair de la palette.
!
end module
