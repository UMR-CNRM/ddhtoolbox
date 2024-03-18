function indice_palette(cdpal)
! --------------------------------------------------------------
! Retourne le n° de palette correspondant à un nom de palette en clair donné.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!    cdpal type de palette: AQUABLUE, TMER, etc...
! En sortie:
!    indice_palette : n° de palette.
! --------------------------------------------------------------
!
!
use imgyom
use parametres
!
#include"implicit_r8i4.h"
!
character(len=*), intent(in) :: cdpal
integer(kind=4) :: indice_palette
indice_palette=-999
do jpal=1,jppal
  if(trim(cgcoulc(jpal)) == trim(cdpal)) then
    indice_palette=jpal
  endif
enddo
if(indice_palette == -999) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/indice_palette/ERREUR: palette inconnue !...'
  write(*,fmt=*) trim(cdpal)
  call exit(1)
endif
end
