function zerolin(kcount,pval1,pval2,pinterm)
! --------------------------------------------------------------
! **** ** Zéro d'une fonction linéaire.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   Une droite passe par les valeurs de Y pval1 et pval2. En un certain X elle vaut pfrac.
! En sortie:
!   zerolin fraction de la distance en X à laquelle la droite vaut pinterm.
! --------------------------------------------------------------
#include"implicit_r8i4.h"
if(pval1 == pval2) then
  zerolin=0.5
else
  zerolin=(pinterm-pval1)/(pval2-pval1)
endif
if(zerolin > 1. .or. zerolin < 0.) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/ERREUR: sortie du rectangle !...'
  write(*,fmt=*) kcount,pval1,pval2,pinterm,zerolin
  write(*,fmt=*)
  call exit(1)
endif
end
