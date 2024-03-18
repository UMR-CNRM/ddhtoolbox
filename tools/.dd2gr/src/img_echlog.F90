subroutine img_echlog(pmax,klog)
! --------------------------------------------------------------
! **** *echlog*  Puissance de 10 par laquelle multiplier le réel pmax pour
! avoir un ordre de grandeur courant.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2013-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   pmax: réel à étudier.
!
! En sortie:
!  klog: puissance de 10.
! --------------------------------------------------------------
!
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"implicit_r8i4.h"
if(abs(pmax) > 1.e5 .or. abs(pmax) < 1.e-2) then
  !
  !-------------------------------------------------
  ! On crée une échelle d'un facteur de 10, pour que les mantisses cotées soient
  ! de magnitude courante.
  !-------------------------------------------------
  !
  klog=-nint(log(max(1.e-3,pmax))/log(10.)-0.5)+1
else
  klog=0
endif
end
