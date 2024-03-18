subroutine img_fonte_9x15bold_taille(cdtexte,kx,ky)
! --------------------------------------------------------------
! **** *img_fonte_9x15bold_taille* Détermination de la taille de l'image-texte.
! Elle est fonction du nombre de caractères et de la fonte utilisée.
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
! cdtexte: texte à écrire
! En sortie:
! kx,ky
! --------------------------------------------------------------

!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"implicit_r8i4.h"

character*(*) cdtexte
integer(kind=4) :: kx,ky

kx=9*(len_trim(cdtexte)+2)
ky=10+10
end
