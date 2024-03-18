program lec
! --------------------------------------------------------------
! **** *LEC* Programme lisant un fichier LFA.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
integer(kind=4) iversion
character*200 clfic
!
! -------------------------------------------------
! Ouverture du fichier d'entree.
! -------------------------------------------------
!
clfic='DZ.lfa' ! nom du fichier d'entree.
open(1,file=clfic,form='unformatted')
!
! -------------------------------------------------
! En-tete du fichier.
! -------------------------------------------------
!
read(1) iversion
write(*,fmt=*) 'iversion=',iversion
end
