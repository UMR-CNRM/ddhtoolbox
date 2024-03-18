subroutine img_lec_carac(kul,cdlec,koctets)
! --------------------------------------------------------------
! Lecture de n octets d'un fichier sur une chaîne de caractères.
! --------------------------------------------------------------
! Sujet:
!
! On lit ici sur un fichier formatté, indépendamment
! des fins de ligne. La fin de lecture intervient
! si on est en fin de fichier ou en fin de chaîne.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kul unité logique du fichier d'entrée.
! En sortie:
! cdlec chaîne sur laquelle ont été portés les octets lus.
! koctets nombre d'octets lus.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
character*(*) cdlec
integer(kind=4) :: kul,ipos,ios,isize,koctets
ipos=1
do
  read(kul,fmt='(a)',iostat=ios,size=isize,advance='no') cdlec(ipos:len(cdlec))
  if(ios == -2) then
    !
    ! -------------------------------------------------
    ! Fin de ligne.
    ! -------------------------------------------------
    !
    cdlec(ipos+isize:ipos+isize)=char(10)
    ipos=ipos+isize+1
  elseif(ios == -1) then
    !
    ! -------------------------------------------------
    ! Fin de fichier.
    ! -------------------------------------------------
    !
    koctets=ipos+isize-1
    return
  elseif(ios == 0) then
    !
    ! -------------------------------------------------
    ! Fin	de chaîne.
    ! -------------------------------------------------
    !
    koctets=len(cdlec)
    return
  else
    print*,'img_lec_carac/ERREUR: réponse inconnue ios=',ios
    call exit(1)
  endif
enddo
end
