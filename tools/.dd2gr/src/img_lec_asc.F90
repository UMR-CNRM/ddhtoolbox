subroutine img_lec_asc(kul,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_lec_asc* Lecture des triplets RVB d'une image ppm ascii.
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
integer(kind=4) :: kx,ky,krvb(3,kx,ky)
integer(kind=4) :: jx,jy,jcoul
character*1 cl1
integer(kind=4) :: icoul
integer(kind=4) :: ientier
integer(kind=4) :: ios
integer(kind=4) :: ipos
integer(kind=4) :: ix
integer(kind=4) :: iy
integer(kind=4) :: kul
integer(kind=4) :: igeom
logical ll_entier
ll_entier=.false.
ipos=0
do
  read(kul,fmt='(a)',iostat=ios,advance='no') cl1
  if(ios == -1) then
    !
    ! -------------------------------------------------
    ! Fin de fichier.
    ! -------------------------------------------------
    !
    cl1='u'
  elseif(ios == -2) then
    !
    ! -------------------------------------------------
    ! Fin de ligne.
    ! -------------------------------------------------
    !
    cl1=char(10)
  elseif(ios == 0) then
    !
    ! -------------------------------------------------
    ! Cas général.
    ! -------------------------------------------------
    !
  else
    !
    ! -------------------------------------------------
    ! Cas non prévu.
    ! -------------------------------------------------
    !
    print*,'Code réponse en lecture non prévu: ',ios
    call exit(1)
  endif
  if(cl1 >= '0'.and.cl1 <= '9') then
    !
    ! -------------------------------------------------
    ! On est dans un entier.
    ! -------------------------------------------------
    !
    if(ll_entier) then
      !
      ! -------------------------------------------------
      ! On était dans un entier.
      ! -------------------------------------------------
      !
      ientier=10*ientier+ichar(cl1)-ichar('0')
    else
      !
      ! -------------------------------------------------
      ! On était hors d'un entier.
      ! -------------------------------------------------
      !
      ientier=ichar(cl1)-ichar('0')
    endif
    ll_entier=.true.
  else
    !
    ! -------------------------------------------------
    ! On est hors d'un entier.
    ! -------------------------------------------------
    !
    if(ll_entier) then
      !
      ! -------------------------------------------------
      ! On était dans un entier.
      ! -------------------------------------------------
      !
      ipos=ipos+1
      icoul=mod(ipos-1,3)+1
      igeom=(ipos-1)/3
      ix=mod(igeom,kx)+1
      iy=igeom/kx+1
      krvb(icoul,ix,iy)=ientier
    else
      !
      ! -------------------------------------------------
      ! On était hors d'un entier.
      ! Rien à faire.
      ! -------------------------------------------------
      !
    endif
    ll_entier=.false.
  endif
  if(cl1 == 'u') exit
enddo
if(ipos /= 3*kx*ky) then
  print*,'img_lec_asc/ERREUR: nombre erroné d''entiers lus sur le ppm!...'
  print*,kx,ky,ipos
  call exit(1)
endif
end
