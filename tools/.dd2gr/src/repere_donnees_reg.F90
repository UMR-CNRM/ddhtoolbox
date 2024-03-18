subroutine repere_donnees_reg(px,py,kdta,kpasx,kpasy)
! --------------------------------------------------------------
! **** ** Si les données d'entrée sont régulières on retourne leurs pas en X et Y.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-08, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   px,py,kdta
! En sortie:
!   kpasx,kpasy
! --------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: px(kdta)
real(kind=8), intent(in) :: py(kdta)
integer(kind=4), intent(in) :: kdta
integer(kind=4), intent(out) :: kpasx
integer(kind=4), intent(out) :: kpasy
kpasx=-999
kpasy=-999
if(kdta >= 9) then
  zecx1=px(2)-px(1)
  zecx2=px(3)-px(2)
  zecy1=py(2)-py(1)
  zecy2=py(3)-py(2)
  if(llegale(zecx1,zecx2)) then
    !
    !-------------------------------------------------
    ! La grille semble régulière en X.
    !-------------------------------------------------
    !
    ipasx=-999
    do jdta=2,kdta
      if(.not. llegale(px(jdta)-px(jdta-1),zecx1)) then
        ipasx=jdta-1
        exit
      endif
    enddo
    if(modulo(kdta,ipasx) == 0) then
      !
      !-------------------------------------------------
      ! Tous les écarts de X sont égaux jusqu'à ipasx, et en plus ipasx divise le nombre de données.
      ! On suppose que les données sont régulières.
      !-------------------------------------------------
      !
      kpasx=ipasx
      kpasy=kdta/kpasx
      if(max(kpasx,kpasy) / min(kpasx,kpasy) < 20.) then
        write(*,fmt=*) '  Les données d''entrée semblent régulières en X et la longueur de la première ligne divise le nombre de données : ',kpasx,' x ',kpasy
      else
        write(*,fmt=*) '  Les données d''entrée semblent régulières en X et la longueur de la première ligne divise le nombre de données, mais le rapport d''aspect est trop différent de 1 : ',kpasx,' x ',kpasy
        kpasx=-999
        kpasy=-999
        kpassx=-999
        kpassy=-999
      endif
    else
      write(*,fmt=*) '  Les premières lignes de données sont régulières en X mais le nombre total de points ',kdta,' n''est pas divisible par le pas en X ',ipasx
      kpassx=-999
      kpassy=-999
    endif
  elseif(llegale(zecy1,zecy2)) then
    !
    !-------------------------------------------------
    ! La grille semble régulière en Y.
    !-------------------------------------------------
    !
    ipasy=-999
    do jdta=2,kdta
      if(.not. llegale(py(jdta)-py(jdta-1),zecy1)) then
        ipasy=jdta-1
        exit
      endif
    enddo
    if(modulo(kdta,ipasy) == 0) then
      !
      !-------------------------------------------------
      ! Tous les écarts de y sont égaux jusqu'à ipasy, et en plus ipasy divise le nombre de données.
      ! On suppose que les données sont régulières.
      !-------------------------------------------------
      !
      kpasy=ipasy
      kpasx=kdta/kpasy
      if(max(kpasx,kpasy) / min(kpasx,kpasy) < 20.) then
        write(*,fmt=*) '  Les données d''entrée semblent régulières en Y et la longueur de la première ligne divise le nombre de données : ',kpasx,' x ',kpasy
      else
        write(*,fmt=*) '  Les données d''entrée semblent régulières en Y et la longueur de la première ligne divise le nombre de données, mais le rapport d''aspect est trop différent de 1 : ',kpasx,' x ',kpasy
        kpasx=-999
        kpasy=-999
        kpassx=-999
        kpassy=-999
      endif
    else
      write(*,fmt=*) '  Les premières lignes de données sont régulières en Y mais le nombre total de points ',kdta,' n''est pas divisible par le pas en Y ',ipasy
      kpassx=-999
      kpassy=-999
    endif
  else
    kpasx=-999
    kpasy=-999
    write(*,fmt=*) '  Les données d''entrée sont irrégulières en X et Y.'
  endif
else ! kdta
  kpasx=-999
  kpasy=-999
  write(*,fmt=*) '  Peu de données. Test de grille régulière non effectué.'
endif ! kdta
end
function llegale(px,py)
! --------------------------------------------------------------
! **** ** Test de proximité relative de 2 réels.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-08, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   px,py,kdta
! En sortie:
!   kpasx,kpasy
! --------------------------------------------------------------
#include"implicit_r8i4.h"
logical :: llegale
real(kind=8), intent(in) :: px,py
if(py == 0.) then
  !
  !-------------------------------------------------
  ! On ne veut pas d'écarts nuls, donc si py==0. on sort direct en .false..
  !-------------------------------------------------
  !
  llegale=.false.
else
  zrap=abs(px/py-1._8)
  if(zrap < 1.e-2_8) then
    llegale=.true.
  else
    llegale=.false.
  endif
endif
end
