subroutine calpaprs(kdim,pvah,pvbh,pspt0,ptmp)
! --------------------------------------------------------------
! **** *calpaprs* Calcul de la pression "half-levels" à partir des coordonnées A et B.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kdim,pvah,pvbh,pspt0
! En sortie:
! ptmp
! --------------------------------------------------------------
real(kind=8) :: pvah(kdim)
real(kind=8) :: pvbh(kdim)
real(kind=8) :: ptmp(kdim)
do jdim=1,kdim
  ptmp(jdim)=pvah(jdim)+pvbh(jdim)*pspt0
enddo
end
