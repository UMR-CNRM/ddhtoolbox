subroutine calpaprsf(kdim,pvah,pvbh,pspt0,ptmp)
! --------------------------------------------------------------
! **** *calpaprs* Calcul de la pression "full-levels" à partir des coordonnées A et B.
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
!
!-------------------------------------------------
! Attention: ptmp est surdimensionné dans cette routine:
! seules les valeurs de 1 à (kdim-1) vont être initialisées.
!-------------------------------------------------
!
do jdim=1,kdim-1
  ptmp(jdim)=0.5*((pvah(jdim)+pvbh(jdim)*pspt0) &
&   +(pvah(jdim+1)+pvbh(jdim+1)*pspt0))
enddo
end
