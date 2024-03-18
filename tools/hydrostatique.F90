subroutine phidept(klev,pp,pt,pphi,pphif)
! --------------------------------------------------------------
! **** *phidept* Calcul du géopotentiel à partir de la pression et de la température.
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
! klev nombre de niveaux verticaux.
! pp pression (Pa) niveaux de flux.
! pt température (K) niveaux de variables.
! En sortie:
! pphi géopotentiel (J/kg) niveaux de flux.
! pphif géopotentiel (J/kg) niveaux de variables.
! --------------------------------------------------------------
real pp(klev+1)
real pt(klev)
real pphi(klev+1)
real pphif(klev)
!
!-------------------------------------------------
! Calcul du géopotentiel niveaux de flux.
!-------------------------------------------------
!
zr=287.059673
pphi(klev+1)=0.
do jlev=klev,2,-1
  pphi(jlev)=pphi(jlev+1)-zr*pt(jlev)*log(pp(jlev)/pp(jlev+1))
enddo
pphi(1)=pphi(2)+pphi(2)-pphi(3)
!
!-------------------------------------------------
! Calcul du géopotentiel niveaux de variables.
!-------------------------------------------------
!
do jlev=1,klev
  pphif(jlev)=0.5*(pphi(jlev)+pphi(jlev+1))
enddo
end
