function zmodulo(px,py)
! --------------------------------------------------------------
! **** *zmodulo* fonction modulo périodique sur l'ensemble des reels.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
REAL(KIND=8) :: ZMODULO
REAL(KIND=8) :: PX
REAL(KIND=8) :: PY

zmodulo=mod(px,py)
if(zmodulo < 0) zmodulo=zmodulo+py
end
function zmodyx(px,py,pz)
! --------------------------------------------------------------
! **** *zmodyx* amène px dans l'intervalle [py,pz[ modulo (pz-py).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! i.e. zmodyx sera le réel tel que
! py <= zmodyx < pz, et px-zmodyx=k*(pz-py.)
! --------------------------------------------------------------
implicit none
REAL(KIND=8) :: ZMODYX
REAL(KIND=8) :: ZMODULO
REAL(KIND=8) :: PX
REAL(KIND=8) :: PY
REAL(KIND=8) :: PZ
zmodyx=py+zmodulo(px-py,pz-py)
end
function imodulo(kx,ky)
! --------------------------------------------------------------
! **** *imodulo* fonction modulo périodique sur l'ensemble des entiers.
! --------------------------------------------------------------
! Sujet:
! 	Cette fonction pallie le manque, en f77, 
! 	d'une fonction modulo périodique sur l'ensemble des entiers.
! 	En.F90, ce problème a été résolu par la fonction modulo.
! 	Il suffit alors d'appeler modulo et non imodulo.
! Limitations: le deuxième argument ky est supposé > 0.
! 	On n'a pas fait ici l'analyse de l'adéquation imodulo <> modulo
! 	pour les ky <= 0, pour lesquels la fonction mathématique
! 	n'est pas usuellement définie. On ne traite donc l'adéquation 
! 	que pour tous les kx relatifs, à ky > 0.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: IMODULO
INTEGER(KIND=4) :: KX
INTEGER(KIND=4) :: KY

imodulo=mod(kx,ky)
if(imodulo < 0) imodulo=imodulo+ky
end
