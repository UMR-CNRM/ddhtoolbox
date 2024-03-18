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
! i.e.zmodyx sera le réel tel que
! py <= zmodyx < pz, et px-zmodyx=k*(pz-py.)
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,j,k)
zmodyx=px-(pz-py)*nint((px-py)/(pz-py)-.5)
end
