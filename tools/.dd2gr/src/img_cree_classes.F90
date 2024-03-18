subroutine cree_classes(pmin,pmax,pec,ldp0,peps,kpclas,kclas,pclas)
! --------------------------------------------------------------
! **** *cree_classes* Fourniture d'un tableau de classes à partir de min et max.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! ----------
! pmin		min
! pmax		max
! pec		écartement imposé, 0. si non fourni
! ldp0		vrai si on ne veut pas de 0 comme classe
! peps		valeur voisine de 0 permettant de l'encadrer sans passer dessus
! kpclas	dimension statique du tableau pclas
! En sortie:
! ----------
! kclas		nombre de réels écrits sur le tableau pclas
! pclas		valeurs des classes
! --------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8) pclas(kpclas)
inomc=26 ! nombre classes désirées maximal.
iclass_max=0
if(pec == 0.) then
  !
  ! L'écart n'a pas été fourni par l'utilisateur.
  ! On le calcule.
  ! Méthode: on recherche quel est l'écart
  ! sur un seul chiffre significatif,
  ! qui confère le nombre de classes le plus
  ! grand, en restant cependant inférieur à inomc.
  !
  do jnomc=1,inomc
    zdx=(pmax-pmin)/real(jnomc)
    !
    ! On arrondit l'écart à un seul chiffre significatif.
    !
    call arrr(zdx,1,zec)
    !
    ! On en déduit combien de classes il y a.
    !
    iclass=nint((pmax-pmin)/zec)
    if(iclass <= inomc .and. iclass > iclass_max) then
      pec=zec
      iclass_max=iclass
    endif
  enddo
endif
!
! Calcul des min et max de tracé en fonction des min et max réels.
! On veut des multiples de l'écart demandé.
!
zmint=pec*nint(pmin/pec+0.5)
if(zmint > pmin+0.9*pec) zmint=zmint-pec
zmaxt=pec*nint(pmax/pec-0.5)
if(zmaxt < pmax-pec*.9) zmaxt=zmaxt+pec
!
! Nombre de classes.
!
!print*,'	Minimum de tracé: ',zmint,' max: ',zmaxt,' écart : ',pec
i1=nint(zmint/pec)
i2=nint(zmaxt/pec)
!
! Ecriture du tableau de sortie.
!
kclas=0
do jclas=i1,i2
  if(jclas == 0.and.ldp0) then
    !
    ! On ne veut pas du zéro.
    !
    if(peps >= pec.or.peps == 0.) then
      !
      ! L'epsilon fourni par l'utilisateur
      ! est plus grand que l'écart!...
      ! On ne met rien du tout.
      !
    else
      kclas=kclas+1
      pclas(kclas)=-peps
      kclas=kclas+1
      pclas(kclas)=peps
    endif
  else
    kclas=kclas+1
    pclas(kclas)=real(jclas)*pec
  endif
enddo
end
