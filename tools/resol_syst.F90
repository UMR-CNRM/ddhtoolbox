subroutine resol(pa,py,kdim,px,pdet)
! ---------------------------------------------------------------------------
! **** *RESOL*  - Routine de resolution de systeme lineaire.
! ---------------------------------------------------------------------------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:  Pivot de Gauss
! --------
! Externes:
! ---------
! Auteur:  J.M. Piriou
! -------
! Modifications.
! --------------
! Original : 1991-01
! ---------------------------------------------------------------------------
! En entree:   pa     matrice carree (kdim,kdim)
! py     vecteur (kdim,1)
! kdim     ordre de la matrice
! En sortie:   px     vecteur (kdim,1)
! pdet   determinant du systeme
! ---------------------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: pa(kdim,kdim),py(kdim)
real(kind=8), intent(out) :: px(kdim),pdet
real(kind=8) :: za(kdim,kdim),zy(kdim)
pdet=1.
za=pa
zy=py
do ji1=1,kdim-1
  !
  ! ---------------------------------------------------------------------------
  ! Triangulation du systeme:
  ! on elimine la variable ji1 des equations ji1+1 a kdim.
  !
  if(za(ji1,ji1) == 0.) then
    pdet=0.
    za(ji1,ji1)=1.
  endif
  do ji2=ji1+1,kdim
    zy(ji2)=zy(ji2)-zy(ji1)*za(ji2,ji1)/za(ji1,ji1)
    do jj=kdim,ji1,-1
      za(ji2,jj)=za(ji2,jj)-za(ji1,jj)*za(ji2,ji1)/za(ji1,ji1)
    enddo
  enddo
enddo
!
! ---------------------------------------------------------------------------
! A ce stade, za est triangulaire superieure.
! On effectue la remontee determinant les xi.
!
if ((pdet /= 0.).and.(za(kdim,kdim) /= 0.)) then
  !
  ! La matrice est reguliere.
  !
  pdet=1.
  do ji=kdim,1,-1
    pdet=pdet*za(ji,ji)
    zsomme=0.
    do jj=ji+1,kdim
      zsomme=zsomme+za(ji,jj)*px(jj)
    enddo
    px(ji)=(zy(ji)-zsomme)/za(ji,ji)
  enddo
else
  !
  ! La matrice est singuliere.
  !
  pdet=0.
endif
end
