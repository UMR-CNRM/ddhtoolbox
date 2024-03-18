subroutine extrapole_concentrique(knx,kny,pindef,ptab)
! --------------------------------------------------------------
! **** ** Extrapolation par recherche des données disponibles par zones concentriques autour du point de valeur manquante.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2010-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  knx,kny: dimension du tableau ptab.
!  pindef: valeur réelle affectée aux points de grille sans donnée.
! En entree/sortie:
!  ptab(knx,kny): données sur grille régulière. Certaines valeurs sont manquantes
!    en entrée, puis comblées en sortie.
! --------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8) ptab(knx,kny)
real(kind=8) ztab(knx,kny)
integer(kind=4) itab(knx,kny)
ztab=ptab
do jx=1,knx
  do jy=1,kny
    if(ptab(jx,jy) == pindef) then 
      irayon=0
      zsomme=0.
      isomme=0
      ptab(jx,jy)=0.
      do
        irayon=irayon+1
        do jy2=max(1,jy-irayon),min(kny,jy+irayon)
          do jx2=max(1,jx-irayon),min(knx,jx+irayon)
            if(ztab(jx2,jy2) /= pindef) then
              !
              !-------------------------------------------------
              ! Moyenne des points non manquants à la distance irayon.
              !-------------------------------------------------
              !
              zpoids=1./real((jx2-jx)*(jx2-jx)+(jy2-jy)*(jy2-jy))
              ptab(jx,jy)=ptab(jx,jy)+ztab(jx2,jy2)*zpoids
              zsomme=zsomme+zpoids
              isomme=isomme+1
            endif
          enddo
        enddo
        if(isomme >= 16) then
          ptab(jx,jy)=ptab(jx,jy)/zsomme
          exit
        elseif(irayon > knx .and. irayon > kny) then
          write(*,fmt=*) 
          write(*,fmt=*) 'dd2gr/extrapole_concentrique/ERREUR: toutes les valeurs sont manquantes!...'
          write(*,fmt=*) isomme,irayon,knx,kny
          ptab(jx,jy)=pindef
          call exit(1)
        endif
      enddo
    endif
  enddo
enddo
end
