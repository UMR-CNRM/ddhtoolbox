program lit
!
!-------------------------------------------------
! Calcul de dégradé de couleur.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"/home/piriou/ftn/implicit_r8i4.h"
integer irvb1(3)
integer irvb2(3)
integer irvb(3)
irvb1(1)=255 ; irvb1(2)=255 ; irvb1(3)=255 ! couleur de départ.
irvb2(1)=000 ; irvb2(2)=000 ; irvb2(3)=255 ! couleur d'arrrivée.
in=5
do jn=0,in
  zpoi=real(jn)/real(in)
  do jc=1,3
    irvb(jc)=max(0,min(255,nint(zpoi*real(irvb2(jc))+(1.-zpoi)*real(irvb1(jc)))))
  enddo
  write(*,fmt=*) jn,'/',in,' : ',irvb(1),irvb(2),irvb(3)
enddo
end
