program petit
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"implicit_r8i4.h"
iul=40 ; open(iul,file='petit.dta',form='formatted')
inb=60
do jnb=1,inb
  zr=3.3e-4
  ztheta=4.*atan(1.)*2.*real(jnb-1)/real(inb-1)
  zx=zr*cos(ztheta)
  zy=zr*sin(ztheta)*1.e15
  write(iul,fmt=*) zx,zy
enddo
end
