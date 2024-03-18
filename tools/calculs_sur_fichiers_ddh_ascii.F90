#include"fonctions.F90"
program calculs
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit none
!
!-------------------------------------------------
! Nombre de domaines et niveaux.
!-------------------------------------------------
!
INTEGER(KIND=4), parameter :: jpx=30
INTEGER(KIND=4), parameter :: jpy=41
CHARACTER*200 :: CLFDDH
CHARACTER*200 :: CLFENT
INTEGER(KIND=4) :: IUL
INTEGER(KIND=4) :: JX
INTEGER(KIND=4) :: JY
REAL(KIND=8) :: ZAUX
REAL(KIND=8) :: ZDERI_H
REAL(KIND=8) :: ZDERI_HQS
REAL(KIND=8) :: ZDERI_HQSPPS
REAL(KIND=8) :: ZDERI_QVMQS
REAL(KIND=8) :: ZDERI_THETAV
REAL(KIND=8) :: ZDERI_U,qs,thetav
!
!-------------------------------------------------
! Tableaux recevant les données (domaine, niveau, valeur).
!-------------------------------------------------
!
REAL(KIND=8) zvuu0(jpx,jpy)
REAL(KIND=8) zvct0(jpx,jpy)
REAL(KIND=8) zvqv0(jpx,jpy)
REAL(KIND=8) zvpp0(jpx,jpy)
REAL(KIND=8) zvep0(jpx,jpy)
REAL(KIND=8) zthetav0(jpx,jpy)
REAL(KIND=8) zx(jpx,jpy)
REAL(KIND=8) zy(jpx,jpy)
REAL(KIND=8) zu(jpx,jpy)
REAL(KIND=8) zt(jpx,jpy)
REAL(KIND=8) zqv(jpx,jpy)
REAL(KIND=8) zp(jpx,jpy)
REAL(KIND=8) zz(jpx,jpy)
REAL(KIND=8) zh(jpx,jpy)
REAL(KIND=8) zhqs(jpx,jpy)
REAL(KIND=8) zhqspps(jpx,jpy)
REAL(KIND=8) zqvmqs(jpx,jpy)
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfddh='DZ.lfa'
clfent=clfddh(1:len_trim(clfddh))//'.tmp.VCT0.dta'
open(iul,file=clfent,form='formatted')
do jx=1,jpx
  do jy=1,jpy
    read(iul,fmt=*) zx(jx,jy),zy(jx,jy),zvct0(jx,jy)
  enddo
enddo
close(iul)
zt=zvct0
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent=clfddh(1:len_trim(clfddh))//'.tmp.VQV0.dta'
open(iul,file=clfent,form='formatted')
do jx=1,jpx
  do jy=1,jpy
    read(iul,fmt=*) zx(jx,jy),zy(jx,jy),zvqv0(jx,jy)
  enddo
enddo
close(iul)
zqv=zvqv0/1000.
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent=clfddh(1:len_trim(clfddh))//'.tmp.VPP0.dta'
open(iul,file=clfent,form='formatted')
do jx=1,jpx
  do jy=1,jpy
    read(iul,fmt=*) zx(jx,jy),zy(jx,jy),zvpp0(jx,jy)
  enddo
enddo
close(iul)
zp=zvpp0
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent=clfddh(1:len_trim(clfddh))//'.tmp.VEP0.dta'
open(iul,file=clfent,form='formatted')
do jx=1,jpx
  do jy=1,jpy
    read(iul,fmt=*) zx(jx,jy),zy(jx,jy),zvep0(jx,jy)
  enddo
enddo
close(iul)
zz=zvep0
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent=clfddh(1:len_trim(clfddh))//'.tmp.VUU0.dta'
open(iul,file=clfent,form='formatted')
do jx=1,jpx
  do jy=1,jpy
    read(iul,fmt=*) zx(jx,jy),zy(jx,jy),zvuu0(jx,jy)
  enddo
enddo
close(iul)
zu=zvuu0
!
!-------------------------------------------------
! Calcul de thetav.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zthetav0(jx,jy)=thetav(zp(jx,jy),zt(jx,jy),zqv(jx,jy))
  enddo
enddo
!
!-------------------------------------------------
! Calcul de h.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zh(jx,jy)=1005.*zt(jx,jy)+9.80665*zz(jx,jy)+2.5E6*zqv(jx,jy)
  enddo
enddo
!
!-------------------------------------------------
! Calcul de hqs.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zhqs(jx,jy)=1005.*zt(jx,jy)+9.80665*zz(jx,jy)+2.5E6*(zqv(jx,jy)-qs(zt(jx,jy),zp(jx,jy)))
  enddo
enddo
!
!-------------------------------------------------
! Calcul de hqspps.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zhqspps(jx,jy)=1005.*zt(jx,jy)+9.80665*zz(jx,jy) &
    & +2.5E6*(zqv(jx,jy)-zp(jx,jy)/zp(jx,jpy)*qs(zt(jx,jy),zp(jx,jy)))
  enddo
enddo
!
!-------------------------------------------------
! Calcul de q-qsat.
!-------------------------------------------------
!
do jx=1,jpx
  do jy=1,jpy
    zqvmqs(jx,jy)=zqv(jx,jy)-qs(zt(jx,jy),zp(jx,jy))
  enddo
enddo
!
!-------------------------------------------------
! Calcul et écriture de diverses grandeurs et de leur gradient vertical.
!-------------------------------------------------
!
open(22,file='tmp.dthetav_sur_dz',form='formatted')
open(23,file='tmp.thetav',form='formatted')
open(24,file='tmp.T',form='formatted')
open(25,file='tmp.h',form='formatted')
open(26,file='tmp.dh_sur_dz',form='formatted')
open(27,file='tmp.hqs',form='formatted')
open(28,file='tmp.dhqs_sur_dz',form='formatted')
open(30,file='tmp.hqspps',form='formatted')
open(31,file='tmp.dhqspps_sur_dz',form='formatted')
open(32,file='tmp.qvmqs',form='formatted')
open(33,file='tmp.zaux',form='formatted')
open(34,file='tmp.dqvmqs_sur_dz',form='formatted')
open(35,file='tmp.du_sur_dz',form='formatted')
do jx=1,jpx
  do jy=1,jpy-1
    zderi_thetav=(zthetav0(jx,jy)-zthetav0(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zderi_h=(zh(jx,jy)-zh(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zderi_hqs=(zhqs(jx,jy)-zhqs(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zderi_hqspps=(zhqspps(jx,jy)-zhqspps(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zderi_qvmqs=(zqvmqs(jx,jy)-zqvmqs(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zderi_u=(zu(jx,jy)-zu(jx,jy+1))/(zz(jx,jy)-zz(jx,jy+1))
    zaux=zderi_qvmqs*max(0.,sign(1.,-zderi_h))
    if(zy(jx,jy) < 12.) then
      write(22,fmt=*) zx(jx,jy),zy(jx,jy),zderi_thetav
      write(23,fmt=*) zx(jx,jy),zy(jx,jy),zthetav0(jx,jy)
      write(24,fmt=*) zx(jx,jy),zy(jx,jy),zt(jx,jy)
      write(25,fmt=*) zx(jx,jy),zy(jx,jy),zh(jx,jy)
      write(26,fmt=*) zx(jx,jy),zy(jx,jy),zderi_h
      write(27,fmt=*) zx(jx,jy),zy(jx,jy),zhqs(jx,jy)
      write(28,fmt=*) zx(jx,jy),zy(jx,jy),zderi_hqs
      write(30,fmt=*) zx(jx,jy),zy(jx,jy),zhqspps(jx,jy)
      write(31,fmt=*) zx(jx,jy),zy(jx,jy),zderi_hqspps
      write(32,fmt=*) zx(jx,jy),zy(jx,jy),zqvmqs(jx,jy)
      write(33,fmt=*) zx(jx,jy),zy(jx,jy),zaux
      write(34,fmt=*) zx(jx,jy),zy(jx,jy),zderi_qvmqs    
      write(35,fmt=*) zx(jx,jy),zy(jx,jy),abs(zderi_u)
    endif
  enddo
enddo
close(iul)
end
