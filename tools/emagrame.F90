subroutine emagramme(kul1,cdficd,cdtypeema,klev)
! --------------------------------------------------------------
! **** ** Emagramme type 761 ou (theta, thetaE, thetaES).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2009-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use const_ther, only : rcpd, rg, rcpv
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)

real(kind=8), allocatable :: zp(:)
real(kind=8), allocatable :: zvep1(:)
real(kind=8), allocatable :: zt(:)
real(kind=8), allocatable :: zqv(:)
real(kind=8), allocatable :: zql(:)
real(kind=8), allocatable :: zqi(:)
real(kind=8), allocatable :: ztheta(:)
real(kind=8), allocatable :: zcooy(:)
real(kind=8), allocatable :: zrcp(:)
real(kind=8) :: fth_theta
real(kind=8) :: fth_tphig
real(kind=8) :: fth_thetad
real(kind=8) :: fth_t
real(kind=8) :: zthetad
real(kind=8) :: ztw
real(kind=8) :: fth_hr
real(kind=8) :: fth_thetaes_bolton
real(kind=8) :: fth_tw
real(kind=8) :: fth_thetapw
real(kind=8) :: fth_thetae_bolton
real(kind=8) :: fth_thetas
real(kind=8) :: fth_thetav
real(kind=8) :: fth_thetal
real(kind=8) :: fth_thetavl
!
!-------------------------------------------------
! Allocation des tableaux.
!-------------------------------------------------
!
allocate(zp(klev))
allocate(zvep1(klev))
allocate(zt(klev))
allocate(zqv(klev))
allocate(zql(klev))
allocate(zqi(klev))
allocate(zrcp(klev))
allocate(ztheta(klev))
allocate(zcooy(klev))
!
!-------------------------------------------------
! Lecture.
!-------------------------------------------------
!
call lfalecc(kul1,'INDICE EXPERIENCE',1,clnamx,ilong,irep)
call lfalecr(kul1,'VPP1',klev,zp,ilong,ierr)
call lfalecr(kul1,'VEP1',klev,zvep1,ilong,ierr)
call lfalecr(kul1,'VCT1',klev,zt,ilong,ierr)
call lfalecr(kul1,'VQV1',klev,zqv,ilong,ierr)
call lfalecr(kul1,'VQL1',klev,zql,ilong,ierr)
!
!-------------------------------------------------
! Lecture de VQN ou VQI.
!-------------------------------------------------
!
call lfacas(kul1,'VQI1',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kul1,'VQI1',klev,zqi,ilong,ierr)
else
  call lfacas(kul1,'VQN1',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kul1,'VQN1',klev,zqi,ilong,ierr)
  else
    zqi=0.
  endif
endif
!
!-------------------------------------------------
! Calcul de T, qv, p, z.
!-------------------------------------------------
!
zqv=zqv/zp
zql=zql/zp
zqi=zqi/zp
zrcp=rcpd+(rcpv-rcpd)*zqv
zt=zt/zp/zrcp
zvep1=zvep1/zp

!
!-------------------------------------------------
! Profil vertical de p.
!-------------------------------------------------
!
zp=zp*rg
do jlev=2,klev
  zp(jlev)=zp(jlev-1)+zp(jlev)
enddo
!
!-------------------------------------------------
! Fichier Theta.
!-------------------------------------------------
!
cltheta=trim(cdficd)//'.ema.tmp.theta.dta'
print*,trim(cltheta)
open(75,file=cltheta,form='formatted')
!
!-------------------------------------------------
! Fichier thetad.
!-------------------------------------------------
!
clthetad=trim(cdficd)//'.ema.tmp.thetad.dta'
print*,trim(clthetad)
open(79,file=clthetad,form='formatted')
!
!-------------------------------------------------
! Fichier thetaw.
!-------------------------------------------------
!
clthetaw=trim(cdficd)//'.ema.tmp.thetaw.dta'
print*,trim(clthetaw)
open(80,file=clthetaw,form='formatted')
!
!-------------------------------------------------
! Fichier hr.
!-------------------------------------------------
!
clhr=trim(cdficd)//'.ema.tmp.hr.dta'
print*,trim(clhr)
open(81,file=clhr,form='formatted')
!
!-------------------------------------------------
! Fichier qv.
!-------------------------------------------------
!
clqv=trim(cdficd)//'.ema.tmp.qv.dta'
print*,trim(clqv)
open(82,file=clqv,form='formatted')
!
!-------------------------------------------------
! Fichier thetapw.
!-------------------------------------------------
!
clthetapw=trim(cdficd)//'.ema.tmp.thetapw.dta'
print*,trim(clthetapw)
open(83,file=clthetapw,form='formatted')
!
!-------------------------------------------------
! Fichier thetaes.
!-------------------------------------------------
!
clfthetaes=trim(cdficd)//'.ema.tmp.thetaes.dta'
print*,trim(clfthetaes)
open(84,file=clfthetaes,form='formatted')
!
!-------------------------------------------------
! Fichier thetae.
!-------------------------------------------------
!
clfthetae=trim(cdficd)//'.ema.tmp.thetae.dta'
print*,trim(clfthetae)
open(85,file=clfthetae,form='formatted')
!
!-------------------------------------------------
! Fichier thetas.
!-------------------------------------------------
!
clfthetas=trim(cdficd)//'.ema.tmp.thetas.dta'
print*,trim(clfthetas)
open(86,file=clfthetas,form='formatted')
!
!-------------------------------------------------
! Fichier thetav.
!-------------------------------------------------
!
clfthetav=trim(cdficd)//'.ema.tmp.thetav.dta'
print*,trim(clfthetav)
open(87,file=clfthetav,form='formatted')
!
!-------------------------------------------------
! Fichier thetavl.
!-------------------------------------------------
!
clfthetavl=trim(cdficd)//'.ema.tmp.thetavl.dta'
print*,trim(clfthetavl)
open(88,file=clfthetavl,form='formatted')
!
!-------------------------------------------------
! Fichier ql.
!-------------------------------------------------
!
clfql=trim(cdficd)//'.ema.tmp.ql.dta'
print*,trim(clfql)
open(89,file=clfql,form='formatted')
!
!-------------------------------------------------
! Fichier qi.
!-------------------------------------------------
!
clfqi=trim(cdficd)//'.ema.tmp.qi.dta'
print*,trim(clfqi)
open(90,file=clfqi,form='formatted')
!
!-------------------------------------------------
! Fichier thetal.
!-------------------------------------------------
!
clfthetal=trim(cdficd)//'.ema.tmp.thetal.dta'
print*,trim(clfthetal)
open(91,file=clfthetal,form='formatted')
!
!-------------------------------------------------
! Fichier autodocumentation.
!-------------------------------------------------
!
clfdoc=trim(cdficd)//'.ema.doc'
print*,trim(clfdoc)
call autodoc(kul1,cdficd,cldate,clorigine)
open(92,file=clfdoc,form='formatted')
write(92,fmt='(9a)') '#FORMAT=XVI'
write(92,fmt='(9a)') '#TITRE=Stability profiles'
write(92,fmt='(9a)') trim(cldate)
write(92,fmt='(9a)') '#LEGENDE_X=(K)'
write(92,fmt='(9a)') '#LEGENDE_Y=p (hPa)'
write(92,fmt='(9a)') trim(clorigine)
write(92,fmt='(9a)') ' '
write(92,fmt='(9a)') '#CHAMP=theta'
write(92,fmt='(9a)') '#FICHIER=',trim(cdficd),'.ema.tmp.theta.dta'
write(92,fmt='(9a)') ' '
write(92,fmt='(9a)') '#CHAMP=thetaE'
write(92,fmt='(9a)') '#FICHIER=',trim(cdficd),'.ema.tmp.thetae.dta'
write(92,fmt='(9a)') ' '
write(92,fmt='(9a)') '#CHAMP=thetaES'
write(92,fmt='(9a)') '#FICHIER=',trim(cdficd),'.ema.tmp.thetaes.dta'
write(92,fmt='(9a)') ' '
write(92,fmt='(9a)') '#CHAMP=thetaS'
write(92,fmt='(9a)') '#FICHIER=',trim(cdficd),'.ema.tmp.thetas.dta'
close(92)
!
!-------------------------------------------------
! Variables atmosphériques.
!-------------------------------------------------
!
do jlev=klev,1,-1
  !
  !-------------------------------------------------
  ! On ne calcule qu'en dessous de 200 hPa.
  !-------------------------------------------------
  !
  if(zp(jlev) < 20000.) cycle
  zcoov=-zp(jlev)/100. ! coordonnée verticale -p en hPa.
  !
  !-------------------------------------------------
  ! Theta.
  !-------------------------------------------------
  !
  ztheta(jlev)=fth_theta(zp(jlev),zt(jlev))
  write(75,fmt=*) ztheta(jlev),zcoov
  !
  !-------------------------------------------------
  ! thetad.
  !-------------------------------------------------
  !
  zthetad=fth_thetad(zp(jlev),zqv(jlev))
  write(79,fmt=*) zthetad,zcoov
  !
  !-------------------------------------------------
  ! thetaw.
  !-------------------------------------------------
  !
  ztw=fth_tw(zp(jlev),zt(jlev),zqv(jlev))
  write(80,fmt=*) ztw,zcoov
  !
  !-------------------------------------------------
  ! hr.
  !-------------------------------------------------
  !
  zhr=fth_hr(zp(jlev),zt(jlev),zqv(jlev))
  write(81,fmt=*) zhr,zcoov
  !
  !-------------------------------------------------
  ! qv.
  !-------------------------------------------------
  !
  write(82,fmt=*) zqv(jlev),zcoov
  !
  !-------------------------------------------------
  ! thetapw.
  !-------------------------------------------------
  !
  zthetapw=fth_thetapw(zp(jlev),zt(jlev),zqv(jlev))
  write(83,fmt=*) zthetapw,zcoov
  !
  !-------------------------------------------------
  ! thetaes_bolton.
  !-------------------------------------------------
  !
  zthetaes_bolton=fth_thetaes_bolton(zp(jlev),zt(jlev))
  write(84,fmt=*) zthetaes_bolton,zcoov
  !
  !-------------------------------------------------
  ! thetae_bolton.
  !-------------------------------------------------
  !
  zthetae_bolton=fth_thetae_bolton(zp(jlev),zt(jlev),zqv(jlev))
  write(85,fmt=*) zthetae_bolton,zcoov
  !
  !-------------------------------------------------
  ! thetaS.
  !-------------------------------------------------
  !
  zthetas=fth_thetas(zp(jlev),zt(jlev),zqv(jlev),zql(jlev),zqi(jlev),3)
  write(86,fmt=*) zthetas,zcoov
  !
  !-------------------------------------------------
  ! thetaV.
  !-------------------------------------------------
  !
  zthetav=fth_thetav(zp(jlev),zt(jlev),zqv(jlev))
  write(87,fmt=*) zthetav,zcoov
  !
  !-------------------------------------------------
  ! thetaVL.
  !-------------------------------------------------
  !
  zqc=zql(jlev)+zqi(jlev)
  zthetavl=fth_thetavl(zp(jlev),zt(jlev),zqv(jlev),zqc)
  write(88,fmt=*) zthetavl,zcoov
  !
  !-------------------------------------------------
  ! ql.
  !-------------------------------------------------
  !
  write(89,fmt=*) zql(jlev),zcoov
  !
  !-------------------------------------------------
  ! qi.
  !-------------------------------------------------
  !
  write(90,fmt=*) zqi(jlev),zcoov
  !
  !-------------------------------------------------
  ! thetaL.
  !-------------------------------------------------
  !
  zqc=zql(jlev)+zqi(jlev)
  zthetal=fth_thetal(zp(jlev),zt(jlev),zqv(jlev),zqc)
  write(91,fmt=*) zthetal,zcoov
enddo
end
