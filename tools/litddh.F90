program lit
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
integer(kind=4), parameter :: klev=41
integer(kind=4), parameter :: klev1=klev+1
real(kind=8) zfl(klev+1)
real(kind=8) zfs(klev+1)
real(kind=8) zfl_perso(klev+1)
real(kind=8) zfs_perso(klev+1)
real(kind=8) zfq(klev+1)
real(kind=8) zvct0(klev)
real(kind=8) zvpp0(klev)
!
!-------------------------------------------------
! Ouverture du fichier LFA.
!-------------------------------------------------
!
iullfa1=23
call lfaouv(iullfa1,'DZ.mh','R')
call lfalecr(iullfa1,'FCTPRECISTL',klev1,zfl,ilong,ierr)
call lfalecr(iullfa1,'FCTPRECCSSTL',klev1,zfs,ilong,ierr)
call lfalecr(iullfa1,'FQTPRECISTL',klev1,zfq,ilong,ierr)
call lfalecr(iullfa1,'VPP0',klev,zvpp0,ilong,ierr)
call lfalecr(iullfa1,'VCT0',klev,zvct0,ilong,ierr)
zvct0=zvct0/zvpp0/1004.
write(*,fmt=*) zvct0
do jlev=1,klev
  write(48,fmt=*) -jlev,zvct0(jlev)
enddo
do jlev=1,klev1
  zt=zvct0(min(klev,jlev))
  if(zt > 273.16) then
    zdelarg=0.
  else
    zdelarg=1.
  endif
  zfl_perso(jlev)=zfq(jlev)*(-folh(zt,zdelarg))
  zfs_perso(jlev)=zfq(jlev)*(zt*4100.)
  write(49,fmt=*) -jlev,zfl(jlev)
  write(50,fmt=*) -jlev,zfs(jlev)
  write(51,fmt=*) -jlev,(zfl(jlev)+zfs(jlev))
  !write(52,fmt=*) -jlev,(zfl(jlev)+zfs(jlev))/zfq(jlev)
  write(53,fmt=*) -jlev,zfs_perso(jlev)
  write(54,fmt=*) -jlev,zfl_perso(jlev)
  write(55,fmt=*) -jlev,(zfl_perso(jlev)+zfs_perso(jlev))
enddo
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iullfa1)
end
#include"fonctions.F90"
