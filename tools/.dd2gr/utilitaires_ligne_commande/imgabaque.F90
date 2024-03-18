program demo
!
!-------------------------------------------------
! Implicit none.
!-------------------------------------------------
!
!IMPLICIT NONE
INTEGER, parameter :: jpx=500
INTEGER, parameter :: jpy=500
INTEGER :: irvb(3,jpx,jpy),inx,iny,iulsor,jx,jy,irvb_loc(3),jcoul,jpal
REAL :: zfracx,zfracy,zfrac
character*200 clpal,clfic
!
!-------------------------------------------------
! Palette directe colorée.
!-------------------------------------------------
!
do jy=1,jpy
  zfracy=real(jy-1)/real(jpy-1)
  do jx=1,jpx
    zfracx=real(jx-1)/real(jpx-1)
    irvb(2,jx,jy)=nint(255.*zfracx)
    irvb(3,jx,jy)=nint(255.*zfracy)
    irvb(1,jx,jy)=max(0,255-irvb(2,jx,jy)-irvb(3,jx,jy))
  enddo
enddo
call img_ecr('directe_coul.ppm',jpx,jpy,irvb)
!
!-------------------------------------------------
! Palette directe, en échelle de gris.
!-------------------------------------------------
!
do jy=1,jpy
  zfracy=real(jy-1)/real(jpy-1)
  do jx=1,jpx
    zfracx=real(jx-1)/real(jpx-1)
    irvb(2,jx,jy)=nint(255.*zfracy)
    irvb(3,jx,jy)=irvb(2,jx,jy)
    irvb(1,jx,jy)=irvb(2,jx,jy)
  enddo
enddo
call img_ecr('directe_gris.ppm',jpx,jpy,irvb)
!
!-------------------------------------------------
! Palette via img_pal_cont.
!-------------------------------------------------
!
call img_pal_init
do jpal=0,8
  if(jpal == 0) then
    clpal='REF'
  ELSEif(jpal == 1) then
    clpal='AQUABLUE'
  elseif(jpal == 2) then
    clpal='AQUABLUE.PER1'
  elseif(jpal == 3) then
    clpal='VIOLET-BLANC'
  elseif(jpal == 4) then
    clpal='CONTRASTE'
  elseif(jpal == 5) then
    clpal='OROG'
  elseif(jpal == 6) then
    clpal='ARC-EN-CIEL'
  elseif(jpal == 7) then
    clpal='BLEU-BLANC-ROUGE'
  elseif(jpal == 8) then
    clpal='TEST'
  else
    write(*,fmt=*) 'imgabaque/ERREUR: palette non attendue!...'
    write(*,fmt=*) jpal
    stop 'call abort'
  endif
  do jy=1,jpy
    zfrac=real(jy-1)/real(jpy-1)
    call img_pal(clpal,'CONT','FRAC',zfrac,irvb_loc)
    do jx=1,jpx
      do jcoul=1,3
        irvb(jcoul,jx,jy)=irvb_loc(jcoul)
      enddo
    enddo
  enddo
  write(clfic,fmt='(2a)') clpal(1:len_trim(clpal)),'.ppm'
  call img_ecr(clfic,jpx,jpy,irvb)
enddo
end
