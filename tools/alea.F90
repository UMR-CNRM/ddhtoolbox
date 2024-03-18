function alea(px)
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)

zx=9821.*px+0.211327
alea=zx-int(zx)
end
