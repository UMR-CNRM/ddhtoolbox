program equa
open(1,file='cercle_rayon_1',form='formatted')
ipts=2800
do 100 jpts=1,ipts
  zang=float(jpts-1)/float(ipts-1)*8.*atan(1.)
  zx=cos(zang)
  zy=sin(zang)
  write(1,fmt='(2E16.7)') zx,zy
  100 continue
close(1)
open(1,file='equateur',form='formatted')
do 200 jpts=1,ipts
  zang=-180.+float(jpts-1)/float(ipts-1)*360.
  write(1,fmt='(2E16.7)') zang,0.
  200 continue
close(1)
stop
end
