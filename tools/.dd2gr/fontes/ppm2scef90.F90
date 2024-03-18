program ppm
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent='9x15bold.ppm'
open(iul,file=clfent,form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
read(iul,fmt=*)
!
!-------------------------------------------------
! Nombre de points en X et Y de l'image pgm ascii.
!-------------------------------------------------
!
read(iul,fmt=*) inx,iny
print*,inx,iny
read(iul,fmt=*) 
open(1,file='data.F90',form='formatted')
!
!-------------------------------------------------
! On lit l'image sur un tableau entier 2D.
!-------------------------------------------------
!
write(1,fmt=*) 'integer ipixfont(',inx,',',iny,')'
write(1,fmt=*) 'ipixfont=0'
do jy=0,iny-1
  do jx=0,inx-1
    read(iul,fmt=*,iostat=ios) igris
    if(igris /= 0) then
      write(1,fmt=*) 'ipixfont(',jx+1,',',jy+1,')=1'
    endif
  enddo
enddo
end
