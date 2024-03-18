program f
! --------------------------------------------------------------
! **** *F* Crée une namelist DDH.
! --------------------------------------------------------------
! Sujet:
!
! Le présent programme lit un fichier de type LLV ou LL
! et écrit en sortie une namelist NAMDDH,
! en traduisant chaque couple de coordonnées (X,Y) comme
! un domaine limité de DDH.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   1999-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*200 :: CLFENT
INTEGER(KIND=4) :: INOMAL
INTEGER(KIND=4) :: IOS
INTEGER(KIND=4) :: IUL
REAL(KIND=8) :: ZX
REAL(KIND=8) :: ZY

!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
clfent='voisinage.tmp.dta'
open(iul,file=clfent,form='formatted')
inomal=0
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
do
  read(iul,fmt=*,iostat=ios) zx,zy
  if(ios /= 0) exit
  inomal=inomal+1
  write(77,fmt=*) ' '
  write(77,fmt=*) '   BDEDDH(1,',inomal,')=4.,   # Domaine type point.'
  write(77,fmt=*) '   BDEDDH(2,',inomal,')=1.,   # Plan virtuel.'
  write(77,fmt=*) '   BDEDDH(3,',inomal,')=',zx,',   # Longitude (deg).'
  write(77,fmt=*) '   BDEDDH(4,',inomal,')=',zy,',   # Latitude (deg).'
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
print*,inomal,' articles lus.'
close(iul)
end
