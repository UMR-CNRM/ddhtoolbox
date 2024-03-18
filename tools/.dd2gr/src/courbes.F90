subroutine courbes
! --------------------------------------------------------------
! **** ** Tracé de courbes x,y.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2013-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8), allocatable :: zc1m(:,:)
real(kind=8), allocatable :: zc2m(:,:)
real(kind=8), allocatable :: zx(:,:)
real(kind=8), allocatable :: zy(:,:)
!
!-------------------------------------------------
! zb: barre d'erreur dans l'estimation de la moyenne.
! Ce champ n'est pas toujours fourni. 
! Si 2 colonnes: zb non fourni.
! Si 3 colonnes: zb fourni.
!-------------------------------------------------
!
real(kind=8), allocatable :: zb(:,:)
!
!-------------------------------------------------
! llb est un logique, vrai si la courbe courante est fournie avec sa barre d'erreur.
!-------------------------------------------------
!
logical :: llb(ncourbes)
character(len=180) clmot(40)
write(*,fmt=*) ' '
write(*,fmt=*) '  ---------------------------------------------'
write(*,fmt=*) ' '
write(*,fmt=*) '  Tracé de courbes XV ou YV ou XVI ou YVI '
write(*,fmt=*) ' '
if(nchamp == 0) then
  !
  !-------------------------------------------------
  ! L'utilisateur n'a pas fourni le paramètre "#CHAMP=".
  ! Par défaut on considère que le nom du champ est celui du fichier.
  !-------------------------------------------------
  !
  do jcourbes=1,ncourbes
    cgchamp(jcourbes)=cgfdta(jcourbes)
    print*,'  courbe à tracer: n°',jcourbes,': ',trim(cgchamp(jcourbes))
  enddo
endif
!
!-------------------------------------------------
! Détermination du nombre de points par courbe, et des extrêmes.
!-------------------------------------------------
!
zc1min=1.e20 ; zc1max=-zc1min
zc2min=1.e20 ; zc2max=-zc2min
indta=0
zsaut=999.999
do jcourbes=1,ncourbes
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  iule=22 ; open(iule,file=cgfdta(jcourbes),form='formatted')
  write(*,fmt='(a,i3,9a)') '   Fichier de données n° ',jcourbes,' : ',trim(cgfdta(jcourbes))
  !
  !-------------------------------------------------
  ! On regarde si le fichier contient 2 ou 3 colonnes.
  !-------------------------------------------------
  !
  read(iule,fmt='(a)') clc
  call casc(clc,1,clmot,ilmot)
  if(ilmot == 1 .or. ilmot == 2) then
    llb(jcourbes)=.false.
  elseif(ilmot >= 3) then
    llb(jcourbes)=.true.
    write(*,fmt=*) '  La 3ème colonne du fichier ',trim(cgfdta(jcourbes)),' va être utilisée comme barre d''erreur.'
  else
    write(*,fmt=*)
    write(*,fmt=*) 'courbes/ERREUR: pas assez de colonnes dans le fichier ',trim(cgfdta(jcourbes)),'!...'
    write(*,fmt=*)
    call exit(1)
  endif
  rewind(iule)
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  inomal=0
  do
    if(llb(jcourbes)) then
      read(iule,fmt=*,iostat=ios) zc1,zc2,zc3
    elseif(ilmot == 2) then
      read(iule,fmt=*,iostat=ios) zc1,zc2
    elseif(ilmot == 1) then
      read(iule,fmt=*,iostat=ios) zc1
      zc2=inomal+1
    else
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/courbes/ERREUR: cas non attendu !...'
      write(*,fmt=*) llb(jcourbes),ilmot
      call exit(1)
    endif
    if(ios == -1) then
      !
      !-------------------------------------------------
      ! Fin de fichier.
      !-------------------------------------------------
      !
      exit
    elseif(ios == 0) then
      !
      !-------------------------------------------------
      ! Cas général.
      !-------------------------------------------------
      !
      inomal=inomal+1
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
      write(*,fmt=*) '  Fichier ',trim(cgfdta(jcourbes))
      write(*,fmt=*) '  Ligne ',inomal
      call exit(1)
    endif
    !
    !-------------------------------------------------
    ! Traitement de la ligne courante.
    !-------------------------------------------------
    !
    if(abs(zc1-zsaut) > 1.e-4 .and. abs(zc2-zsaut) > 1.e-4) then
      !
      !-------------------------------------------------
      ! Le point courant n'est pas un saut de plume.
      !-------------------------------------------------
      !
      if(zc1 < zc1min) zc1min=zc1
      if(zc1 > zc1max) zc1max=zc1
      if(zc2 < zc2min) zc2min=zc2
      if(zc2 > zc2max) zc2max=zc2
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(iule)
  !
  !-------------------------------------------------
  ! Nombre maxi de points, sur l'ensemble des courbes.
  !-------------------------------------------------
  !
  indta=max(indta,inomal)
enddo
if(ncourbes == 0) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/courbes/ERREUR: ncourbes=0 : aucune courbe référencée dans ce fichier de directives !...'
  write(*,fmt=*)
  call exit(1)
endif
print*,' '
print*,'  Sur les ',ncourbes,' courbes à tracer'
print*,'    - le nombre maxi de points est ',indta
print*,'    - les extrêmes en colonne 1 sont ',zc1min,' < c1 < ',zc1max
print*,'    - les extrêmes en colonne 2 sont ',zc2min,' < c2 < ',zc2max
!
!-------------------------------------------------
! Allocation des tableaux recevant les ncourbes courbes.
!-------------------------------------------------
!
allocate(zc1m(ncourbes,indta))
zc1m=rindef
allocate(zc2m(ncourbes,indta))
zc2m=rindef
allocate(zb(ncourbes,indta))
zb(:,:)=rindef
!
!-------------------------------------------------
! Lecture de ces tableaux.
!-------------------------------------------------
!
do jcourbes=1,ncourbes
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  iule=22 ; open(iule,file=cgfdta(jcourbes),form='formatted')
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  inomal=0
  do
    if(llb(jcourbes)) then
      read(iule,fmt=*,iostat=ios) zc1,zc2,zc3
    elseif(ilmot == 2) then
      read(iule,fmt=*,iostat=ios) zc1,zc2
    elseif(ilmot == 1) then
      read(iule,fmt=*,iostat=ios) zc1
      zc2=inomal+1
    endif
    if(ios == -1) then
      !
      !-------------------------------------------------
      ! Fin de fichier.
      !-------------------------------------------------
      !
      exit
    elseif(ios == 0) then
      !
      !-------------------------------------------------
      ! Cas général.
      !-------------------------------------------------
      !
      inomal=inomal+1
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
      call exit(1)
    endif
    !
    !-------------------------------------------------
    ! Traitement de la ligne courante.
    !-------------------------------------------------
    !
    zc1m(jcourbes,inomal)=zc1
    zc2m(jcourbes,inomal)=zc2
    if(llb(jcourbes)) then
      zb(jcourbes,inomal)=zc3
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(iule)
enddo
!
!-------------------------------------------------
! Si XV ou XVI, pas de changement à effectuer.
! Si YV ou YVI, permutation x<>y à effectuer avant tracé.
!-------------------------------------------------
!
allocate(zx(ncourbes,indta))
allocate(zy(ncourbes,indta))
if(cgformat(1:1) == 'X') then
  rxmin=zc1min
  rymin=zc2min
  rxmax=zc1max
  rymax=zc2max
  zx=zc1m
  zy=zc2m
else
  rxmin=zc2min
  rymin=zc1min
  rxmax=zc2max
  rymax=zc1max
  zx=zc2m
  zy=zc1m
endif
if(lgdebu) write(*,fmt=*) 'courbes/rxmin=',rxmin
if(lgdebu) write(*,fmt=*) 'courbes/rxmax=',rxmax
if(lgdebu) write(*,fmt=*) 'courbes/rymin=',rymin
if(lgdebu) write(*,fmt=*) 'courbes/rymax=',rymax
!
!-------------------------------------------------
! Extrêmes dynamiques.
!-------------------------------------------------
!
if(cgextdyn /= cgindef) then
  !
  !-------------------------------------------------
  ! L'utilisateur a fourni par ex "#EXTDYN=3.7", ce qui veut dire qu'il
  ! souhaite des EXTrêmes de tracé DYNamiques, calculés comme moyenne + 3.7
  ! fois l'écart-type.
  !-------------------------------------------------
  !
  call extdyn(zsaut,zx,zy,ncourbes,indta,rxmin,rxmax,rymin,rymax)
endif
!
!-------------------------------------------------
! On force les extrêmes s'ils ont été fournis par l'utilisateur.
!-------------------------------------------------
!
if(rgxminl /= rindef) rxmin=rgxminl
if(rgxmaxl /= rindef) rxmax=rgxmaxl
if(rgyminl /= rindef) rymin=rgyminl
if(rgymaxl /= rindef) rymax=rgymaxl

if(lginversex) call permuter(rxmin,rxmax)
if(lginversey) call permuter(rymin,rymax)
!
! Appel au tracé.
!
lllegende=.true.
if(cgfpix(len_trim(cgfpix)-2:len_trim(cgfpix)) == 'svg') then
  !
  !-------------------------------------------------
  ! L'utilisateur veut produire un fichier vectoriel SVG.
  !-------------------------------------------------
  !
  call svg_trac1d(rindef,zsaut,indta,ncourbes,zx,zy,zb,llb,cgchamp &
  & ,lllegende,nximage,nyimage,cgfpix)
else
  !
  !-------------------------------------------------
  ! L'utilisateur veut produire un fichier pixel (type GIF, PNG, JPG, etc).
  !-------------------------------------------------
  !
  !
  ! Epaisseur de trait des courbes en pixels.
  !
  ipix_largeur=2
  call img_trac1d(rindef,zsaut,indta,ncourbes,zx,zy,cgchamp,rxmin,rxmax,rymin,rymax &
  & ,cgtitre,lllegende,ipix_largeur,nximage,nyimage,cgfpix)
endif
end
