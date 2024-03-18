subroutine xyct
! --------------------------------------------------------------
! **** ** Format XYCT: XY Couleur Texte.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entrée on a un fichier ASCII dont chaque ligne est du type
! 5.245 -7.52315 255,0,0 Maison
! ce qui écrira en sortie sur le graphique SVG 
! la chaîne "Maison" en rouge au lieu de coordonnées (5.245 , -7.52315).
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8), allocatable :: zx(:),zxloc(:)
real(kind=8), allocatable :: zy(:),zyloc(:)
character(len=400), allocatable :: clrvb(:),clrvbloc(:)
character(len=400), allocatable :: cltexte(:),cltexteloc(:)
character(len=180) :: clmot(40)
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
inquire(file=cgfdta(1),exist=llex)
if(.not. llex .and. lgc2d) then
  write(*,fmt=*) 
  write(*,fmt=*) 'dd2gr/ERREUR: fichier DTA inexistant!...'
  write(*,fmt=*) trim(cgfdta(1))
  call exit(1)
endif
write(*,fmt=*) ' '
write(*,fmt=*) '  ---------------------------------------------'
write(*,fmt=*) ' '
write(*,fmt=*) '  Tracé XYCT'
write(*,fmt=*) ' '
write(*,fmt=*) '  Fichier de données lu: ',trim(cgfdta(1))
iule=22 ; open(iule,file=cgfdta(1),form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
indta=0
do
  read(iule,fmt='(a)',iostat=ios) clc
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
    indta=indta+1
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
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
!
!-------------------------------------------------
! Impression.
!-------------------------------------------------
!
write(*,fmt=*) indta,' lignes lues.'
!
!-------------------------------------------------
! Allocation.
!-------------------------------------------------
!
allocate(zxloc(indta))
allocate(zyloc(indta))
allocate(clrvbloc(indta))
allocate(cltexteloc(indta))
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iule=22 ; open(iule,file=cgfdta(1),form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
indta=0
do
  read(iule,fmt='(a)',iostat=ios) clc
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
  ! On casse la chaîne en ses 4 mots.
  !-------------------------------------------------
  !
  call casc(clc,1,clmot,ilmot)
  if(ilmot /= 4) then
    write(*,fmt=*)
    write(*,fmt=*) 'dd2gr/xyct/ERREUR: en format XYCT on attend 4 mots sur la ligne !...'
    write(*,fmt=*) trim(clc)
    write(*,fmt=*)
    call exit(1)
  endif
  read(clmot(1),fmt=*) zxtmp
  read(clmot(2),fmt=*) zytmp
  clrvbtmp=clmot(3)
  cltextetmp=clmot(4)
  !
  !-------------------------------------------------
  ! Gestion des axes en log.
  !-------------------------------------------------
  !
  if(trim(cgtypax) == 'LOG') zxtmp=log(abs(zxtmp))/log(10.)
  if(trim(cgtypay) == 'LOG') zytmp=log(abs(zytmp))/log(10.)
  if(trim(cgtypax) == '-LOG') zxtmp=-log(abs(zxtmp))/log(10.)
  if(trim(cgtypay) == '-LOG') zytmp=-log(abs(zytmp))/log(10.)
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  if(rgxminl /= rindef .and. zxtmp < rgxminl) cycle
  if(rgxmaxl /= rindef .and. zxtmp > rgxmaxl) cycle
  if(rgyminl /= rindef .and. zytmp < rgyminl) cycle
  if(rgymaxl /= rindef .and. zytmp > rgymaxl) cycle
  indta=indta+1
  zxloc(indta)=zxtmp
  zyloc(indta)=zytmp
  clrvbloc(indta)=clrvbtmp
  cltexteloc(indta)=cltextetmp
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
write(*,fmt=*) indta,' points au sein du zoom X et Y demandé.'
if(indta == 0) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/xyct/ERREUR: aucun point dans le zoom demandé !...'
  write(*,fmt=*)
  call exit(1)
endif
!
!-------------------------------------------------
! Transfert du tableau dimensionné au nombre de lignes
! du fichier à celui dimensionné au nombre de points
! au sein du zoom demandé.
!-------------------------------------------------
!
allocate(zx(indta))
allocate(zy(indta))
allocate(clrvb(indta))
allocate(cltexte(indta))
do jcourbes=1,indta
  zx(jcourbes)=zxloc(jcourbes)
  zy(jcourbes)=zyloc(jcourbes)
  clrvb(jcourbes)=clrvbloc(jcourbes)
  cltexte(jcourbes)=cltexteloc(jcourbes)
enddo
deallocate(zxloc)
deallocate(zyloc)
deallocate(clrvbloc)
deallocate(cltexteloc)
!
!-------------------------------------------------
! Calcul du nombre de points en X et Y sur l'image de sortie.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Dans le cas de pointage, pas d'interpolation à faire.
! On dimensionne le tableau interpolé au minimum.
!-------------------------------------------------
!
inx=1
iny=1
!
!-------------------------------------------------
! Initialisation des min/max.
!-------------------------------------------------
!
rxmin=minval(zx)
rxmax=maxval(zx)
rymin=minval(zy)
rymax=maxval(zy)
!
!-------------------------------------------------
! S'ils sont imposés par l'utilisateur, on force ces min/max à être ceux de
! l'utilisateur.
!-------------------------------------------------
!
if(rgxminl /= rindef) rxmin=rgxminl
if(rgxmaxl /= rindef) rxmax=rgxmaxl
if(rgyminl /= rindef) rymin=rgyminl
if(rgymaxl /= rindef) rymax=rgymaxl

if(lginversex) call permuter(rxmin,rxmax)
if(lginversey) call permuter(rymin,rymax)
!
!-------------------------------------------------
! On sauvegarde ces min/max, car après ils vont être modifiés (forcés si égaux
! entre eux), or sur l'affichage final des min/max on veut celle réelle du
! champ, avant forçage.
!-------------------------------------------------
!
rcmin_reel=rcmin
rcmax_reel=rcmax
!
!-------------------------------------------------
! Tracé.
!-------------------------------------------------
!
write(*,fmt=*) '  Champ en entrée: '
write(*,fmt=*) '    X : ',rxmin,' > ',rxmax
write(*,fmt=*) '    Y : ',rymin,' > ',rymax
write(*,fmt=*) ' '
!
!-------------------------------------------------
! L'utilisateur veut produire un fichier vectoriel SVG.
!-------------------------------------------------
!
!-------------------------------------------------
! Pointage de texte.
!-------------------------------------------------
!
llclose=.true.
call svg_xyct(indta,zx,zy,clrvb,cltexte)
end
