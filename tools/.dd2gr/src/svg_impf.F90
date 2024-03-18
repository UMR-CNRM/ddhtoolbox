subroutine svg_impf
! --------------------------------------------------------------
! **** *svg_impf* IMPacts de Foudre: pointage.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2023-01-16, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
use parametres
use lfagen
#include"implicit_r8i4.h"
character(len=400) :: clmot(40)
real(kind=8), allocatable :: ztab2(:,:)
integer(kind=4) :: iisolcn(3) ! Couleur de l'isoligne de valeur minimale.
integer(kind=4) :: iisolcx(3) ! Couleur de l'isoligne de valeur maximale.
!
!-------------------------------------------------
! Prologue.
!-------------------------------------------------
!
write(*,fmt=*) ' '
write(*,fmt=*) '  Impacts de foudre : pointage.'
write(*,fmt=*) trim(cgimpf)
write(nulsvg,fmt=*) ' ' 
write(nulsvg,fmt=*) '<!-- Impact de foudre : pointage -->'

!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------

! Couleur du "plus" pointé en début de période.
zr0=000.
zv0=000.
zb0=255.

! Couleur du "plus" pointé en fin de période.
zr1=255.
zv1=000.
zb1=000.

!-------------------------------------------------
! La variable cgimpf contient 3 champs : nom du fichier d'impacts, date de début de tracé, date de fin de tracé. Les dates sont AAAAMMJJHH.
!-------------------------------------------------
call casc(cgimpf,1,clmot,ilmot)
clfimpf=clmot(1) ! fichier ASCII des impacts de foudre observés.
cltemps0=clmot(2) ! AAAAMMJJHH du début de période.
cltemps1=clmot(3) ! AAAAMMJJHH de la fin de période.
read(cltemps0(09:10),fmt=*) ihh0
read(cltemps1(09:10),fmt=*) ihh1
if(ihh1 == 0) ihh1=24

!-------------------------------------------------
! Lecture du fichier d'impacts de foudre.
!-------------------------------------------------
iuli=52 ; open(iuli,file=clfimpf,form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
inomal=0
do
  read(iuli,fmt=*,iostat=ios) ihh,imm,iss,zgol1,zgol2,zlat,zlon
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
    stop 'call abort'
  endif
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Position X et Y de l'impact de foudre.
  !-------------------------------------------------
  !
  zx1=rxt+rlxt*(zlon-rxmin)/(rxmax-rxmin)
  zy1=ryt+rlyt*(rymax-zlat)/(rymax-rymin)
  
  !-------------------------------------------------
  ! Ecriture d'un "plus" à cet endroit.
  !-------------------------------------------------
  !rgplusse=0.500 ! la taille du plus est plus petite que le standard.
  
  ! Couleur du plus.
  ztemps_impact=real(ihh)+real(imm)/60.+real(iss)/3600.
  zfrac=(ztemps_impact-real(ihh0))/(real(ihh1)-real(ihh0))
  if(zfrac >= 0. .and. zfrac <= 1. .and. zlon > rxmin .and. zlon < rxmax .and. zlat > rymin .and. zlat < rymax) then
    ir=min(255,max(0,nint(zr0+(zr1-zr0)*zfrac)))
    iv=min(255,max(0,nint(zv0+(zv1-zv0)*zfrac)))
    ib=min(255,max(0,nint(zb0+(zb1-zb0)*zfrac)))
    write(clcoul,fmt='(4(a,i3.3))') 'rgb(',ir,',',iv,',',ib,')'
    call plusse(zx1,zy1,clcoul)
  endif
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iuli)
end
