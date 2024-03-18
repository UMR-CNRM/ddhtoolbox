subroutine xyv_reg
! --------------------------------------------------------------
! **** ** Tracé image de données régulières.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2010-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8), allocatable :: ztab(:,:)
real(kind=8) :: zxirreg(1)
real(kind=8) :: zyirreg(1)
real(kind=8) :: zzirreg(1)
!
!-------------------------------------------------
! Lecture du nombre de données en X et Y, et de l'incrément.
!-------------------------------------------------
!
read(cgregx,fmt=*) inx,rxmin,rxmax
read(cgregy,fmt=*) iny,rymin,rymax

if(lginversex) call permuter(rxmin,rxmax)
if(lginversey) call permuter(rymin,rymax)
!
!-------------------------------------------------
! Allocation.
!-------------------------------------------------
!
allocate(ztab(inx,iny))
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
write(*,fmt=*) '  Tracé XYV régulier '
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
  read(iule,fmt=*,iostat=ios) zztmp
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
! indta est le nombre de données.
!-------------------------------------------------
!
if(indta /= inx * iny) then
  write(*,fmt=*) ' '
  write(*,fmt=*) 'dd2gr/ERREUR: le nombre de données est ',indta
  write(*,fmt=*) '  or le champ REGX signale ',inx,' valeurs en X'
  write(*,fmt=*) '     le champ REGY signale ',iny,' valeurs en Y'
  write(*,fmt=*) '     soit ',inx,' * ',iny,' = ',inx*iny,' valeurs attendues!' 
  write(*,fmt=*) 
  call exit(1)
endif
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
zmoy=0.
zrcm=0.
rcmin=5.25e99
rcmax=-rcmin
do jy=1,iny
  do jx=1,inx
    read(iule,fmt=*) ztab(jx,jy)
    !
    !-------------------------------------------------
    ! Cumul pour moyenne, écart-type, etc.
    !-------------------------------------------------
    !
    rcmin=min(rcmin,ztab(jx,jy))
    rcmax=max(rcmax,ztab(jx,jy))
    zmoy=zmoy+ztab(jx,jy)
    zrcm=zrcm+ztab(jx,jy)*ztab(jx,jy)
  enddo
enddo
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
! Moyenne, écart-type.
!-------------------------------------------------
!
rcmoy=zmoy/real(inx*iny)
rcect=sqrt(max(0.,zrcm/real(inx*iny)-rcmoy*rcmoy))
rcrcm=sqrt(zrcm/real(inx*iny))
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
write(*,fmt=*) '  Champ en entrée: '
write(*,fmt=*) '    X : ',rxmin,' > ',rxmax
write(*,fmt=*) '    Y : ',rymin,' > ',rymax
write(*,fmt=*) '    V : ',minval(ztab),' > ',maxval(ztab)
write(*,fmt=*) ' '
!
!-------------------------------------------------
! Tracé.
!-------------------------------------------------
!
lllegende=.true.
if(trim(cglegendexy) == 'non') then
  lllegendexy=.false.
else
  lllegendexy=.true.
endif
rcmin=minval(ztab)
rcmax=maxval(ztab)
if(cgvmin /= cgindef) read(cgvmin,fmt=*) rcmin
if(cgvmax /= cgindef) read(cgvmax,fmt=*) rcmax
!
!-------------------------------------------------
! Si le min est égal au max on force un écart, pour ne pas planter en division par (max-min).
!-------------------------------------------------
!
call minmaxsecur
write(clcoord,fmt=*) rxmin,rxmax,rymin,rymax
write(*,fmt=*) '  Grille régulière d''entrée ',inx,' x ',iny
call img_gere_palette_auto(cgpal)
if(cgfpix(len_trim(cgfpix)-2:len_trim(cgfpix)) == 'svg') then
  !
  !-------------------------------------------------
  ! L'utilisateur veut produire un fichier vectoriel SVG.
  !-------------------------------------------------
  !
  llclose=.true.
  iirreg=1
  call svg_trac2d(zxirreg,zyirreg,zzirreg,iirreg,ztab,inx,iny,rcmin,rcmax,clcoord,cgpal,lllegende &
  & ,lllegendexy,cglegx,cglegy,nximage,nyimage,rindef,nrvb_indef,cgfpix,llclose)
else
  !
  !-------------------------------------------------
  ! L'utilisateur veut produire un fichier pixel (type GIF, PNG, JPG, etc).
  !-------------------------------------------------
  !
  !
  ! Epaisseur de trait des courbes en pixels.
  !
  !call img_trac2d(ztab,inx,iny,rcmin,rcmax,clcoord,cgpal,cgfond_de_carte_appel,cgtitre,lllegende &
  !& ,lllegendexy,cglegx,cglegy,nximage,nyimage,rindef,nrvb_indef,cgfpix)
endif
deallocate(ztab)
end
