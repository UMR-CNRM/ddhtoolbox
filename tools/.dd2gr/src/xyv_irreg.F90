subroutine xyv_irreg
! --------------------------------------------------------------
! **** ** Tracé image de données irrégulières (x,y,valeur).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2009-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use parametres
use lfagen
#include"implicit_r8i4.h"
real(kind=8), dimension(:), pointer :: zx,zxloc
real(kind=8), pointer :: zy(:),zyloc(:)
real(kind=8), pointer :: zz(:),zzloc(:)
real(kind=8), allocatable :: ztab(:,:)
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
write(*,fmt=*) '  Tracé XYV irrégulier '
write(*,fmt=*) ' '
write(*,fmt=*) '  Fichier de données lu: ',trim(cgfdta(1))
iule=22
!
! Le fichier est-il LFA?
!
call lfatest(iule,cgfdta(1),lllfa)
if(lllfa) then
  !
  !-------------------------------------------------
  ! Fichier LFA.
  !-------------------------------------------------
  !
  call lfalecgen(cgfdta(1),zxloc,zyloc,zzloc,indta)
else
  !
  !-------------------------------------------------
  ! Le fichier n'est pas LFA. C'est un fichier ASCII.
  !-------------------------------------------------
  !
  open(iule,file=cgfdta(1),form='formatted')
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  indta=0
  do
    read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zztmp
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
  write(*,fmt=*) indta,' triplets dans le fichier ASCII de données.'
  !
  !-------------------------------------------------
  ! Allocation.
  !-------------------------------------------------
  !
  allocate(zxloc(indta))
  allocate(zyloc(indta))
  allocate(zzloc(indta))
endif
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
if(.not. lllfa) open(iule,file=cgfdta(1),form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
indta1=indta
indta=0
indta_lfa=0
do
  if(lllfa) then
    indta_lfa=indta_lfa+1
    if(indta_lfa > indta1) exit
    zxtmp=zxloc(indta_lfa)
    zytmp=zyloc(indta_lfa)
    zztmp=zzloc(indta_lfa)
  else
    read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zztmp
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
  endif
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
  ! Est-on à l'intérieur du zoom en X?
  !-------------------------------------------------
  !
  if(rgxminl == rindef .and. rgxmaxl == rindef) then
    !
    ! Rien à faire.
    !
  elseif(rgxminl == rindef .and. rgxmaxl /= rindef) then
    !
    ! On ne teste que le max.
    !
    if(zxtmp > rgxmaxl) cycle
  elseif(rgxminl /= rindef .and. rgxmaxl == rindef) then
    !
    ! On ne teste que le min.
    !
    if(zxtmp < rgxminl) cycle
  elseif(rgxminl /= rindef .and. rgxmaxl /= rindef .and. rgxminl < rgxmaxl) then
    !
    ! Le min et le max sont founis, et on est dans le cas standard où min < max.
    !
    if(zxtmp < rgxminl) cycle
    if(zxtmp > rgxmaxl) cycle
  elseif(rgxminl /= rindef .and. rgxmaxl /= rindef .and. rgxminl > rgxmaxl) then
    !
    ! Le min et le max sont founis, et on est dans le cas particulier où min > max: l'utilisateur veut que l'axe aille en sens inverse de l'habituel. Par exemple: l'axe va de 0 à 1 de la droite vers la gauche.
    !
    if(zxtmp < rgxmaxl) cycle
    if(zxtmp > rgxminl) cycle
  else
    write(*,fmt=*)
    write(*,fmt=*) 'dd2gr/xyv_irreg/ERREUR: cas de zoom X non prévu !...'
    write(*,fmt=*)
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Est-on à l'intérieur du zoom en Y?
  !-------------------------------------------------
  !
  if(rgyminl == rindef .and. rgymaxl == rindef) then
    !
    ! Rien à faire.
    !
  elseif(rgyminl == rindef .and. rgymaxl /= rindef) then
    !
    ! On ne teste que le max.
    !
    if(zytmp > rgymaxl) cycle
  elseif(rgyminl /= rindef .and. rgymaxl == rindef) then
    !
    ! On ne teste que le min.
    !
    if(zytmp < rgyminl) cycle
  elseif(rgyminl /= rindef .and. rgymaxl /= rindef .and. rgyminl < rgymaxl) then
    !
    ! Le min et le max sont founis, et on est dans le cas standard où min < max.
    !
    if(zytmp < rgyminl) cycle
    if(zytmp > rgymaxl) cycle
  elseif(rgyminl /= rindef .and. rgymaxl /= rindef .and. rgyminl > rgymaxl) then
    !
    ! Le min et le max sont founis, et on est dans le cas particulier où min > max: l'utilisateur veut que l'axe aille en sens inverse de l'habituel. Par exemple: l'axe va de 0 à 1 de la droite vers la gauche.
    !
    if(zytmp < rgymaxl) cycle
    if(zytmp > rgyminl) cycle
  else
    write(*,fmt=*)
    write(*,fmt=*) 'dd2gr/xyv_irreg/ERREUR: cas de zoom Y non prévu !...'
    write(*,fmt=*)
    call exit(1)
  endif
  indta=indta+1
  zxloc(indta)=zxtmp
  zyloc(indta)=zytmp
  zzloc(indta)=zztmp
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
  write(*,fmt=*) 'dd2gr/xyv_irreg/ERREUR: aucun point dans le zoom demandé !...'
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
allocate(zz(indta))
do jcourbes=1,indta
  zx(jcourbes)=zxloc(jcourbes)
  zy(jcourbes)=zyloc(jcourbes)
  zz(jcourbes)=zzloc(jcourbes)
enddo
deallocate(zxloc)
deallocate(zyloc)
deallocate(zzloc)
!
!-------------------------------------------------
! Interpolation sur une grille régulière.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Calcul du nombre de points en X et Y sur l'image de sortie.
!-------------------------------------------------
!
call repere_donnees_reg(zx,zy,indta,ipasx,ipasy)
if(lgpointage) then
  !
  !-------------------------------------------------
  ! Dans le cas de pointage, pas d'interpolation à faire.
  ! On dimensionne le tableau interpolé au minimum.
  !-------------------------------------------------
  !
  inx=1
  iny=1
elseif(cginterpole == cgindef) then
  !
  !-------------------------------------------------
  ! L'utilisateur n'a pas imposé ce nombre de points.
  !-------------------------------------------------
  !
  if(ipasx > 0) then
    !
    !-------------------------------------------------
    ! On déduit ce nb de points de la régularité de la grille.
    !-------------------------------------------------
    !
    inx=ipasx
    iny=ipasy
  else
    !
    !-------------------------------------------------
    ! On le déduit de la taille des données.
    ! On part du principe qu'il y a 2 fois plus de points en X qu'en Y, et qu'on
    ! met autant de points interpolés que ce que donnerait la simple racine du nombre
    ! de points irréguliers.
    !-------------------------------------------------
    !
    iny=nint(sqrt(real(indta/2)))
    !iny=max(60,min(400,iny))
    iny=max(1,min(nyimage,iny))
    inx=2*iny
  endif
else
  !
  !-------------------------------------------------
  ! L'utilisateur a imposé ce nombre de points.
  !-------------------------------------------------
  !
  read(cginterpole,fmt=*) inx,iny
endif
!
!-------------------------------------------------
! Allocation du tableau recevant les données régulières.
!-------------------------------------------------
!
allocate(ztab(inx,iny))
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
! S'ils sont imposés par l'utilisateur, on force ces min/max à être ceux de dd2gr.
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
! Interpolation de la grille irrégulière vers la grille régulière.
!-------------------------------------------------
!
if(.not.lgpointage) then
  write(*,fmt=*) ' '
  write(*,fmt=*) '  Interpolation vers une grille régulière ',inx,' x ',iny
  call interpole(rindef,lgextrapolation,zx,zy,zz,indta,inx,iny,ztab)
  !
  !-------------------------------------------------
  ! Recherche des extrêmes et statistiques du champ interpolé.
  !-------------------------------------------------
  !
  rcmin=rindef
  rcmax=rindef
  ival=0
  zmoy=0.
  zrcm=0.
  do jx=1,inx
    do jy=1,iny
      if(ztab(jx,jy) /= rindef) then
        if(rcmin == rindef) then
          rcmin=ztab(jx,jy)
          rcmax=ztab(jx,jy)
        else
          rcmin=min(ztab(jx,jy),rcmin)
          rcmax=max(ztab(jx,jy),rcmax)
        endif
        !
        !-------------------------------------------------
        ! Cumul pour moyenne, écart-type, etc.
        !-------------------------------------------------
        !
        zmoy=zmoy+ztab(jx,jy)
        zrcm=zrcm+ztab(jx,jy)*ztab(jx,jy)
        ival=ival+1
      endif
    enddo
  enddo
else
  !
  !-------------------------------------------------
  ! Recherche des extrêmes et statistiques du champ irrégulier d'entrée.
  !-------------------------------------------------
  !
  rcmin=1.e70
  rcmax=-rcmin
  ival=0
  zmoy=0.
  zrcm=0.
  do jdta=1,indta
    if(abs(zz(jdta)-rgpointage) > 0.01) then
      rcmin=min(zz(jdta),rcmin)
      rcmax=max(zz(jdta),rcmax)
    else
      lgpoil=.true.
    endif
    !
    !-------------------------------------------------
    ! Cumul pour moyenne, écart-type, etc.
    !-------------------------------------------------
    !
    zmoy=zmoy+zz(jdta)
    zrcm=zrcm+zz(jdta)*zz(jdta)
    ival=ival+1
  enddo
endif
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
rcmoy=zmoy/real(ival)
rcect=sqrt(max(0.,zrcm/real(ival)-rcmoy*rcmoy))
rcrcm=sqrt(zrcm/real(ival))
!
!-------------------------------------------------
! Min/max de zz, en tenant compte que rgpointage est une valeur spéciale: pointage de points noirs.
!-------------------------------------------------
!
zzmin_irreg=1.e70
zzmax_irreg=-zzmin_irreg
do jz=1,indta
  if(abs(zz(jz)-rgpointage) > 0.01) then
    zzmin_irreg=min(zzmin_irreg,zz(jz))
    zzmax_irreg=max(zzmax_irreg,zz(jz))
  endif
enddo
!
!-------------------------------------------------
! Tracé.
!-------------------------------------------------
!
write(*,fmt=*) '  Champ en entrée: '
write(*,fmt=*) '    X : ',rxmin,' > ',rxmax
write(*,fmt=*) '    Y : ',rymin,' > ',rymax
write(*,fmt=*) '    V irrégulier : ',zzmin_irreg,' > ',zzmax_irreg
if(.not.lgpointage) write(*,fmt=*) '    V régulier   : ',rcmin,' > ',rcmax
write(*,fmt=*) ' '
lllegende=.true.
if(trim(cglegendexy) == 'non') then
  lllegendexy=.false.
else
  lllegendexy=.true.
endif
if(cgvmin /= cgindef) read(cgvmin,fmt=*) rcmin
if(cgvmax /= cgindef) read(cgvmax,fmt=*) rcmax
!
!-------------------------------------------------
! Si le min est égal au max on force un écart, pour ne pas planter en division par (max-min).
!-------------------------------------------------
!
call minmaxsecur
write(clcoord,fmt=*) rxmin,rxmax,rymin,rymax
call img_gere_palette_auto(cgpal)
if(cgfpix(len_trim(cgfpix)-2:len_trim(cgfpix)) == 'svg') then
  !
  !-------------------------------------------------
  ! L'utilisateur veut produire un fichier vectoriel SVG.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Remplissage de rectangles colorés.
  !-------------------------------------------------
  !
  llclose=.true.
  call svg_trac2d(zx,zy,zz,indta,ztab,inx,iny,rcmin,rcmax,clcoord,cgpal,lllegende &
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
end
