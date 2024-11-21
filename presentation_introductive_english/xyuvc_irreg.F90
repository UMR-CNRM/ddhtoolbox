subroutine xyuvc_irreg(kximage,kyimage)
! --------------------------------------------------------------
! **** ** Tracé image de données irrégulières (x,y,u,v,champ).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2015-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kximage,kyimage: tailles X,Y en pixels du fichier SVG de sortie.
! En sortie:
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
real(kind=8), allocatable :: zx(:),zxloc(:)
real(kind=8), allocatable :: zy(:),zyloc(:)
real(kind=8), allocatable :: zu(:),zuloc(:)
real(kind=8), allocatable :: zv(:),zvloc(:)
real(kind=8), allocatable :: zc(:),zcloc(:)
real(kind=8), allocatable :: ztab(:,:)
real(kind=8) :: zxirreg(1)
real(kind=8) :: zyirreg(1)
real(kind=8) :: zzirreg(1)
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
write(*,fmt=*) '  Tracé XYUVC irrégulier '
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
  read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zutmp,zvtmp,zctmp
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
allocate(zuloc(indta))
allocate(zvloc(indta))
allocate(zcloc(indta))
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
  read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zutmp,zvtmp,zctmp
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
  zuloc(indta)=zutmp
  zvloc(indta)=zvtmp
  zcloc(indta)=zctmp
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
  write(*,fmt=*) 'dd2gr/xyuvc_irreg/ERREUR: aucun point dans le zoom demandé !...'
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
allocate(zu(indta))
allocate(zv(indta))
allocate(zc(indta))
do jcourbes=1,indta
  zx(jcourbes)=zxloc(jcourbes)
  zy(jcourbes)=zyloc(jcourbes)
  zu(jcourbes)=zuloc(jcourbes)
  zv(jcourbes)=zvloc(jcourbes)
  zc(jcourbes)=zcloc(jcourbes)
enddo
deallocate(zxloc)
deallocate(zyloc)
deallocate(zuloc)
deallocate(zvloc)
deallocate(zcloc)
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
if(cginterpole == cgindef) then
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
    ! On le déduit de la taille des données.
    inx=nint(sqrt(real(indta)))
    inx=min(200,inx)
    iny=inx/2
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

if(rxmin < rxmax) then
  isignx=1
else
  isignx=-1
endif
if(rymin < rymax) then
  isigny=1
else
  isigny=-1
endif
!
!-------------------------------------------------
! Interpolation de la grille irrégulière vers la grille régulière.
!-------------------------------------------------
!
write(*,fmt=*) ' '
write(*,fmt=*) '  Interpolation vers une grille régulière ',inx,' x ',iny
call interpole(rindef,lgextrapolation,zx,zy,zc,indta,inx,iny,ztab)
!
!-------------------------------------------------
! Recherche des extrêmes du champ interpolé.
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
! Tracé.
!-------------------------------------------------
!
zcmin_irreg=minval(zc)
zcmax_irreg=maxval(zc)
write(*,fmt=*) '  Champ en entrée: '
write(*,fmt=*) '    X : ',rxmin,' > ',rxmax
write(*,fmt=*) '    Y : ',rymin,' > ',rymax
write(*,fmt=*) '    V irrégulier : ',zcmin_irreg,' > ',zcmax_irreg
write(*,fmt=*) '    V régulier   : ',rcmin,' > ',rcmax
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
  ! Tracé du champ coloré.
  !-------------------------------------------------
  !
  llclose=.false.
  iirreg=1
  call svg_trac2d(zxirreg,zyirreg,zzirreg,iirreg,ztab,inx,iny,rcmin,rcmax,clcoord,cgpal,lllegende &
  & ,lllegendexy,cglegx,cglegy,nximage,nyimage,pindef,nrvb_indef,cgfpix,llclose)
  !
  !-------------------------------------------------
  ! Tracé des flèches.
  !-------------------------------------------------
  !
  zxn=rxmin      ; zxx=rxmax     
  zyn=rymin      ; zyx=rymax      
  zun=minval(zu) ; zux=maxval(zu)
  zvn=minval(zv) ; zvx=maxval(zv)
  zmodule_max=abs(zun)
  zmodule_max=max(zmodule_max,abs(zux))
  zmodule_max=max(zmodule_max,abs(zvn))
  zmodule_max=max(zmodule_max,abs(zvx))
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Flèches du champ (u,v). -->'
  do jdta=1,indta
    iband=(jdta-1)/105+1
    if(modulo(iband,4) /= 0) cycle
    if(modulo(jdta,2) /= 0) cycle
    if(modulo(jdta-1,105) == 0) cycle
    !
    !-------------------------------------------------
    ! Base de la flèche.
    !-------------------------------------------------
    !
    zfrac=(zx(jdta)-zxn)/(zxx-zxn)
    zxtrac=rxt+rlxt*zfrac
    zfrac=(zy(jdta)-zyn)/(zyx-zyn)
    zytrac=ryt+rlyt*(1.-zfrac)
    if(zfrac > 0.99 ) cycle
    !
    !-------------------------------------------------
    ! Extrémité de la flèche.
    !-------------------------------------------------
    !
    if(rech_vec == rindef) then
      !
      !-------------------------------------------------
      ! L'utilisateur n'impose pas l'échelle du module des flèches.
      !-------------------------------------------------
      !
      zconv=0.10*rlxt
      zxextf=zxtrac+zconv*real(isignx)*zu(jdta)/zmodule_max
      zyextf=zytrac-zconv*real(isigny)*zv(jdta)/zmodule_max
    else
      !
      !-------------------------------------------------
      ! L'utilisateur impose l'échelle du module des flèches.
      ! rech_vec est alors le facteur de conversion imposé, en pixels / UV, où UV est l'unité des champs u et v dans le fichier d'entrée.
      !-------------------------------------------------
      !
      zxextf=zxtrac+rech_vec*real(isignx)*zu(jdta)
      zyextf=zytrac-rech_vec*real(isigny)*zv(jdta)
    endif
    !
    !-------------------------------------------------
    ! Bords de la pointe de la flèche.
    !-------------------------------------------------
    !
    call poif(zxtrac,zytrac,zxextf,zyextf,zxempd,zyempd,zxempg,zyempg)
    !write(*,fmt=*) 'flèche de ',zxtrac,zytrac,' à ',zxextf,zyextf
    !write(*,fmt=*) 'puis pointe d ',zxempd,zyempd
    !write(*,fmt=*) 'puis pointe g ',zxempg,zyempg
    !
    !-------------------------------------------------
    ! Largeur de la flèche.
    !-------------------------------------------------
    !
    zwidth=1.1e-3*rlxsvg*0.75
    !
    !-------------------------------------------------
    ! Ecriture d'une ligne polygonale.
    !-------------------------------------------------
    !
    if(cgcoul_vec == cgindef) then
      clcoul='black'
    else
      clcoul=trim(cgcoul_vec)
    endif
    clpoin=cgindef
    call svg_trait(nulsvg,zxtrac,zxextf,zytrac,zyextf,zwidth,clcoul,clpoin)
    call svg_trait(nulsvg,zxextf,zxempd,zyextf,zyempd,zwidth,clcoul,clpoin)
    call svg_trait(nulsvg,zxextf,zxempg,zyextf,zyempg,zwidth,clcoul,clpoin)
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier SVG.
  !-------------------------------------------------
  !
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Indicateur de fin de fichier SVG. -->'
  write(nulsvg,fmt='(a)') '</svg>'
  close(nulsvg)
  !
  !-------------------------------------------------
  ! Epilogue.
  !-------------------------------------------------
  !
  write(*,fmt=*) ' '
  write(*,fmt=*) '  Fichier SVG écrit : ',trim(cgfpix)
  write(*,fmt=*) ' '

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
  !& ,lllegendexy,cglegx,cglegy,nximage,nyimage,pindef,nrvb_indef,cgfpix)
endif
end
