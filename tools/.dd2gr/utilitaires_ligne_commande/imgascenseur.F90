program jux
! --------------------------------------------------------------
! Combinaison de deux images ppm.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)

INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
INTEGER,ALLOCATABLE :: irvb3(:,:,:)
CHARACTER*200 CLPPM1
CHARACTER*200 CLPPM2
CHARACTER*200 CLPPM3
INTEGER :: IARG
INTEGER :: IARGC
INTEGER :: INX3,iny3
INTEGER :: INX1
INTEGER :: INX2
INTEGER :: INY1
INTEGER :: INY2
INTEGER :: IPOSX1
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: irvb_bord(3)
INTEGER :: IPOSY2,iopt
INTEGER :: jcoul,ix,iy,inx2ana,iny2ana
INTEGER :: JX,ix1,iy1,ix3,iy3
INTEGER :: JY
REAL :: ZRAPAE
REAL :: ZRAPA_DROITE
REAL :: ZRAPA_SOUS,zrapa2
character*200 clmot(180)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
clcgau='100,100,100'
clcmil='000,000,000'
clcdro='255,255,255'
clfrac1=' '
clfrac2=' '
clx=' '
cly=' '
clppm=' '
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Crée un ascenseur, avec 3 zones colorées'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgascenseur [-gRVB -mRVB -dRVB] FRAC1 FRAC2 XPIX YPIX IMGSORTIE'
  write(*,'(9a)') ' '
  write(*,'(9a)') '	FRAC1 et FRAC2: de 0 à FRAC1: couleur gauche, de FRAC1 à FRAC2 couleur milieu, ailleurs couleur droite.'
  write(*,'(9a)') '	FRAC1 et FRAC2 peuvent être éventuellement égaux (pas de zone milieu).'
  write(*,'(9a)') '	FRAC1 et FRAC2 peuvent être éventuellement nuls (zone droite totale).'
  write(*,'(9a)') '	FRAC2 doit être impérativement >= FRAC1.'
  write(*,'(9a)') '	XPIX et YPIX sont la taille de sortie de l''image en pixels.'
  write(*,'(9a)') '	-g couleur GAUCHE en RVB.'
  write(*,'(9a)') '	  Défaut: ',trim(clcgau)
  write(*,'(9a)') '	-m couleur MILIEU en RVB.'
  write(*,'(9a)') '	  Défaut: ',trim(clcmil)
  write(*,'(9a)') '	-d couleur DROITE en RVB.'
  write(*,'(9a)') '	  Défaut: ',trim(clcdro)
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Exemple: imgascenseur -g255,255,255 -m0,255,255 -d255,255,255 0.3 0.92 300 100 ASC.gif'
  write(*,'(9a)') ' '
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
do jarg=1,iarg
  call getargp(jarg,clarg)
  if(clarg(1:2) == '-g') then
    clcgau=clarg(3:)
  elseif(clarg(1:2) == '-m') then
    clcmil=clarg(3:)
  elseif(clarg(1:2) == '-d') then
    clcdro=clarg(3:)
  elseif(clfrac1 == ' ') then
    clfrac1=clarg
  elseif(clfrac2 == ' ') then
    clfrac2=clarg
  elseif(clx == ' ') then
    clx=clarg
  elseif(cly == ' ') then
    cly=clarg
  elseif(clppm == ' ') then
    clppm=clarg
  else
  endif
enddo
!
!-------------------------------------------------
! Lecture des valeurs RVB.
!-------------------------------------------------
!
call casc(clcgau,10,clmot,imot)
read(clmot(1),fmt=*) irgau
read(clmot(2),fmt=*) ivgau
read(clmot(3),fmt=*) ibgau
call casc(clcmil,10,clmot,imot)
read(clmot(1),fmt=*) irmil
read(clmot(2),fmt=*) ivmil
read(clmot(3),fmt=*) ibmil
call casc(clcdro,10,clmot,imot)
read(clmot(1),fmt=*) irdro
read(clmot(2),fmt=*) ivdro
read(clmot(3),fmt=*) ibdro
!
!-------------------------------------------------
! Conversions de type.
!-------------------------------------------------
!
read(clfrac1,fmt=*) zfrac1
read(clfrac2,fmt=*) zfrac2
read(clx,fmt=*) inx3
read(cly,fmt=*) iny3
print*,'imgascenseur:'
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb3(3,inx3,iny3))
!
!-------------------------------------------------
! Combinaison des deux images 1 et 2 en l'image de sortie 3.
!-------------------------------------------------
!
if(zfrac2 < zfrac1) then
  write(*,fmt=*) 'imgascenseur/ERREUR: il faut FRAC2 >= FRAC1!...'
  stop 'call abort'
endif
if(zfrac2 /= zfrac1 .and. 1./real(inx3) > zfrac2 - zfrac1) then
  !
  !-------------------------------------------------
  ! On est dans le cas où l'écart entre zfrac1 et zfrac2
  ! est si faible qu'aucun pixel de l'image de sortie
  ! risque de n'être validé comme étant dans la zone centre.
  ! Or si l'utilisateur a saisi zfrac2 différent de zfrac1,
  ! c'est qu'il souhaite au moins un pixel ici!...
  ! On élargit du coup artificiellement zfrac2.
  !-------------------------------------------------
  !
  zfrac2=zfrac1+1.5/real(inx3)
endif
do jx=1,inx3
  do jy=1,iny3
    zfrac=(real(jx-1)+0.5)/real(inx3)
    if(zfrac < zfrac1) then
      !
      !-------------------------------------------------
      ! Gauche.
      !-------------------------------------------------
      !
      irvb3(1,jx,jy)=irgau
      irvb3(2,jx,jy)=ivgau
      irvb3(3,jx,jy)=ibgau
    elseif(zfrac < zfrac2) then
      !
      !-------------------------------------------------
      ! Milieu.
      !-------------------------------------------------
      !
      irvb3(1,jx,jy)=irmil
      irvb3(2,jx,jy)=ivmil
      irvb3(3,jx,jy)=ibmil
    else
      !
      !-------------------------------------------------
      ! Droite.
      !-------------------------------------------------
      !
      irvb3(1,jx,jy)=irdro
      irvb3(2,jx,jy)=ivdro
      irvb3(3,jx,jy)=ibdro
    endif
  enddo
enddo
!
!-------------------------------------------------
! Ajout d'une bordure noire.
!-------------------------------------------------
!
irvb_bord(:)=0
call img_bordure(irvb3,inx3,iny3,irvb_bord)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
call img_ecr(clppm,inx3,iny3,irvb3)
print*,'	fichier écrit: ',trim(clppm)
print*,' '
end
#include"../../caracteres.F90"
