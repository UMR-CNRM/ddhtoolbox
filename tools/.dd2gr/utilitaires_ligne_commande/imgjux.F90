program imgjux
! --------------------------------------------------------------
! Juxtaposition de n images en une seule.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------


!IMPLICIT NONE
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
CHARACTER*200 CLPPM1
CHARACTER*200 CLPPM2
CHARACTER*200 CLPPM3,clx1,cly1
CHARACTER*200 clopt
INTEGER :: IARG
INTEGER :: IARGC
INTEGER :: INX,icx1,icy1,jarg
INTEGER :: INX2
INTEGER :: INY
INTEGER :: INY2
INTEGER :: IPOSX1,iloc(3)
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: IPOSY2,iopt
INTEGER :: JCOUL,ix,iy,inx2ana,iny2ana
INTEGER :: JX,inx1,iny1,ix2,iy2,ityps(4)
INTEGER :: JY
REAL :: zopac
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(mod(iarg,3) /= 1.or.iarg < 4) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Juxtaposition de n images en une seule.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgjux IMG1 X1 Y1 [... IMGn Xn Yn] IMGSORTIE'
  write(*,'(a)') ' '
  write(*,'(a)') 'où IMGi est le nom du fichier-image i,'
  write(*,'(a)') 'Xi et Yi les coordonnées du coin haut-gauche de IMGi'
  write(*,'(a)') 'dans l''image finale.'
  write(*,'(a)') 'Les images IMGi sont recopiées bit à bit sur l''image de sortie, i.e. sans aucune modification.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Exemple: imgjux toto.ppm 1 1 tata.ppm 55 45 image_res.ppm'
  write(*,'(a)') ' '
  call exit(1)
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
print*,'imgjux:'
call getargp(iarg,clppm2)
!
!-------------------------------------------------
! Détermination de la dimension de l'image de sortie.
!-------------------------------------------------
!
inx2=0
iny2=0
do jarg=1,iarg-1,3
  !
  !-------------------------------------------------
  ! Ligne de commande de la jarg-ème image.
  !-------------------------------------------------
  !
  call getargp(jarg,clppm1)
  call getargp(jarg+1,clx1) ; read(clx1,fmt=*) icx1
  call getargp(jarg+2,cly1) ; read(cly1,fmt=*) icy1
  call img_taille(clppm1,inx1,iny1)
  inx2=max(inx2,icx1+inx1-1)
  iny2=max(iny2,icy1+iny1-1)
enddo
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb2(3,inx2,iny2))
!
!-------------------------------------------------
! Initialisation d'un fond gris.
!-------------------------------------------------
!
irvb2=190
!
!-------------------------------------------------
! Boucle sur les images à surimposer.
!-------------------------------------------------
!
do jarg=1,iarg-1,3
  !
  !-------------------------------------------------
  ! Ligne de commande de la jarg-ème image.
  !-------------------------------------------------
  !
  call getargp(jarg,clppm1)
  call getargp(jarg+1,clx1) ; read(clx1,fmt=*) icx1
  call getargp(jarg+2,cly1) ; read(cly1,fmt=*) icy1
  call img_taille(clppm1,inx1,iny1)
  !
  !-------------------------------------------------
  ! Allocation de l'image à lire.
  !-------------------------------------------------
  !
  allocate(irvb1(3,inx1,iny1))
  !
  !-------------------------------------------------
  ! Lecture de l'image 1.
  !-------------------------------------------------
  !
  print*,'	lecture de l''image ',clppm1(1:len_trim(clppm1)),', de taille (',inx1,',',iny1,')...'
  call img_lec(clppm1,inx1,iny1,irvb1)
  !
  !-------------------------------------------------
  ! Surimposition sur l'image 2.
  !-------------------------------------------------
  !
  zopac=1.
  ityps(1)=0
  iloc(1)=0 ; iloc(2)=icx1 ; iloc(3)=icy1
  print*,'	surimposition de cette image au lieu (',icx1,',',icy1,')...'
  call img_surimpose_image(inx1,iny1,irvb1,zopac,ityps,iloc,inx2,iny2,irvb2)
  !
  !-------------------------------------------------
  ! Désallocation de l'image à lire.
  !-------------------------------------------------
  !
  deallocate(irvb1)
enddo
!
!-------------------------------------------------
! Ecriture de l'image finale.
!-------------------------------------------------
!
print*,'	écriture de l''image finale ',clppm2(1:len_trim(clppm2)),', de taille (',inx2,',',iny2,')...'
call img_ecr(clppm2,inx2,iny2,irvb2)
print*,'	fichier écrit: ',clppm2(1:len_trim(clppm2))
print*,' '
end
