program jux
! --------------------------------------------------------------
! Juxtaposition de deux images ppm.
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
INTEGER,ALLOCATABLE :: irvb_lit(:,:,:)
INTEGER,ALLOCATABLE :: irvb_ecr(:,:,:)
CHARACTER*200 CLPPM1
CHARACTER*200 CLPPM2
CHARACTER*200 CLPPM3
INTEGER :: IARG
INTEGER :: IARGC
INTEGER :: INX
INTEGER :: INX1
INTEGER :: INX2
INTEGER :: INY
INTEGER :: INY1
INTEGER :: INY2
INTEGER :: IPOSX1
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: IPOSY2,iopt
INTEGER :: JCOUL,ix,iy,inx2ana,iny2ana
INTEGER :: JX,ix_lit,iy_lit,ix_ecr,iy_ecr
INTEGER :: JY
REAL :: ZRAPAE
REAL :: ZRAPA_DROITE
REAL :: ZRAPA_SOUS,zrapa2
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
if(iarg /= 3) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Juxtaposition de deux images PPM sur une seule.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: img2en1 PPM1 PPM2 PPM_SORTIE'
  write(*,'(a)') ' '
  write(*,'(a)') 'La 1ère est prioritaire sur la 2e: la 1ère sera'
  write(*,'(a)') 'en effet copiée sans modification,'
  write(*,'(a)') 'tandis que la 2e sera forcée à entrer dans'
  write(*,'(a)') 'un cadre de même taille que celui de la 1ère,'
  write(*,'(a)') 'avec respect toutefois de son rapport d''aspect.'
  write(*,'(a)') 'La 2ème sera donc anamorphosée par sur ou sous-échantillonnage.'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getargp(1,clppm1)
call getargp(2,clppm2)
call getargp(3,clppm3)
print*,'img2en1:'
!
!-------------------------------------------------
! Lecture de la taille des ppm d'entrée.
!-------------------------------------------------
!
call img_taille(clppm1,inx1,iny1)
call img_taille(clppm2,inx2,iny2)
print*,'	image 1 d''entrée: (',inx1,',',iny1,')'
print*,'	image 2 d''entrée: (',inx2,',',iny2,')'
!
!-------------------------------------------------
! Taille du ppm de sortie: faut-il
! ajouter l'image 2 à droite ou sous la 1?
! On veut que le rapport d'aspect de l'image combinée soit le plus
! proche possible du rapport d'aspect de l'écran.
!-------------------------------------------------
!
zrapae=log(0.76) ! rapport d'aspect usuel des écrans.
zrapa_droite=abs(log(real(iny1)/2./real(inx1))-zrapae)
zrapa_sous=abs(log(2.*real(iny1)/real(inx1))-zrapae)
zrapa2=real(iny2)/real(inx2) ! rapport d'aspect de l'image 2.
if(zrapa_sous < zrapa_droite) then
  !
  !-------------------------------------------------
  ! L'image 2 sera sous l'image 1.
  !-------------------------------------------------
  !
  iposx2=1 ; iposy2=iny1+1 ! position de l'image 2 dans l'image finale.
  print*,'	l''image 2 sera sous l''image 1.'
  iopt=0
else
  !
  !-------------------------------------------------
  ! L'image 2 sera à droite de l'image 1.
  !-------------------------------------------------
  !
  iposx2=inx1+1 ; iposy2=1 ! position de l'image 2 dans l'image finale.
  print*,'	l''image 2 sera à droite de l''image 1.'
  iopt=1
endif
!
!-------------------------------------------------
! Tailles X et Y de l'image 2 anamorphosée pour tenir
! dans un rectangle de taille (inx1,inx2).
!-------------------------------------------------
!
iny2ana=min(iny1,nint(real(inx1)/real(inx2)*real(iny2)))
inx2ana=min(inx1,nint(real(iny2ana)/zrapa2))
if(iny2ana > iny2) then
  print*,'	l''image 2 sera sur-échantillonnée d''un facteur ',real(iny2ana)/real(iny2),'.'
  print*,'	'
elseif(iny2ana < iny2) then
  print*,'	l''image 2 sera sous-échantillonnée d''un facteur ',real(iny2)/real(iny2ana),'.'
endif
print*,'	image 1 de sortie: (',inx1,',',iny1,')'
print*,'	image 2 de sortie: (',inx2ana,',',iny2ana,')'
if(iopt == 0) then
  inx=inx1
  iny=iny1+iny2ana
else
  inx=inx1+inx2ana
  iny=iny1
endif
print*,'	taille de l''image 3 résultante: (',inx,',',iny,')'
!
!-------------------------------------------------
! Initialisation du fond.
!-------------------------------------------------
!
allocate(irvb_ecr(3,inx,iny))
irvb_ecr=190
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'	lecture de l''image 1...'
allocate(irvb_lit(3,inx1,iny1))
call img_lec(clppm1,inx1,iny1,irvb_lit)
do jy=1,iny1
  do jx=1,inx1
    do jcoul=1,3
      irvb_ecr(jcoul,jx,jy)=irvb_lit(jcoul,jx,jy)
    enddo
  enddo
enddo
deallocate(irvb_lit)
!
!-------------------------------------------------
! Lecture de l'image 2.
!-------------------------------------------------
!
print*,'	lecture de l''image 2...'
allocate(irvb_lit(3,inx2,iny2))
call img_lec(clppm2,inx2,iny2,irvb_lit)
!
!-------------------------------------------------
! Echantillonnage de l'image 2.
!-------------------------------------------------
!
print*,'	échantillonnage de l''image 2...'
do jy=1,iny2ana
  iy_lit=nint( 1. +real(iny2-1)*real(jy-1)/real(iny2ana-1))
  iy_ecr=jy-1+iposy2
  do jx=1,inx2ana
    ix_lit=nint( 1. +real(inx2-1)*real(jx-1)/real(inx2ana-1))
    ix_ecr=jx-1+iposx2
    do jcoul=1,3
      irvb_ecr(jcoul,ix_ecr,iy_ecr)=irvb_lit(jcoul,ix_lit,iy_lit)
    enddo
  enddo
enddo
deallocate(irvb_lit)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture du PPM résultant...'
call img_ecr(clppm3,inx,iny,irvb_ecr)
print*,'	fichier écrit: ',clppm3(1:len_trim(clppm3))
print*,' '
end
