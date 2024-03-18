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
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
INTEGER,ALLOCATABLE :: irvb3(:,:,:)
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
! Tailles X et Y de l'image 2 anamorphosée pour tenir
! dans un rectangle de taille (inx1,inx2).
!-------------------------------------------------
!
iny3=iny1
inx3=inx1
print*,'	taille de l''image 3 résultante: (',inx3,',',iny3,')'
!
!-------------------------------------------------
! Initialisation du fond.
!-------------------------------------------------
!
allocate(irvb3(3,inx3,iny3))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'	lecture de l''image 1...'
allocate(irvb1(3,inx1,iny1))
call img_lec(clppm1,inx1,iny1,irvb1)
!-------------------------------------------------
! Lecture de l'image 2.
!-------------------------------------------------
!
print*,'	lecture de l''image 2...'
allocate(irvb2(3,inx2,iny2))
call img_lec(clppm2,inx2,iny2,irvb2)
!
!-------------------------------------------------
! Echantillonnage de l'image 2.
!-------------------------------------------------
!
print*,'	échantillonnage de l''image 2...'
ix1=1281
ix2=1468
do jy=1,iny3
  do jx=1,ix1
    do jcoul=1,3
      irvb3(jcoul,jx,jy)=irvb2(jcoul,jx,jy)
    enddo
  enddo
  do jx=ix1+1,ix2-1
    zfrac=real(jx-ix1-1)/real(ix2-1-ix1-1)
    do jcoul=1,3
      irvb3(jcoul,jx,jy)=max(0,min(255,nint(zfrac*real(irvb1(jcoul,jx,jy))+(1.-zfrac)*real(irvb2(jcoul,jx,jy)))))
    enddo
  enddo
  do jx=ix2,inx3
    do jcoul=1,3
      irvb3(jcoul,jx,jy)=irvb1(jcoul,jx,jy)
    enddo
  enddo
enddo
deallocate(irvb1)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture du PPM résultant...'
call img_ecr(clppm3,inx3,iny3,irvb3)
print*,'	fichier écrit: ',clppm3(1:len_trim(clppm3))
print*,' '
end
