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
INTEGER :: IPOSY2,iopt
INTEGER :: jcoul,ix,iy,inx2ana,iny2ana
INTEGER :: JX,ix1,iy1,ix3,iy3
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
if(iarg /= 5) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Ajout conditionnel:'
  write(*,'(a)') 'Ajoute l''image 2 sur la 1, le coin haut gauche de 2 étant au lieu (X1,Y1) de 1, et là où les pixels de la 1 sont blancs.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgajcond IMG1 IMG2 X1 Y1 IMGSORTIE'
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
call getargp(3,clx1)
call getargp(4,cly1)
call getargp(5,clppm3)
read(clx1,fmt=*) iposx
read(cly1,fmt=*) iposy
print*,'imgajcond:'
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'	lecture de l''image 1...'
call img_taille(clppm1,inx1,iny1)
print*,'	image 1 d''entrée: (',inx1,',',iny1,')'
allocate(irvb1(3,inx1,iny1))
call img_lec(clppm1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Lecture de l'image 2.
!-------------------------------------------------
!
print*,'	lecture de l''image 2...'
call img_taille(clppm2,inx2,iny2)
print*,'	image 2 d''entrée: (',inx2,',',iny2,')'
allocate(irvb2(3,inx2,iny2))
call img_lec(clppm2,inx2,iny2,irvb2)
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
inx3=inx1 ; iny3=iny1
allocate(irvb3(3,inx3,iny3))
!
!-------------------------------------------------
! Combinaison des deux images 1 et 2 en l'image de sortie 3.
!-------------------------------------------------
!
print*,'	combinaison des deux images...'
do jx=1,inx3
  do jy=1,iny3
    if(jx >= iposx .and. jx <= iposx+inx2-1 &
    & .and. jy >= iposy .and. jy <= iposy+iny2-1) then
      !
      !-------------------------------------------------
      ! On est dans le sous-carré de l'image 2.
      !-------------------------------------------------
      !
      if(irvb1(1,jx,jy) > 250 .and. irvb1(2,jx,jy) > 250 .and. irvb1(3,jx,jy) > 250) then
        !
        !-------------------------------------------------
        ! L'image 1 est claire en ce point.
        ! On prend le pixel de l'image 2.
        !-------------------------------------------------
        !
        do jc=1,3
          irvb3(jc,jx,jy)=irvb2(jc,jx-iposx+1,jy-iposy+1)
        enddo
      else
        !
        !-------------------------------------------------
        ! L'image 1 n'est pas claire en ce point.
        ! On prend le pixel de l'image 1.
        !-------------------------------------------------
        !
        do jc=1,3
          irvb3(jc,jx,jy)=irvb1(jc,jx,jy)
        enddo
      endif
    else
      !
      !-------------------------------------------------
      ! On est hors sous-carré de l'image 2.
      ! Simple recopie des pixels de l'image 1.
      !-------------------------------------------------
      !
      do jc=1,3
        irvb3(jc,jx,jy)=irvb1(jc,jx,jy)
      enddo
    endif
  enddo
enddo
!
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture du fichier résultant...'
print*,'	image 3 de sortie: (',inx3,',',iny3,')'
call img_ecr(clppm3,inx3,iny3,irvb3)
print*,'	fichier écrit: ',clppm3(1:len_trim(clppm3))
print*,' '
end
