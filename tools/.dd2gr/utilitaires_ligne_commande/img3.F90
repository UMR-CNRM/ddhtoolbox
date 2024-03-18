program img3
! --------------------------------------------------------------
! 
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
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 1) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Séparation de l''image d''entrée en ses 3 composantes; en sortie 3 fichiers: R.ppm, V.ppm, B.ppm.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: img3 IMG'
  write(*,'(9a)') ' '
  stop
endif
print*,'img3:'
call getargp(1,climg1)
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
print*,'	image ',climg1(1:len_trim(climg1))
print*,'		taille (',inx1,',',iny1,')'
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb2(3,inx1,iny1))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! On ne garde que le R.
!-------------------------------------------------
!
print*,'		modification de l''image d''entrée'
do jy=1,iny1
  do jx=1,inx1
    irvb2(1,jx,jy)=irvb1(1,jx,jy)
    irvb2(2,jx,jy)=irvb1(1,jx,jy)
    irvb2(3,jx,jy)=irvb1(1,jx,jy)
  enddo
enddo
climg2='R.ppm'
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
!
!-------------------------------------------------
! On ne garde que le V.
!-------------------------------------------------
!
print*,'		modification de l''image d''entrée'
do jy=1,iny1
  do jx=1,inx1
    irvb2(1,jx,jy)=irvb1(2,jx,jy)
    irvb2(2,jx,jy)=irvb1(2,jx,jy)
    irvb2(3,jx,jy)=irvb1(2,jx,jy)
  enddo
enddo
climg2='V.ppm'
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
!
!-------------------------------------------------
! On ne garde que le B.
!-------------------------------------------------
!
print*,'		modification de l''image d''entrée'
do jy=1,iny1
  do jx=1,inx1
    irvb2(1,jx,jy)=irvb1(3,jx,jy)
    irvb2(2,jx,jy)=irvb1(3,jx,jy)
    irvb2(3,jx,jy)=irvb1(3,jx,jy)
  enddo
enddo
climg2='B.ppm'
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
