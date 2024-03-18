program imgsymetrie
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
if(iarg /= 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Symétrie d''une image par rapport à un axe vertical.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgsymetrie IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgsymetrie:'
call getargp(1,climg1)
call getargp(2,climg2)
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
! Allocation de l'image d'entrée.
!-------------------------------------------------
!
allocate(irvb1(3,inx1,iny1))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
inx2=inx1
iny2=iny1
allocate(irvb2(3,inx2,iny2))
!
!-------------------------------------------------
! Symétrie.
!-------------------------------------------------
!
do jx2=1,inx2
  do jy2=1,iny2
    do jc=1,3
      irvb2(jc,jx2,jy2)=irvb1(jc,inx1-jx2+1,jy2)
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'		écriture de l''image (',inx2,',',iny2,') résultante'
call img_ecr(climg2,inx2,iny2,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
