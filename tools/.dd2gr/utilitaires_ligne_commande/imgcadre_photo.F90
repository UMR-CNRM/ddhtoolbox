program imgcadre_photo
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
  write(*,'(9a)') 'Crée un effet à partir d''une photo donnée: effet de miroir dans le bas de la photo, pour la mettre en valeur.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgcadre_photo IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgcadre_photo:'
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
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
ibord_noir=2
idegrade=iny1/3
inx2=iny1+inx1+iny1
iny2=iny1+iny1+ibord_noir+idegrade+iny1
allocate(irvb2(3,inx2,iny2))
!
!-------------------------------------------------
! Calcul de l'image de sortie.
!-------------------------------------------------
!
irvb2=0
do jy=1,iny1
  do jx=1,inx1
    do jc=1,3
      irvb2(jc,jx+iny1,jy+iny1)=irvb1(jc,jx,jy)
    enddo
  enddo
enddo
do jy=1,idegrade
  do jx=1,inx1
    do jc=1,3
      irvb2(jc,jx+iny1,jy+2*iny1+ibord_noir)=max(1,min(255,nint(real(irvb1(jc,jx,iny1-jy+1))*0.6*real(idegrade-jy)/real(idegrade-1))))
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
print*,'		fichier écrit: ',trim(climg2)
print*,' '
end
