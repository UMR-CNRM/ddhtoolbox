program imgmedaillon
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
if(iarg /= 5) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Création d''un médaillon circulaire à partir d''une image donnée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgmedaillon IMG X Y RAYON IMGSORTIE'
  write(*,'(9a)') 'avec'
  write(*,'(9a)') '	X et Y les cordonnées du centre du médaillon sur l''image d''entrée.'
  write(*,'(9a)') '	RAYON son rayon.'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgmedaillon:'
call getargp(1,climg1)
call getargp(2,clx)
read(clx,fmt=*) ix
call getargp(3,cly)
read(cly,fmt=*) iy
call getargp(4,clrayon)
read(clrayon,fmt=*) irayon
call getargp(5,climg2)
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
! Piratage ad-hoc.
!-------------------------------------------------
!
print*,'		modification de l''image d''entrée'
call medaillon(ix,iy,irayon,inx1,iny1,irvb1,irvb2)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
subroutine medaillon(kx,ky,krayon,knx1,kny1,krvb1,krvb2)
! --------------------------------------------------------------
! Modification d'une image rectangulaire.
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
INTEGER :: krvb1(3,knx1,kny1),krvb2(3,knx1,kny1)
real :: ztonm(3)
!
!-------------------------------------------------
! Initialisation de l'image de sortie.
!-------------------------------------------------
!
krvb2=255
!
!-------------------------------------------------
! Piratage.
!-------------------------------------------------
!
do jy=1,kny1
  do jx=1,knx1
    iray=nint(sqrt(real((jx-kx)**2+(jy-ky)**2)))
    if(iray < krayon) then
      do jc=1,3
        krvb2(jc,jx,jy)=krvb1(jc,jx,jy)
      enddo
    endif
  enddo
enddo
end
