program imgpirate
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
  write(*,'(9a)') 'Piratage de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpirate IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpirate:'
call getargp(1,climg1)
call getargp(2,climg2)
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
print*,'  image ',climg1(1:len_trim(climg1))
print*,'    taille (',inx1,',',iny1,')'
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
iplus=250
allocate(irvb2(3,inx1,iny1+iplus))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'    lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Piratage ad-hoc.
!-------------------------------------------------
!
print*,'    modification de l''image d''entrée'
do jx=1,inx1
  do jy=1,iny1
    do jc=1,3
      irvb2(jc,jx,jy)=irvb1(jc,jx,jy)
    enddo
  enddo
enddo
do jx=1,inx1
  do jy=iny1+1,iny1+iplus
    irvb2(1,jx,jy)=2
    irvb2(2,jx,jy)=34
    irvb2(3,jx,jy)=87
  enddo
enddo
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'    écriture de l''image (',inx1,',',iny1,') résultante'
itot=iny1+iplus
call img_ecr(climg2,inx1,itot,irvb2)
print*,'    fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
subroutine pirate(knx1,kny1,krvb1,krvb2)
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
!
!-------------------------------------------------
! Initialisation de l'image de sortie à celle d'entrée.
!-------------------------------------------------
!
krvb2=krvb1
!
!-------------------------------------------------
! Piratage.
!-------------------------------------------------
!
do jy=1,kny1
  do jx=1,knx1
    if(krvb1(1,jx,jy)+krvb1(2,jx,jy)+krvb1(3,jx,jy) < 80) then
      krvb2(1,jx,jy)=255
      krvb2(2,jx,jy)=255
      krvb2(3,jx,jy)=255
    endif
  enddo
enddo
!do jy=1,kny1
!  do jx=1,knx1
!    if(krvb1(1,jx,jy) < 100) then
!      krvb2(1,jx,jy)=255
!      krvb2(2,jx,jy)=0
!      krvb2(3,jx,jy)=0
!    endif
!  enddo
!enddo
!do jy=176,916
!  do jx=935,1710
!    krvb2(1,jx,jy)=255
!    krvb2(2,jx,jy)=255
!    krvb2(3,jx,jy)=255
!  enddo
!enddo
end
