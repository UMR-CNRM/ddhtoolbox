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
if(iarg /= 6) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Tracé d''une ligne sur une image.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgligne IMG X1 Y1 X2 Y2 IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpirate:'
call getargp(1,climg1)
call getargp(6,climg2)
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
call pirate(inx1,iny1,irvb1,irvb2)
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
integer :: irvb_trait(3)
real :: ztonm(3)
!
!-------------------------------------------------
! Ligne de commande.
!-------------------------------------------------
!
call getargp(2,clarg) ; read(clarg,fmt=*) ix1
call getargp(3,clarg) ; read(clarg,fmt=*) iy1
call getargp(4,clarg) ; read(clarg,fmt=*) ix2
call getargp(5,clarg) ; read(clarg,fmt=*) iy2
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
ilarg=2 ! largeur du trait en pixels.
irvb_trait(1)=255 ; irvb_trait(2)=000 ; irvb_trait(3)=000 ! trait rouge.
irvb_trait(1)=255 ; irvb_trait(2)=000 ; irvb_trait(3)=200 ! trait rose.
call img_trait(ix1,iy1,ix2,iy2,ilarg,irvb_trait,krvb2,knx1,kny1)
end
