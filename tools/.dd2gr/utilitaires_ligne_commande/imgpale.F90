program imgpale
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
if(iarg /= 3) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Rendre plus pâle et blanche l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpale TAUX IMG IMGSORTIE'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'avec TAUX réel entre 0 et 1: proportion de blanchissage de l''image de sortie.'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpale:'
call getargp(1,cltaux)
call getargp(2,climg1)
call getargp(3,climg2)
read(cltaux,fmt=*) ztaux
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
call pirate1(ztaux,inx1,iny1,irvb1,irvb2)
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
subroutine pirate1(ptaux,knx1,kny1,krvb1,krvb2)
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

!krvb2=krvb1
do jy=1,kny1
  do jx=1,knx1
    !
    !-------------------------------------------------
    ! Moyenne entre l'image courante et une couleur donnée.
    !-------------------------------------------------
    !
    ir=163 ; iv=211 ; ib=243 ! bleu clair.
    ir=255 ; iv=255 ; ib=255 ! blanc.
    krvb2(1,jx,jy)=max(0,min(255,nint(real(krvb1(1,jx,jy))*(1.-ptaux)+real(ir)*ptaux)))
    krvb2(2,jx,jy)=max(0,min(255,nint(real(krvb1(2,jx,jy))*(1.-ptaux)+real(iv)*ptaux)))
    krvb2(3,jx,jy)=max(0,min(255,nint(real(krvb1(3,jx,jy))*(1.-ptaux)+real(ib)*ptaux)))
  enddo
enddo
end
