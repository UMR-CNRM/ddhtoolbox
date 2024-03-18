program imgnuap
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
  write(*,'(9a)') 'NUAges Personnalisés: ridirige les niveaux de gris des pixels voisins du gris de l''image d''entrée vers une palette colorée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgnuap IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgnuap:'
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
call nuap(inx1,iny1,irvb1,irvb2)
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
subroutine nuap(knx1,kny1,krvb1,krvb2)
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
INTEGER :: krvb1(3,knx1,kny1),krvb2(3,knx1,kny1),irvb_loc(3)

call img_pal_init
krvb2=krvb1
do jy=1,kny1
  do jx=1,knx1
    !
    !-------------------------------------------------
    ! Critère de couleur.
    !-------------------------------------------------
    !
    zratio1=real(krvb1(2,jx,jy))/real(krvb1(1,jx,jy)) ! V / R.
    zratio2=real(krvb1(3,jx,jy))/real(krvb1(1,jx,jy)) ! B / R.
    if(abs(zratio1-1.)+abs(zratio2-1.) < 0.15) then
      zfrac=(real(krvb1(1,jx,jy))+real(krvb1(2,jx,jy))+real(krvb1(3,jx,jy)))/3./255.
      !call img_pal(clpal,'CONT','FRAC',zfrac,irvb_loc)
      irvb_loc=255
      krvb2(1,jx,jy)=irvb_loc(1)
      krvb2(2,jx,jy)=irvb_loc(2)
      krvb2(3,jx,jy)=irvb_loc(3)
    endif
  enddo
enddo
end
