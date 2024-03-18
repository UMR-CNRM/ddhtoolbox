program imgtonm
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
  write(*,'(9a)') 'Ton moyen d''une image, i.e. son triplet RVB moyen.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgtonm IMAGE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgtonm:'
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
print*,'		analyse de l''image d''entrée'
call pirate(inx1,iny1,irvb1,irvb2)
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
real :: ztonm(3)

ztonm=0

do jy=1,kny1
  do jx=1,knx1
    do jc=1,3
      ztonm(jc)=ztonm(jc)+real(krvb1(jc,jx,jy))
    enddo
  enddo
enddo
do jc=1,3
  ztonm(jc)=ztonm(jc)/real(knx1)/real(kny1)
enddo
write(*,fmt=*) '	Ton moyen : (R, V, B)=(',nint(ztonm(1)),', ', nint(ztonm(2)),', ', nint(ztonm(3)),')'
write(*,fmt=*) '	Moyenne de ces 3 composantes=',nint((ztonm(1)+ztonm(2)+ztonm(3))/3.)
end
