program imgmoy
! --------------------------------------------------------------
! Moyenne de plusieurs images.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------


!IMPLICIT NONE
INTEGER,ALLOCATABLE :: irvb_lit(:,:,:)
INTEGER,ALLOCATABLE :: irvb_ecr(:,:,:)
CHARACTER*200 clarg,clppm2,clopt
INTEGER :: IARG,igris,inx_ref,iny_ref
INTEGER :: IARGC
INTEGER :: INX,imin,imax
INTEGER :: INX1
INTEGER :: INY,jarg
INTEGER :: INY1
INTEGER :: IPOSX1
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: IPOSY2,iopt
INTEGER :: JCOUL,ix,iy
INTEGER :: JX,ix_lit,iy_lit,ix_ecr,iy_ecr,ioctet
REAL :: ZRAPA,znorm,zsom,zsom2,zect,zmoy,zfrac
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
clppm2='moy.ppm'
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Moyenne de plusieurs images.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgmoy IMG1 [IMG2 ... IMGn]'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Le fichier de sortie est ',clppm2(1:len_trim(clppm2))
  write(*,'(9a)') ' '
  stop
endif
print*,'imgmoy:'
do jarg=1,iarg
  call getargp(jarg,clarg)
  !
  !-------------------------------------------------
  ! Lecture de la taille du ppm d'entrée.
  !-------------------------------------------------
  !
  call img_taille(clarg,inx1,iny1)
  print*,'	image ',clarg(1:len_trim(clarg))
  print*,'		taille (',inx1,',',iny1,')'
  !
  !-------------------------------------------------
  ! Allocation de l'image moyenne.
  !-------------------------------------------------
  !
  if(jarg == 1) then
    allocate(irvb_ecr(3,inx1,iny1))
    irvb_ecr=0
    inx_ref=inx1
    iny_ref=iny1
  else
    if(inx1 /= inx_ref .or. iny1 /= iny_ref) then
      print*,'imgmoy/ERREUR: l''image ',clarg(1:len_trim(clarg)) &
      & ,' n''a pas la même taille que les autres!...'
      stop 'call abort'
    endif
  endif
  !
  !-------------------------------------------------
  ! Lecture de l'image 1.
  !-------------------------------------------------
  !
  print*,'		lecture de l''image d''entrée'
  if(jarg == 1) allocate(irvb_lit(3,inx1,iny1))
  call img_lec(clarg,inx1,iny1,irvb_lit)
  !
  !-------------------------------------------------
  ! Cumul.
  !-------------------------------------------------
  !
  irvb_ecr=irvb_ecr+irvb_lit
enddo
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
irvb_ecr=irvb_ecr/iarg
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(clppm2,inx1,iny1,irvb_ecr)
print*,'	fichier écrit: ',clppm2(1:len_trim(clppm2))
print*,' '
end
