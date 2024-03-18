program imgpirate
! --------------------------------------------------------------
! 
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2022-04-28, J.M. Piriou.
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
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iule=22 ; open(iule,file='fort.78',form='formatted')
read(iule,fmt=*) iclas
ipix=20 ! nb de pixels par plage de couleur.
inx1=ipix*iclas
iny1=ipix
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb2(3,inx1,iny1))
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
inomal=0
do
  read(iule,fmt=*,iostat=ios) ir,iv,ib
  if(ios == -1) then
    !
    !-------------------------------------------------
    ! Fin de fichier.
    !-------------------------------------------------
    !
    exit
  elseif(ios == 0) then
    !
    !-------------------------------------------------
    ! Cas général.
    !-------------------------------------------------
    !
    inomal=inomal+1
  else
    !
    !-------------------------------------------------
    ! Cas non prévu.
    !-------------------------------------------------
    !
    write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
    stop 'call abort'
  endif
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  do jx=1,ipix
    do jy=1,ipix
      ix=(inomal-1)*ipix+jx
      irvb2(1,ix,jy)=ir
      irvb2(2,ix,jy)=iv
      irvb2(3,ix,jy)=ib
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'    écriture de l''image (',inx1,',',iny1,') résultante'
climg2='palette_classes.png'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'    fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
