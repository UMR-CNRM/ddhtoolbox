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
iarg=iargc() ! nombre d'arguments.
if(iarg /= 3) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Conversion d''une image METEOSAT TIFF en un fichier ASCII XYV.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpirate IMG RAYON FXYV'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgsat2xyv:'
call getarg(1,climg1)
call getarg(2,clray)
call getarg(3,climg2)
read(clray,fmt=*) zr
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
allocate(irvb2(3,inx1,iny1))
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
! Calcul et écriture d'un fichier LLV.
!-------------------------------------------------
!
iul=40 ; open(iul,file=climg2,form='formatted')
zima=real(inx1)
do jx=1,inx1
  do jy=1,iny1
    zrpix=sqrt(max(0.,(real(jx)-zima/2.)**2+(real(jy)-zima/2.)**2))
    if(zrpix < zr) then
      !
      !-------------------------------------------------
      ! Le pixel est sur le disque.
      !-------------------------------------------------
      !
      zx=(real(jx)-zima/2.)/zr
      zy=-(real(jy)-zima/2.)/zr
      ztb=irvb1(1,jx,jy)+100.
      write(iul,fmt=*) zx,zy,ztb
    endif
  enddo
enddo
print*,'    fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
close(iul)
end
