program imgzoom
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
  write(*,'(9a)') 'Zoom avant sur l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgzoom IMG X1 Y1 X2 Y2 IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgzoom:'
call getargp(1,climg1)
call getargp(2,clx1)
call getargp(3,cly1)
call getargp(4,clx2)
call getargp(5,cly2)
call getargp(6,climg2)
read(clx1,fmt=*) ix1 ; read(cly1,fmt=*) iy1
read(clx2,fmt=*) ix2 ; read(cly2,fmt=*) iy2
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
! Test de cohérence.
!-------------------------------------------------
!
if(ix1 < 1) then
  write(*,fmt=*) 'imgzoom/ERREUR: X1 doit être >= 1!...'
  stop 'call abort'
endif
if(iy1 < 1) then
  write(*,fmt=*) 'imgzoom/ERREUR: Y1 doit être >= 1!...'
  stop 'call abort'
endif
if(ix2 > inx1) then
  write(*,fmt=*) 'imgzoom/ERREUR: X2 doit être <= au nombre de pixels en X!...'
  stop 'call abort'
endif
if(iy2 > iny1) then
  write(*,fmt=*) 'imgzoom/ERREUR: Y2 doit être <= au nombre de pixels en Y!...'
  stop 'call abort'
endif
!
!-------------------------------------------------
! Dimension de l'image de sortie.
!-------------------------------------------------
!
inx2=ix2-ix1+1
iny2=iy2-iy1+1
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb2(3,inx2,iny2))
!
!-------------------------------------------------
! Zoom proprement dit.
!-------------------------------------------------
!
print*,'		zoom de l''image d''entrée'
do jy=1,iny2
  do jx=1,inx2
    do jcoul=1,3
      irvb2(jcoul,jx,jy)=irvb1(jcoul,ix1+jx-1,iy1+jy-1)
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
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
