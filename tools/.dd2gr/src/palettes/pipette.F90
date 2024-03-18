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
if(iarg /= 1) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Piratage de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpirate IMG '
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpirate:'
call getarg(1,climg1)
call getarg(2,climg2)
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
! Piratage ad-hoc.
!-------------------------------------------------
!
print*,'    modification de l''image d''entrée'
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
!
!-------------------------------------------------
! On copie ci-dessous les coordonnées-pixels des 2 rectangles du bas et du haut, et le nombre de rectangles de la palette à pipeter.
!-------------------------------------------------
!
ix1=1002  ; iy1=764 ; zval1=0.2
ix2=ix1 ; iy2= 125 ; zval2=500.
inb=12
!
!-------------------------------------------------
! Calcul.
!-------------------------------------------------
!
do jy=1,inb
  zdelta=(zval2-zval1)/real(inb-1)
  zval=zval1+(jy-1)*zdelta
  iy=nint(real(iy1)+real(jy-1)*real(iy2-iy1)/real(inb-1))
  write(*,fmt='(a,f5.1,3(i4))') '#PAL_SPECIF= <  ',zval,krvb1(1,ix1,iy),krvb1(2,ix1,iy),krvb1(3,ix1,iy)
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
