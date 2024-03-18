program imgdate_heure_5sat
! --------------------------------------------------------------
! ** Lecture dans une image JPG type pentasat Lannion, la date/heure de cette image.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2006-11, J.M. Piriou.
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
  write(*,'(9a)') 'Date/heure d''une image pentasat Lannion.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgdate_heure_5sat IMG'
  write(*,'(9a)') ' '
  stop
endif
call getargp(1,climg1)
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
!print*,'	image ',climg1(1:len_trim(climg1))
!print*,'		taille (',inx1,',',iny1,')'
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
!print*,'		lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Chiffres.
!-------------------------------------------------
!
ix1=96 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic1)
!
ix1=110 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic2)
!
ix1=139 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic3)
!
ix1=151 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic4)
!
ix1=181 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic5)
!
ix1=193 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic6)
!
ix1=208 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic7)
!
ix1=221 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic8)
!
ix1=278 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic9)
!
ix1=291 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic10)
!
ix1=319 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic11)
!
ix1=334 ; iy1=948 ; ix2=ix1+12 ; iy2=iy1+18
call img_chiffre(inx1,iny1y,irvb1,ix1,iy1,ix2,iy2,ic12)
!
!-------------------------------------------------
! Date.
!-------------------------------------------------
!
iaaaa=ic8+ic7*10+ic6*100+ic5*1000
imm=ic4+ic3*10
iqq=ic2+ic1*10
!
!-------------------------------------------------
! Heure.
!-------------------------------------------------
!
ihh=ic10+ic9*10
inn=ic12+ic11*10
write(*,fmt='(i4.4,4(i2.2))') iaaaa,imm,iqq,ihh,inn
end
