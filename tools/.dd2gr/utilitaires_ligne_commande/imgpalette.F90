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
if(iarg /= 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Piratage de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpirate IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpirate:'
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
real :: ztonm(3)
!
!-------------------------------------------------
! Calcul de la valeur du triplet RVB moyen.
!-------------------------------------------------
!
ztonm=0
do jy=1,kny1
  do jx=1,knx1
    do jc=1,3
      ztonm(jc)=ztonm(jc)+real(krvb1(jc,jx,jy))
    enddo
  enddo
enddo
ztonm=ztonm/(real(knx1)*real(kny1))
write(*,fmt=*) '		Couleur moyenne de l''image d''entrée: (R,V,B) = (',nint(ztonm(1)),',',nint(ztonm(2)),',',nint(ztonm(3)),')'
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
do jy=1,kny1
  do jx=1,knx1
    if(krvb1(1,jx,jy) == 255 .and. krvb1(2,jx,jy) == 000 .and. krvb1(3,jx,jy) == 200) then
      krvb2(1,jx,jy)=252
      krvb2(2,jx,jy)=006
      krvb2(3,jx,jy)=004
    elseif(krvb1(1,jx,jy) == 000 .and. krvb1(2,jx,jy) == 150 .and. krvb1(3,jx,jy) == 255) then
      krvb2(1,jx,jy)=4
      krvb2(2,jx,jy)=2
      krvb2(3,jx,jy)=252
    elseif(krvb1(1,jx,jy) == 000 .and. krvb1(2,jx,jy) == 255 .and. krvb1(3,jx,jy) == 254) then
      krvb2(1,jx,jy)=4
      krvb2(2,jx,jy)=50
      krvb2(3,jx,jy)=252
    elseif(krvb1(1,jx,jy) == 000 .and. krvb1(2,jx,jy) == 255 .and. krvb1(3,jx,jy) == 146) then
      krvb2(1,jx,jy)=4
      krvb2(2,jx,jy)=122
      krvb2(3,jx,jy)=252
    elseif(krvb1(1,jx,jy) == 000 .and. krvb1(2,jx,jy) == 255 .and. krvb1(3,jx,jy) == 039) then
      krvb2(1,jx,jy)=4
      krvb2(2,jx,jy)=250
      krvb2(3,jx,jy)=252
    elseif(krvb1(1,jx,jy) == 175 .and. krvb1(2,jx,jy) == 255 .and. krvb1(3,jx,jy) == 000) then
      krvb2(1,jx,jy)=115
      krvb2(2,jx,jy)=254
      krvb2(3,jx,jy)=135
    elseif(krvb1(1,jx,jy) == 255 .and. krvb1(2,jx,jy) == 228 .and. krvb1(3,jx,jy) == 000) then
      krvb2(1,jx,jy)=243
      krvb2(2,jx,jy)=254
      krvb2(3,jx,jy)=005
    elseif(krvb1(1,jx,jy) == 255 .and. krvb1(2,jx,jy) == 121 .and. krvb1(3,jx,jy) == 000) then
      krvb2(1,jx,jy)=252
      krvb2(2,jx,jy)=198
      krvb2(3,jx,jy)=004
    elseif(krvb1(1,jx,jy) == 255 .and. krvb1(2,jx,jy) == 014 .and. krvb1(3,jx,jy) == 000) then
      krvb2(1,jx,jy)=252
      krvb2(2,jx,jy)=071
      krvb2(3,jx,jy)=004
    elseif(krvb1(1,jx,jy) == 255 .and. krvb1(2,jx,jy) == 000 .and. krvb1(3,jx,jy) == 093) then
      krvb2(1,jx,jy)=252
      krvb2(2,jx,jy)=006
      krvb2(3,jx,jy)=004
    else
      krvb2(1,jx,jy)=krvb1(1,jx,jy)
      krvb2(2,jx,jy)=krvb1(2,jx,jy)
      krvb2(3,jx,jy)=krvb1(3,jx,jy)
    endif
  enddo
enddo
end
