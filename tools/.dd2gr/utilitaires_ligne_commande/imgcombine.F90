program jux
! --------------------------------------------------------------
! Combinaison de deux images ppm.
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


INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
INTEGER,ALLOCATABLE :: irvb3(:,:,:)
CHARACTER*200 CLPPM1
CHARACTER*200 CLPPM2
CHARACTER*200 CLPPM3
INTEGER :: IARG
INTEGER :: IARGC
INTEGER :: INX3,iny3
INTEGER :: INX1
INTEGER :: INX2
INTEGER :: INY1
INTEGER :: INY2
INTEGER :: IPOSX1
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: IPOSY2,iopt
INTEGER :: jcoul,ix,iy,inx2ana,iny2ana
INTEGER :: JX,ix1,iy1,ix3,iy3
INTEGER :: JY
REAL :: ZRAPAE
REAL :: ZRAPA_DROITE
REAL :: ZRAPA_SOUS,zrapa2
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
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
  write(*,'(a)') ' '
  write(*,'(a)') 'Combinaison de deux images PPM sur une seule.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgcombine PPM1 PPM2 PPM_SORTIE'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getargp(1,clppm1)
call getargp(2,clppm2)
call getargp(3,clppm3)
print*,'imgcombine:'
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'	lecture de l''image 1...'
call img_taille(clppm1,inx1,iny1)
print*,'	image 1 d''entrée: (',inx1,',',iny1,')'
allocate(irvb1(3,inx1,iny1))
call img_lec(clppm1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Lecture de l'image 2.
!-------------------------------------------------
!
print*,'	lecture de l''image 2...'
call img_taille(clppm2,inx2,iny2)
print*,'	image 2 d''entrée: (',inx2,',',iny2,')'
allocate(irvb2(3,inx2,iny2))
call img_lec(clppm2,inx2,iny2,irvb2)
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
inx3=inx1 ; iny3=iny1
allocate(irvb3(3,inx3,iny3))
!
!-------------------------------------------------
! Combinaison des deux images 1 et 2 en l'image de sortie 3.
!-------------------------------------------------
!
print*,'	combinaison des deux images...'
call combine_1(inx1,iny1,irvb1,inx2,iny2,irvb2,inx3,iny3,irvb3)
!
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture du fichier PPM résultant...'
print*,'	image 3 de sortie: (',inx3,',',iny3,')'
call img_ecr(clppm3,inx3,iny3,irvb3)
print*,'	fichier écrit: ',clppm3(1:len_trim(clppm3))
print*,' '
end
subroutine combine_1(knx1,kny1,krvb1,knx2,kny2,krvb2,knx3,kny3,krvb3)
! --------------------------------------------------------------
! Combinaison de deux images ppm.
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
! knx1,kny1,krvb1,knx2,kny2,krvb2,knx3,knx3
! En sortie:
! krvb3
! --------------------------------------------------------------

!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
INTEGER :: knx1,kny1
INTEGER :: knx2,kny2
INTEGER :: knx3,kny3
INTEGER :: krvb1(3,knx1,kny1)
INTEGER :: krvb2(3,knx2,kny2)
INTEGER :: krvb3(3,knx3,kny3)
INTEGER :: ix_coin_haut_gauche,iy_coin_haut_gauche
INTEGER :: jy,jx,jcoul,ix,iy,ipos1,ipos2
!
!-------------------------------------------------
! Ecriture de l'image de sortie.
!-------------------------------------------------
!
do jy=1,kny3
  do jx=1,knx3
    if(krvb1(1,jx,jy) == krvb1(2,jx,jy) .and. krvb1(2,jx,jy) == krvb1(3,jx,jy)) then
      if(krvb1(1,jx,jy)+krvb1(2,jx,jy)+krvb1(3,jx,jy) < 500) then
        !
        !-------------------------------------------------
        ! Blanc.
        !-------------------------------------------------
        !
        do jcoul=1,3
          krvb3(jcoul,jx,jy)=255
        enddo
      else
        !
        !-------------------------------------------------
        ! Image 2.
        !-------------------------------------------------
        !
        do jcoul=1,3
          krvb3(jcoul,jx,jy)=krvb2(jcoul,jx,jy)
        enddo
      endif
    else
      !
      !-------------------------------------------------
      ! Image 1.
      !-------------------------------------------------
      !
      do jcoul=1,3
        krvb3(jcoul,jx,jy)=krvb1(jcoul,jx,jy)
      enddo
    endif
  enddo
enddo
end
