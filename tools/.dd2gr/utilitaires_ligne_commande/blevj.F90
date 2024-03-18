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
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'    écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'    fichier écrit: ',climg2(1:len_trim(climg2))
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
    iseuil=100000
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-099)+abs(krvb1(2,jx,jy)-000)+abs(krvb1(3,jx,jy)-110)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=202
      krvb2(2,jx,jy)=151
      krvb2(3,jx,jy)=161
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-000)+abs(krvb1(2,jx,jy)-000)+abs(krvb1(3,jx,jy)-255)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=202
      krvb2(2,jx,jy)=151
      krvb2(3,jx,jy)=161
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-000)+abs(krvb1(2,jx,jy)-178)+abs(krvb1(3,jx,jy)-255)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=094
      krvb2(2,jx,jy)=011
      krvb2(3,jx,jy)=028
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-000)+abs(krvb1(2,jx,jy)-255)+abs(krvb1(3,jx,jy)-255)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=165
      krvb2(2,jx,jy)=006
      krvb2(3,jx,jy)=035
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-008)+abs(krvb1(2,jx,jy)-223)+abs(krvb1(3,jx,jy)-214)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=254
      krvb2(2,jx,jy)=006
      krvb2(3,jx,jy)=035
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-028)+abs(krvb1(2,jx,jy)-184)+abs(krvb1(3,jx,jy)-165)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=254
      krvb2(2,jx,jy)=112
      krvb2(3,jx,jy)=004
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-107)+abs(krvb1(2,jx,jy)-165)+abs(krvb1(3,jx,jy)-048)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=253
      krvb2(2,jx,jy)=161
      krvb2(3,jx,jy)=001
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-255)+abs(krvb1(2,jx,jy)-255)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=254
      krvb2(2,jx,jy)=217
      krvb2(3,jx,jy)=002
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-255)+abs(krvb1(2,jx,jy)-216)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=254
      krvb2(2,jx,jy)=217
      krvb2(3,jx,jy)=002
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-255)+abs(krvb1(2,jx,jy)-165)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=254
      krvb2(2,jx,jy)=250
      krvb2(3,jx,jy)=002
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-255)+abs(krvb1(2,jx,jy)-000)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-153)+abs(krvb1(2,jx,jy)-020)+abs(krvb1(3,jx,jy)-007)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-255)+abs(krvb1(2,jx,jy)-000)+abs(krvb1(3,jx,jy)-255)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-164)+abs(krvb1(2,jx,jy)-255)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-000)+abs(krvb1(2,jx,jy)-250)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-049)+abs(krvb1(2,jx,jy)-190)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-049)+abs(krvb1(2,jx,jy)-133)+abs(krvb1(3,jx,jy)-139)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=242 ; krvb2(2,jx,jy)=239 ; krvb2(3,jx,jy)=182
      krvb2(1,jx,jy)=100 ; krvb2(2,jx,jy)=100 ; krvb2(3,jx,jy)=100
    endif
    !
    ! Nouvelle couleur-test.
    !
    idelta=abs(krvb1(1,jx,jy)-000)+abs(krvb1(2,jx,jy)-000)+abs(krvb1(3,jx,jy)-000)
    if(idelta < iseuil) then
      iseuil=idelta
      krvb2(1,jx,jy)=000
      krvb2(2,jx,jy)=000
      krvb2(3,jx,jy)=000
    endif
    !
    !-------------------------------------------------
    ! Les gris sont laissés invariants.
    !-------------------------------------------------
    !
    if(abs(krvb1(1,jx,jy)-krvb1(2,jx,jy)) < 32 .and. abs(krvb1(2,jx,jy)-krvb1(3,jx,jy)) < 32) then
      krvb2(1,jx,jy)=krvb1(1,jx,jy)
      krvb2(2,jx,jy)=krvb1(2,jx,jy)
      krvb2(3,jx,jy)=krvb1(3,jx,jy)
    endif
  enddo
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
