program imgfondu
! --------------------------------------------------------------
! Fondu d'images ppm: transition continue de l'une à l'autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------


!IMPLICIT NONE
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
INTEGER,ALLOCATABLE :: irvb3(:,:,:)
INTEGER :: IARG,jarg,inb,jnb
INTEGER :: IARGC
CHARACTER*200 CLPPM1
CHARACTER*200 CLPPM2
CHARACTER*200 CLPPM3,cltype
INTEGER :: INX1
INTEGER :: INX2
INTEGER :: INY1
INTEGER :: INY2
REAL :: zfrac
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
inb=10
cltype='TRANSP'
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg < 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  print*, ' '
  print*, 'Fondu d''images PPM: transition continue de chacune à la suivante.'
  print*, ' '
  print*, 'Utilisation: imgfondu [-nNIMAGES -tTYPE] PPM1 PPM2 [PPM3 ... PPMn]'
  print*, ' '
  print*, 'avec'
  print*, '	- NIMAGES nombres d''images effectuant la transition de chacune à la suivante.'
  print*, '	  Défaut: ',inb
  print*, '	- TYPE: type de transition entre deux images:'
  print*, '	  - TRANSP si transition en transparence'
  print*, '	  - ALEA si transition aléatoire (non encore implémenté)'
  print*, '	  - TOURNE si transition par tourné de page (non encore implémenté)'
  print*, '	  Défaut: ',cltype
  print*, '	'
  print*, '	'
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
print*,'imgfondu:'
do jarg=1,iarg-1
  !
  !-------------------------------------------------
  ! Traitement de la ligne de commande.
  !-------------------------------------------------
  !
  call getarg(jarg,clppm1)
  if(clppm1(1:2) == '-n') then
    !
    !-------------------------------------------------
    ! L'utilisateur fournit le nb d'images.
    !-------------------------------------------------
    !
    read(clppm1(3:),fmt=*) inb
  elseif(clppm1(1:2) == '-t') then
    !
    !-------------------------------------------------
    ! L'utilisateur fournit le type de transition.
    !-------------------------------------------------
    !
    cltype=clppm1(3:)
  else
    !
    !-------------------------------------------------
    ! L'utilisateur fournit un nom de fichier.
    !-------------------------------------------------
    !
    call getarg(jarg+1,clppm2)
    call img_taille(clppm1,inx1,iny1)
    call img_taille(clppm2,inx2,iny2)
    print*,'	',clppm1(1:len_trim(clppm1)),' ==> ',clppm2(1:len_trim(clppm2)),'...'
    if(inx1 /= inx2 .or. iny1 /= iny2) then
      print*,'imgfondu/ERREUR: les deux images n''ont pas la m^eme taille!...'
      stop 'call abort'
    endif
    !
    !-------------------------------------------------
    ! Lecture de l'image 1.
    !-------------------------------------------------
    !
    print*,'		lecture de l''image 1...'
    allocate(irvb1(3,inx1,iny1))
    call img_lec(clppm1,inx1,iny1,irvb1)
    !
    !-------------------------------------------------
    ! Lecture de l'image 2.
    !-------------------------------------------------
    !
    print*,'		lecture de l''image 2...'
    allocate(irvb2(3,inx2,iny2))
    call img_lec(clppm2,inx2,iny2,irvb2)
    !
    !-------------------------------------------------
    ! Allocation de l'image de sortie.
    !-------------------------------------------------
    !
    allocate(irvb3(3,inx1,iny1))
    !
    !-------------------------------------------------
    ! Fondu.
    !-------------------------------------------------
    !
    do jnb=1,inb
      !
      !-------------------------------------------------
      ! Distance de l'image courante à la 1ère des 2
      ! (entre 0 et 1).
      !-------------------------------------------------
      !
      zfrac=real(jnb)/real(inb+1)
      !
      !-------------------------------------------------
      ! Ouverture du fichier de sortie.
      !-------------------------------------------------
      !
      write(clppm3,fmt='(2a,i3.3,a)') clppm1(1:len_trim(clppm1)),'.',jnb,'.fondu.ppm'
      print*,'		calcul / écriture de ',clppm3(1:len_trim(clppm3)),'...'
      if(cltype(1:len_trim(cltype)) == 'TRANSP') then
        irvb3=nint((1.-zfrac)*real(irvb1)+zfrac*real(irvb2))
      else
        print*,'imgfondu/ERREUR: type de transition inconnu!...'
        stop 'call abort'
      endif
      !
      !-------------------------------------------------
      ! Ecriture du fichier de sortie.
      !-------------------------------------------------
      !
      call img_ecr(clppm3,inx1,iny1,irvb3)
    enddo
    !
    !-------------------------------------------------
    ! Désallocation.
    !-------------------------------------------------
    !
    deallocate(irvb1)
    deallocate(irvb2)
    deallocate(irvb3)
  endif
enddo
end
