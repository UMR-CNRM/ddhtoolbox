program imgblanc
! --------------------------------------------------------------
! Génération d'une image blanche.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
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
INTEGER,ALLOCATABLE :: irvb_ecr(:,:,:)
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
  write(*,'(a)') 'Génération d''une image blanche.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgblanc NX NY IMG'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getargp(1,clnx2)
call getargp(2,clny2)
call getargp(3,clppm2)
read(clnx2,fmt=*) inx2
read(clny2,fmt=*) iny2
print*,'imgblanc:'
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb_ecr(3,inx2,iny2))
!
!-------------------------------------------------
! Initialisation de l'image 2.
!-------------------------------------------------
!
irvb_ecr=255
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture de l''image (',inx2,',',iny2,') résultante...'
call img_ecr(clppm2,inx2,iny2,irvb_ecr)
print*,'	fichier écrit: ',trim(clppm2)
print*,' '
end
