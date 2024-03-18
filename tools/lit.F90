program lit
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"/home/piriou/ftn/implicit_r8i4.h"
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
iarg=iargc() ! nombre d'arguments.
if(iarg /= 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'OBJET: '
  write(*,'(9a)') ' '
  write(*,'(9a)') 'UTILISATION: '
  write(*,'(9a)') ' '
  write(*,'(9a)') 'METHODE: '
  write(*,'(9a)') ' '
  write(*,'(9a)') 'EXEMPLE: '
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getarg(1,clin)
call getarg(2,clout)
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iule=22 ; open(iule,file=clin,form='formatted')
iuls=23 ; open(iuls,file=clout,form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
inomal=0
do
  read(iule,fmt='(a)',iostat=ios) clc
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
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  clout=clc(32:)
  write(iuls,fmt='(a)') trim(clout)
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
end
