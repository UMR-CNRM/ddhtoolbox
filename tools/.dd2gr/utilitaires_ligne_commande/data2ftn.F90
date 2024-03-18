program ttt
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
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
if(iarg /= 1) then
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
call getargp(1,clarg)
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
iule=22 ; open(iule,file=clarg,form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
inomal=0
read(iule,fmt=*,iostat=ios) itx,ity
do
  read(iule,fmt=*,iostat=ios) ix,iy
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
    stop 'call abort'
  endif
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  zvx=(real(ix)+0.5)/real(itx)
  zvy=(real(iy)+0.5)/real(ity)
  write(*,fmt='(2(a,f4.2))') 'idata=idata+1 ; zxfrac(ichiffre,idata)=',zvx,' ; zyfrac(ichiffre,idata)=',zvy
enddo
!
!-------------------------------------------------
! Fermeture du fichier d'entrée.
!-------------------------------------------------
!
close(iule)
end

