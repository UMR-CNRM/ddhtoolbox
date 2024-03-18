program mdoc
! --------------------------------------------------------------
! 
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use const_ther
implicit character*200 (c)
implicit logical (l)
!
! -------------------------------------------------
! Saisie de la ligne de commande.
! -------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 2) then
  !
  ! -------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  ! -------------------------------------------------
  !
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Get autodocumentation of a SCM LFA file:'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Usage: mdoc -date PROFILE'
  write(*,'(9a)') '	to get information about base, valid. time, etc... of current profile.'
  write(*,'(9a)') 'or     mdoc -loc  PROFILE'
  write(*,'(9a)') '	to get information about location of current profile.'
  write(*,'(9a)') 'or     mdoc -hsl  PROFILE'
  write(*,'(9a)') '	to get the value of the local solar time of current profile (format hours and decimals).'
  write(*,'(9a)') 'or     mdoc -hsls  PROFILE'
  write(*,'(9a)') '	to get the value of the local solar time of current profile (format HH:MM LST).'
  write(*,'(9a)') ' '
  ! -------------------------------------------------
  stop
else
  !
  !-------------------------------------------------
  ! Nombre d'arguments OK.
  !-------------------------------------------------
  !
  call getargp(1,clopt)
  call getargp(2,clfic)
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  iul1=23
  call lfaouv(iul1,clfic,'R')
  !
  !-------------------------------------------------
  ! Lecture des autodocumentations.
  !-------------------------------------------------
  !
  if(clopt(1:len_trim(clopt)) == '-loc') then
    call localisation_en_clair(iul1,cldoc)
  elseif(clopt(1:len_trim(clopt)) == '-date') then
    call date_en_clair(iul1,cldoc)
  elseif(clopt(1:len_trim(clopt)) == '-hsl') then
    !
    !-------------------------------------------------
    ! Heure solaire locale.
    !-------------------------------------------------
    !
    call hsl(iul1,zhsl)
    write(cldoc,fmt='(f5.2)') zhsl
  elseif(clopt(1:len_trim(clopt)) == '-hsls') then
    !
    !-------------------------------------------------
    ! Heure solaire locale.
    !-------------------------------------------------
    !
    call hsl(iul1,zhsl)
    ih=int(zhsl)
    imi=nint((zhsl-real(ih))*60.)
    if(imi == 60) then
      imi=0
      ih=ih+1
    endif
    write(cldoc,fmt='(i2.2,a,i2.2,a)') ih,':',imi,' LST'
  elseif(clopt(1:len_trim(clopt)) == '-mu0') then
    !
    !-------------------------------------------------
    ! Sinus de la hauteur du Soleil.
    !-------------------------------------------------
    !
    call lfacas(iul1,'PMU0',cltype,ilong,ierr)
    if(ierr == 0) then
      !
      !-------------------------------------------------
      ! PMU0 a été calculé par la physique.
      !-------------------------------------------------
      !
      call lfalecr(iul1,'PMU0',1,zmu0,ilong,ierr)
    else
      !
      !-------------------------------------------------
      ! PMU0 n'a pas été calculé par la physique.
      ! On va le déduire de la date, la latitude, l'heure solaire locale.
      !-------------------------------------------------
      !
      !
      !-------------------------------------------------
      ! Date.
      !-------------------------------------------------
      !
      call lfacas(iul1,'KINDAT',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfaleci(iul1,'KINDAT',1,iindat,ilong,ierr)
      else
        call lfacas(iul1,'NINDAT',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfaleci(iul1,'NINDAT',1,iindat,ilong,ierr)
        else
          print*,'mdoc/ERROR: neither KINDAT nor NINDAT articles!...'
          call exit(1)
        endif
      endif
      lldebug=.false.
      iqq=modulo(iindat,100)
      iindat=iindat/100
      imm=modulo(iindat,100)
      if(lldebug) print*,'imm,iqq=',imm,iqq
      !
      !-------------------------------------------------
      ! Ecart au solstice d'été en jours.
      !-------------------------------------------------
      !
      zsolst=real(imm-6)*30.5+real(iqq-21)
      if(lldebug) print*,'zsolst=',zsolst
      !
      !-------------------------------------------------
      ! Déclinaison solaire.
      !-------------------------------------------------
      !
      zpi=4.*atan(1.)
      zdelta=23.45*zpi/180.*cos(2.*zpi*zsolst/365.25)
      if(lldebug) print*,'zdelta=',zdelta
      !
      !-------------------------------------------------
      ! Latitude.
      !-------------------------------------------------
      !
      call latitude(iul1,zlat)
      if(lldebug) print*,'zlat=',zlat
      !
      !-------------------------------------------------
      ! HSL.
      !-------------------------------------------------
      !
      call hsl(iul1,zhsl)
      zfrjour=zhsl/24.
      if(lldebug) print*,'zfrjour=',zfrjour
      !
      !-------------------------------------------------
      ! Calcul de hauteur solaire.
      !-------------------------------------------------
      !
      call azimh(zlat,zdelta,zfrjour,zazim,zhaut)
      zmu0=max(0.,sin(zhaut))
      if(lldebug) print*,'zhaut,zmu0=',zhaut,zmu0
    endif
    write(cldoc,fmt='(f5.2)') zmu0
  else
    write(*,fmt=*) 'mdoc/ERROR: first command line argument unrecognized!...'
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Fermeture du fichier LFA.
  !-------------------------------------------------
  !
  call lfafer(iul1)
  !
  !-------------------------------------------------
  ! Affichage.
  !-------------------------------------------------
  !
  write(*,fmt='(a)') cldoc(1:len_trim(cldoc))
endif
end
#include"dates.F90"
#include"mautodoc.F90"
#include"azimh.F90"
