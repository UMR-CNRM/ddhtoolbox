program imgpixxy
! --------------------------------------------------------------
! Info sur la taille d'une image.
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
CHARACTER*200 CLPPM1,clarg
INTEGER :: IARG,jarg,inx1,iny1,iargcp
INTEGER :: IARGC
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
llxseul=.false.
llyseul=.false.
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Information sur la taille d''une ou plusieurs images.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgpixxy [-x] [-y] IMG1 [IMG2 ... IMGn]'
  write(*,'(a)') ' '
  write(*,'(a)') 'avec'
  write(*,'(a)') '	-x si on veut la taille en X seulement.'
  write(*,'(a)') '	-y si on veut la taille en Y seulement.'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
do jarg=1,iarg
  call getarg(jarg,clarg)
  if(trim(clarg) == '-x') then
    llxseul=.true.
  elseif(trim(clarg) == '-y') then
    llyseul=.true.
  else
    !
    !-------------------------------------------------
    ! Lecture de la taille du ppm d'entrée.
    !-------------------------------------------------
    !
    call img_taille(clarg,inx1,iny1)
    if(llxseul) then
      write(clprint,fmt=*) inx1
      write(*,fmt='(a)') trim(adjustl(clprint))
    elseif(llyseul) then
      write(clprint,fmt=*) iny1
      write(*,fmt='(a)') trim(adjustl(clprint))
    else
      print*,clarg(1:len_trim(clarg)),'	:	(',inx1,',',iny1,')'
    endif
  endif
enddo
end
