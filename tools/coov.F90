subroutine coov(cdcooy,klong,kule,pcooy)
! --------------------------------------------------------------
! **** *cooy* Fourniture d'une coordonnée Y, lue sur fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! cdcooy,klong,kule
! En sortie:
! pcooy
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
character*(*) cdcooy
real(kind=8) :: pcooy(klong)
real(kind=8) :: ztmp(klong+1)
real(kind=8) :: zvah(klong+1)
real(kind=8) :: zvbh(klong+1)
real(kind=8) :: zphi(klong+1)
real(kind=8) :: zphif(klong)
real(kind=8) :: zt(klong)
real(kind=8) :: zorog(1), zpspt0(1)
if(cdcooy == 'LEV') then
  !
  !-------------------------------------------------
  ! Coordonnée niveaux.
  !-------------------------------------------------
  !
  do jlong=1,klong
    pcooy(jlong)=-jlong
  enddo
elseif(cdcooy == 'PRE') then
  !
  !-------------------------------------------------
  ! Coordonnée pression.
  ! On va regarder quelle est la coordoonée
  ! adaptée au champ à tracer disponible
  ! dans le fichier; on va choisir
  ! entre les articles PAPRS, PAPRSF, VAH
  ! et VBH.
  !-------------------------------------------------
  !
  call lfacas(kule,'PAPRSF',cltype,ilong,ierr)
  if(ierr == 0) then
    !
    !-------------------------------------------------
    ! L'article PAPRSF existe.
    !-------------------------------------------------
    !
    if(ilong == klong) then
      !
      ! Champ de type variable.
      !
      call lfalecr(kule,'PAPRSF',klong,ztmp,ilong,ierr)
      do jlong=1,klong
        pcooy(jlong)=-ztmp(jlong)/100.
      enddo
    elseif(ilong == klong-1) then
      !
      ! Champ de type flux.
      !
      call lfacas(kule,'PAPRS',cltype,ilong,ierr)
      if(ierr == 0) then
        !
        ! L'article PAPRS existe.
        !
        call lfalecr(kule,'PAPRS',klong,ztmp,ilong,ierr)
        do jlong=1,klong
          pcooy(jlong)=-ztmp(jlong)/100.
        enddo
      else
        print*,'coov/ERROR: PAPRSF exists in file'
        print*,'but PAPRS does not!...'
        call exit(1)
      endif
    else
      print*,'coov/ERROR: number of levels inconsistency'
      print*,'between user field and pressure field:'
      print*,klong,' levels in user field'
      print*,ilong,' levels in PAPRSF'
      call exit(1)
    endif
  else
    !
    ! L'article PAPRS n'existe pas.
    ! Il faut donc lire VAH et VBH.
    !
    idim=klong+1
    call lfalecr(kule,'PSPT0',1,zpspt0,ilong,ierr)
    call lfalecr(kule,'VAH',idim,zvah,ilong,ierr)
    call lfalecr(kule,'VBH',idim,zvbh,ilong,ierr)
    zpspt0(1)=exp(zpspt0(1))
    if(ilong == klong) then
      !
      ! Champ de type flux.
      !
      ! Calcul de la pression "half-levels"
      ! à partir de VAH et VBH.
      !
      zscal=zpspt0(1)
      call calpaprs(klong,zvah,zvbh,zscal,ztmp)
      do jlong=1,klong
        pcooy(jlong)=-ztmp(jlong)/100.
      enddo
    elseif(ilong == klong+1) then
      !
      ! Champ de type variable.
      !
      ! Calcul de la pression "full-levels"
      ! à partir de VAH et VBH.
      !
      zscal=zpspt0(1)
      call calpaprsf(idim,zvah,zvbh,zscal,ztmp)
      do jlong=1,klong
        pcooy(jlong)=-ztmp(jlong)/100.
      enddo
    else
      print*,'coov/ERROR: number of levels inconsistency'
      print*,'between user field and pressure field:'
      print*,klong,' levels in user field'
      print*,ilong,' levels in VAH'
      call exit(1)
    endif
  endif
elseif(cdcooy == 'HEI') then
  !
  !-------------------------------------------------
  ! Coordonnée hauteur.
  ! On va regarder quelle est la coordoonée
  ! adaptée au champ à tracer disponible
  ! dans le fichier; on va choisir
  ! entre les articles PAPHI, PAPHIF, VAH
  ! et VBH.
  !-------------------------------------------------
  !
  zg=9.80665
  zc=1./zg/1000. ! conversion J/kg > km.
  call lfacas(kule,'PAPHIF',cltype,ilong,ierr)
  if(ierr == 0) then
    !
    !-------------------------------------------------
    ! L'article PAPHIF existe.
    !-------------------------------------------------
    !
    if(ilong == klong) then
      !
      ! Champ de type variable.
      !
      call lfalecr(kule,'PAPHIF',klong,ztmp,ilong,ierr)
      do jlong=1,klong
        pcooy(jlong)=ztmp(jlong)*zc
      enddo
    elseif(ilong == klong-1) then
      !
      ! Champ de type flux.
      !
      call lfacas(kule,'PAPHI',cltype,ilong,ierr)
      if(ierr == 0) then
        !
        ! L'article PAPHI existe.
        !
        call lfalecr(kule,'PAPHI',klong,ztmp,ilong,ierr)
        do jlong=1,klong
          pcooy(jlong)=ztmp(jlong)*zc
        enddo
      else
        print*,'coov/ERROR: PAPHIF exists in file'
        print*,'but PAPHI does not!...'
        call exit(1)
      endif
    else
      print*,'coov/ERROR: number of levels inconsistency'
      print*,'between user field and pressure field:'
      print*,klong,' levels in user field'
      print*,ilong,' levels in PAPHIF'
      call exit(1)
    endif
  else
    !
    !-------------------------------------------------
    ! L'article PAPHI n'existe pas.
    ! Il faut donc le déduire de la pression et
    ! de la température.
    !-------------------------------------------------
    !
    !
    ! Calcul de p(a,b,vp00).
    !
    idim=klong+1
    call lfalecr(kule,'VAH',idim,zvah,ilong,ierr)
    call lfalecr(kule,'VBH',idim,zvbh,ilong,ierr)
    ilevflux=ilong
    ilevvar=ilevflux-1
    zpspt0(1)=101325.
    zscal=zpspt0(1)
    call calpaprs(ilong,zvah,zvbh,zscal,ztmp)
    !
    ! Lecture de la température.
    !
    call lfalecr(kule,'PTT0',klong,zt,ilt,ierr)
    !
    ! Calcul de phi(p,t).
    !
    call phidept(ilevvar,ztmp,zt,zphi,zphif)
    !
    !-------------------------------------------------
    ! Altitude: on ajoute l'altitude de la surface.
    !-------------------------------------------------
    !
    call lfacas(kule,'POROG',cltype,ilong_orog,ierr)
    if(ierr == 0) then
      call lfalecr(kule,'POROG',1,zorog,ilong_orog,ierr)
    else
      zorog(1)=0.
    endif
    call lfaleci(kule,'KLEV',1,ilev,ilong_ilev,ierr)
    do jlong=1,ilev
      zphif(jlong)=zphif(jlong)+zorog(1)
    enddo
    do jlong=1,ilev+1
      zphi(jlong)=zphi(jlong)+zorog(1)
    enddo
    if(ilong == klong) then
      !
      ! Champ de type flux.
      !
      do jlong=1,klong
        pcooy(jlong)=zphi(jlong)*zc
      enddo
    elseif(ilong == klong+1) then
      !
      ! Champ de type variable.
      !
      do jlong=1,klong
        pcooy(jlong)=zphif(jlong)*zc
      enddo
    else
      print*,'coov/ERROR: number of levels inconsistency'
      print*,'between user field and pressure field:'
      print*,klong,' levels in user field'
      print*,ilong,' levels in VAH'
      call exit(1)
    endif
  endif
else
  print*,'coov/ERROR: kind of Y coordinate!...'
  print*,cdcooy
  call exit(1)
endif
end
