subroutine cooz(kule,klev,paphi,paphif)
! --------------------------------------------------------------
! **** ** Fourniture d'une coordonnée Y géopotentiel, lue sur fichier M1D LFA.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kule: unité logique du fichier LFA. Ce fichier doit déjà être ouvert.
!	klev: nombre de niveaux de variables sur la verticale.
! En sortie:
!	paphi: géopotentiel niveaux flux.
!	paphif: géopotentiel niveau variables.
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
real ztmp(klev+1)
real zvah(klev+1)
real zvbh(klev+1)
real paphi(klev+1)
real paphif(klev)
real zt(klev)
real ztab(1)
ilev1=klev+1
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
call lfacas(kule,'PAPHIF',cltype,ilong,ierr)
if(ierr == 0) then
  !
  !-------------------------------------------------
  ! L'article PAPHIF existe.
  !-------------------------------------------------
  !
  call lfalecr(kule,'PAPHIF',klev,paphif,ilong,ierr)
  call lfalecr(kule,'PAPHI',ilev1,paphi,ilong,ierr)
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
  idim=klev+1
  call lfalecr(kule,'VAH',idim,zvah,ilong,ierr)
  call lfalecr(kule,'VBH',idim,zvbh,ilong,ierr)
  ilevflux=ilong
  ilevvar=ilevflux-1
  zpspt0=101325.
  call calpaprs(ilong,zvah,zvbh,zpspt0,ztmp)
  !
  ! Lecture de la température.
  !
  call lfalecr(kule,'PTT0',klev,zt,ilt,ierr)
  !
  ! Calcul de phi(p,t).
  !
  call phidept(ilevvar,ztmp,zt,paphi,paphif)
  !
  !-------------------------------------------------
  ! Altitude: on ajoute l'altitude de la surface.
  !-------------------------------------------------
  !
  call lfacas(kule,'POROG',cltype,ilong_orog,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'POROG',1,ztab,ilong_orog,ierr)
    zorog=ztab(1)
  else
    zorog=0.
  endif
  paphi=paphi+zorog
  paphif=paphif+zorog
endif
end
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
!     2019-10-15: un bug corrigé: klev était utilisé dans cette routine, non initialisé. klev a été renommé en klong, ce qui corrige ce bug. Bug soulevé par Salomé Antoine, étudiante en thèse.
! --------------------------------------------------------------
! En entree:
! cdcooy,klong,kule
! En sortie:
! pcooy
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
character*(*) cdcooy
real pcooy(klong)
real ztmp(klong+1)
real zvah(klong+1)
real zvbh(klong+1)
real zphi(klong+1)
real zphif(klong)
real ztab(1)
real zt(klong)
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
    call lfalecr(kule,'PSPT0',1,ztab,ilong,ierr)
    zpspt0=ztab(1)
    call lfalecr(kule,'VAH',idim,zvah,ilong,ierr)
    call lfalecr(kule,'VBH',idim,zvbh,ilong,ierr)
    zpspt0=exp(zpspt0)
    if(ilong == klong) then
      !
      ! Champ de type flux.
      !
      ! Calcul de la pression "half-levels"
      ! à partir de VAH et VBH.
      !
      call calpaprs(klong,zvah,zvbh,zpspt0,ztmp)
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
      call calpaprsf(idim,zvah,zvbh,zpspt0,ztmp)
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
      ilongflux=klong+1
      call lfalecr(kule,'PAPHI',ilongflux,ztmp,ilongtmp,ierr)
      zphis=ztmp(ilongtmp) ! géopotentiel de surface.
      call lfalecr(kule,'PAPHIF',klong,ztmp,ilong,ierr)
      do jlong=1,klong
        !pcooy(jlong)=(ztmp(jlong)-zphis)*zc ! hauteur (au-dessus de la surface).
        pcooy(jlong)=ztmp(jlong)*zc ! altitude (au-dessus du niveau de la mer).
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
        zphis=ztmp(ilong) ! géopotentiel de surface.
        do jlong=1,klong
          !pcooy(jlong)=(ztmp(jlong)-zphis)*zc ! hauteur (au-dessus de la surface).
          pcooy(jlong)=ztmp(jlong)*zc ! altitude (au-dessus du niveau de la mer).
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
    zpspt0=101325.
    call calpaprs(ilong,zvah,zvbh,zpspt0,ztmp)
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
      call lfalecr(kule,'POROG',1,ztab,ilong_orog,ierr)
      zorog=ztab(1)
    else
      zorog=0.
    endif
    call lfaleci(kule,'KLEV',1,ilev,ilong_ilev,ierr)
    do jlong=1,ilev
      zphif(jlong)=zphif(jlong)+zorog
    enddo
    do jlong=1,ilev+1
      zphi(jlong)=zphi(jlong)+zorog
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
subroutine calpaprs(kdim,pvah,pvbh,pspt0,ptmp)
! --------------------------------------------------------------
! **** *calpaprs* Calcul de la pression "half-levels" à partir des coordonnées A et B.
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
! kdim,pvah,pvbh,pspt0
! En sortie:
! ptmp
! --------------------------------------------------------------
real pvah(kdim)
real pvbh(kdim)
real ptmp(kdim)
do jdim=1,kdim
  ptmp(jdim)=pvah(jdim)+pvbh(jdim)*pspt0
enddo
end
subroutine calpaprsf(kdim,pvah,pvbh,pspt0,ptmp)
! --------------------------------------------------------------
! **** *calpaprs* Calcul de la pression "full-levels" à partir des coordonnées A et B.
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
! kdim,pvah,pvbh,pspt0
! En sortie:
! ptmp
! --------------------------------------------------------------
real pvah(kdim)
real pvbh(kdim)
real ptmp(kdim)
!
!-------------------------------------------------
! Attention: ptmp est surdimensionné dans cette routine:
! seules les valeurs de 1 à (kdim-1) vont être initialisées.
!-------------------------------------------------
!
do jdim=1,kdim-1
  ptmp(jdim)=0.5*((pvah(jdim)+pvbh(jdim)*pspt0) &
& 	+(pvah(jdim+1)+pvbh(jdim+1)*pspt0))
enddo
end
#include"hydrostatique.F90"
