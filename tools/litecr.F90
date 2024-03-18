subroutine litecr(kranf,cdcoox,cdcooy,cduni,cduf,cddateref &
& ,ldkindat,kule,kuldoc,cdfics,cdna,cdtype,klong,kniv &
& ,kuls,pcoef,padd,ldxcoo,ldft,plon,ldfirst,pref_time,pindef,ptemps,cdlegx)
! --------------------------------------------------------------
! **** *litecr* Lecture d'un article LFA et écriture sur fichier formatté.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! kule: unité logique d'entrée.
! cdna: nom de l'article LFA.
! klong: longueur physique de cet article.
! kniv: niveau à recopier.
! kuls: unité logique de sortie.
! En sortie:
! --------------------------------------------------------------
use evoldata, only : gprof, lgdiff
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,j,k)

real(kind=8) :: zloc(klong)
real(kind=8) :: zdelp(klong)
real(kind=8) :: zcoo(2)
real(kind=8) :: zarr(1)
real(kind=8) :: zcooy(klong)
integer(kind=4) :: iloc(klong)
integer(kind=4) :: istep(1)
integer(kind=4) :: indat(1)
integer(kind=4) :: isssss(1)
character*(*) cdcoox,cdcooy,cddateref,cduni,cduf,cdtype,cdfics,cdlegx
character*80 cldate
if(ldxcoo) then
  !
  ! -------------------------------------------------
  ! Lecture de la coordonnée X de sortie.
  ! -------------------------------------------------
  !
  if(cdcoox == 'RSTATI') then
    !
    !-------------------------------------------------
    ! La coordonnée X est le temps écoulé depuis
    ! le début de la prévision, estimé par RSTATI.
    !-------------------------------------------------
    !
    call lfalecr(kule,'RSTATI',1,zarr,ilong,ierr)
    zt=zarr(1)
  elseif(cdcoox == 'TSTEP') then
    !
    !-------------------------------------------------
    ! La coordonnée X est le numéro de pas de temps.
    !-------------------------------------------------
    !
    call lfacas(kule,'KSTEP',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfaleci(kule,'KSTEP',1,istep,ilong,ierr)
    else
      call lfaleci(kule,'NSTEP',1,istep,ilong,ierr)
    endif
    zt=real(istep(1))
  elseif(cdcoox == 'NINDAT') then
    !
    !-------------------------------------------------
    ! La coordonnée X est le temps écoulé depuis
    ! le début de la prévision, estimé par
    ! NINDAT et NSSSSS.
    !-------------------------------------------------
    !
    if(ldkindat) then
      call lfaleci(kule,'KINDAT',1,indat,ilong,ierr)
      call lfaleci(kule,'KSSSSS',1,isssss,ilong,ierr)
    else
      call lfaleci(kule,'NINDAT',1,indat,ilong,ierr)
      call lfaleci(kule,'NSSSSS',1,isssss,ilong,ierr)
    endif
    call lfalecr(kule,'RSTATI',1,zarr,ilong,ierr)
    zt=zarr(1)
    !
    !-------------------------------------------------
    ! On va écrire la date sur une chaîne de caractères.
    !-------------------------------------------------
    !
    zecoul=(real(isssss(1))+zt)/86400.
    ijours=int(zecoul)
    !
    zecoul=zecoul-int(zecoul)
    zecoul=24.*zecoul
    ihh=int(zecoul)
    !
    zecoul=zecoul-int(zecoul)
    zecoul=60.*zecoul
    imm=int(zecoul)
    !
    zecoul=zecoul-int(zecoul)
    zecoul=60.*zecoul
    iss=int(zecoul)
    !
    write(cldate,fmt='(i8.8,3i2.2)') indat(1),ihh,imm,iss
    if(kranf == 1) cddateref=cldate
    !
    !-------------------------------------------------
    ! Ecart entre les deux dates, en secondes.
    !-------------------------------------------------
    !
    call ecartds(cddateref,cldate,iecart)
    zt=real(iecart+ijours*86400)
  elseif(cdcoox == 'LST' .or. cdcoox == 'SYB' .or. trim(cdcoox) == 'JD' ) then
    !
    !-------------------------------------------------
    ! La coordonnée X est le temps écoulé depuis
    ! le 1er janvier à 0h heure solaire moyenne.
    !-------------------------------------------------
    !
    if(ldkindat) then
      call lfaleci(kule,'KINDAT',1,indat,ilong,ierr)
      call lfaleci(kule,'KSSSSS',1,isssss,ilong,ierr)
    else
      call lfaleci(kule,'NINDAT',1,indat,ilong,ierr)
      call lfaleci(kule,'NSSSSS',1,isssss,ilong,ierr)
    endif
    call lfalecr(kule,'RSTATI',1,zarr,ilong,ierr)
    zstati=zarr(1)
    !
    !-------------------------------------------------
    ! Nombre de jours écoulés entre le 1er janvier et la date courante.
    !-------------------------------------------------
    !
    indatref=10000*(indat(1)/10000)+0101 ! 1er janvier au format AAAAMMQQ.
    indat_scal=indat(1)
    call ecartdj(indatref,indat_scal,iecart)
    !
    !-------------------------------------------------
    ! Temps en secondes entre l'instant courant
    ! et le 1er janvier à 0 UTC.
    !-------------------------------------------------
    !
    zt=real(iecart)*86400.+zstati+real(isssss(1))
    if(trim(cdcoox) == 'JD') then
      !
      !-------------------------------------------------
      ! Date julienne du 1er janvier de l'année courante.
      !-------------------------------------------------
      !
      iqq=modulo(indat(1),100)
      idatl=indat(1)/100
      imm=modulo(idatl,100)
      iaaaa=idatl/100
      ihh=0
      imn=0
      zss=0.
      call amqhms_vers_dj(iaaaa,imm,iqq,ihh,imn,zss,zdj)
      zt=zdj+real(isssss(1))/86400.+zstati/86400.
    endif
    if(cdcoox == 'LST') then
      !
      !-------------------------------------------------
      ! Correction de longitude.
      !-------------------------------------------------
      !
      zpi=4.*atan(1.)
      z1=-zpi
      z2=zpi
      zlon=zmodyx(plon,z1,z2) ! on force la longitude entre -pi et pi.
      zcorr=zlon/(2.*zpi)*86400. ! correction de longitude.
      zt=zt+zcorr
      if(ldfirst) then
        !
        !-------------------------------------------------
        ! Le présent fichier est le premier ouvert.
        ! On va initialiser la référence de temps
        ! au dernier instant UTC où il était 00 heure solaire moyenne du lieu courant.
        !-------------------------------------------------
        !
        pref_time=86400.*nint(zt/86400.)
        if(pref_time > zt) pref_time=pref_time-86400.
      else
        !
        !-------------------------------------------------
        ! Le présent fichier est le premier ouvert.
        ! On va soustraire la référence de temps.
        !-------------------------------------------------
        !
        if(pref_time == pindef) then
          write(*,fmt=*) 'mevol/ERREUR: pref_time should be initialized!...'
          call exit(1)
        endif
      endif
      zt=zt-pref_time
    endif
  else
    print*,'mevol/ERROR: kind of X coordinate!...'
    print*,cdcoox
    call exit(1)
  endif
  if(trim(cdcoox) == 'JD') then
    !
    !-------------------------------------------------
    ! On ne modifie pas zt.
    !-------------------------------------------------
    !
  elseif(cdcoox == 'TSTEP') then
    !
    !-------------------------------------------------
    ! Le temps est souhaité en nombre de pas de temps.
    !-------------------------------------------------
    !
    cdlegx='time step number'
  elseif(cduni == 's') then
    !
    !-------------------------------------------------
    ! Le temps est souhaité en secondes.
    !-------------------------------------------------
    !
    cdlegx='t (s)'
  elseif(cduni == 'h') then
    !
    !-------------------------------------------------
    ! Le temps est souhaité en heures.
    !-------------------------------------------------
    !
    zt=zt/3600.
    cdlegx='t (h)'
  elseif(cduni == 'd') then
    !
    !-------------------------------------------------
    ! Le temps est souhaité en jours.
    !-------------------------------------------------
    !
    zt=zt/86400.
    cdlegx='t (days)'
  else
    print*,'mevol/ERROR: time dimension!...'
    print*,cduni
    call exit(1)
  endif
else
  if(cduf == ' ') then
    cdlegx=trim(cdna)
  else
    cdlegx=trim(cdna)//' ('//trim(cduf)//')'
  endif
  zt=0.
endif
ptemps=zt
!
! -------------------------------------------------
! Lecture de l'article dans le LFA.
! -------------------------------------------------
!
if(cdtype(1:1) == 'I') then
  call lfaleci(kule,cdna,klong,iloc,ilong,ierr)
  llent=.true.
elseif(cdtype(1:1) == 'R') then
  call lfalecr(kule,cdna,klong,zloc,ilong,ierr)
  llent=.false.
else
  print*,'mevol/ERROR: unexpected case: ',cdtype(1:1)
  call exit(1)
endif
if(ilong == 1.and.kniv == -1) then
  !
  !-------------------------------------------------
  ! On demande tous les niveaux
  ! d'un champ qui n'en comporte qu'un seul.
  !-------------------------------------------------
  !
  kniv=0
endif
if(ldft) then
  !
  !-------------------------------------------------
  ! L'utilisateur a demandé de convertir
  ! les flux en tendences, la longueur
  ! à utiliser dans la suite est donc (klong-1).
  !-------------------------------------------------
  !
  ilongloc=klong-1
  !
  !-------------------------------------------------
  ! On effectue la divergence sur la verticale.
  !-------------------------------------------------
  !
  if(cdtype(1:1) == 'I') then
    print*,'mevol/ERROR: you asked for a tendency computation, starting from an INTEGER(KIND=4) field!...'
    call exit(1)
  elseif(cdtype(1:1) == 'R') then
    !
    !-------------------------------------------------
    ! Lecture de l'épaisseur-pression de la couche.
    !-------------------------------------------------
    !
    call lfalecr(kule,'PDELP',klong,zdelp,ilong,ierr)
    !
    !-------------------------------------------------
    ! Divergence sur la verticale.
    !-------------------------------------------------
    !
    do jlong=1,ilongloc
      zloc(jlong)=-9.80665*(zloc(jlong+1)-zloc(jlong))/zdelp(jlong)
    enddo
  else
    print*,'mevol/ERROR: unexpected data type: ',cdtype(1:1)
    call exit(1)
  endif
else
  !
  !-------------------------------------------------
  ! L'utilisateur n'a pas demandé de convertir
  ! les flux en tendences, la longueur
  ! à utiliser dans la suite est donc la même que celle
  ! en entrée de la routine, soit klong.
  !-------------------------------------------------
  !
  ilongloc=klong
endif
if(kniv == -1) then
  !
  !-------------------------------------------------
  ! On veut tous les niveaux.
  ! Il faut donc fournir une coordonnée verticale.
  !-------------------------------------------------
  !
  call coov(cdcooy,ilongloc,kule,zcooy)
elseif(kniv == -2) then
  !
  !-------------------------------------------------
  ! On veut une intégrale verticale.
  ! Il faut donc lire PDELP.
  !-------------------------------------------------
  !
  call lfalecr(kule,'PDELP',klong,zdelp,ilong,ierr)
  !
  !-------------------------------------------------
  ! Calcul de l'intégrale verticale.
  !-------------------------------------------------
  !
  zint=0.
  do jlev=1,ilong
    zint=zint+zloc(jlev)*zdelp(jlev)/9.80665
  enddo
endif
!
! -------------------------------------------------
! Ecriture sur fichier formatté.
! -------------------------------------------------
!
if(kniv == -1) then
  !
  ! -------------------------------------------------
  ! On veut tous les niveaux.
  ! -------------------------------------------------
  !
  ilong1=1
  ilong2=ilongloc
elseif(kniv == -2) then
  !
  !-------------------------------------------------
  ! On veut l'intégrale verticale.
  !-------------------------------------------------
  !
  ilong1=ilongloc
  ilong2=ilong1
  zloc(ilong1)=zint
else
  !
  ! -------------------------------------------------
  ! On veut un niveau précis:
  ! pas de coordonnée verticale souhaitée.
  ! -------------------------------------------------
  !
  if(kniv == 0) then
    !
    !-------------------------------------------------
    ! On veut le niveau le plus bas.
    !-------------------------------------------------
    !
    ilong1=ilongloc
    ilong2=ilong1
  elseif(kniv >= 1.and.kniv <= ilongloc) then
    !
    !-------------------------------------------------
    ! On veut un niveau intermédiaire.
    !-------------------------------------------------
    !
    ilong1=kniv
    ilong2=ilong1
  else
    print*,'mevol/ERROR: you asked for level ',kniv,'!...'
    call exit(1)
  endif
endif
do jlong=ilong1,ilong2
  !
  !-------------------------------------------------
  ! Coordonnées.
  !-------------------------------------------------
  !
  icoo=0
  if(ldxcoo) then
    !
    !-------------------------------------------------
    ! Coordonée temps souhaitée.
    !-------------------------------------------------
    !
    icoo=icoo+1
    zcoo(icoo)=zt
  endif
  if(kniv == -1) then
    !
    ! -------------------------------------------------
    ! On veut tous les niveaux:
    ! coordonnée verticale souhaitée.
    ! -------------------------------------------------
    !
    icoo=icoo+1
    zcoo(icoo)=zcooy(jlong)
  endif
  if(llent) then
    !
    !-------------------------------------------------
    ! Champ entier.
    !-------------------------------------------------
    !
    if(kranf == 1 .and. lgdiff) then
      gprof(jlong)=real(iloc(jlong))
    endif
    write(kuls,fmt=*) (zcoo(jcoo),jcoo=1,icoo) &
&     ,nint((real(iloc(jlong))-gprof(jlong)+padd)*pcoef)
  else
    !
    !-------------------------------------------------
    ! Champ réel.
    !-------------------------------------------------
    !
    if(kranf == 1 .and. lgdiff) then
      gprof(jlong)=zloc(jlong)
    endif
    !
    ! La première coordonnée est mise sur 12 chiffres significatifs
    ! pour encaisser les jours juliens du type "2442289.50000".
    !
    write(kuls,fmt='(g21.12,9g16.7)') (zcoo(jcoo),jcoo=1,icoo) &
&     ,(zloc(jlong)-gprof(jlong)+padd)*pcoef
  endif
enddo
if(kranf == 1) then
  !
  !-------------------------------------------------
  ! Format de tracé.
  !-------------------------------------------------
  !
  if(icoo == 2) then
    write(kuldoc,fmt='(9a)') '#FORMAT=XYV'
    write(kuldoc,fmt='(9a)') '#0PF'
    write(kuldoc,fmt='(9a)') '#INTERPOLE=200 150'
    write(kuldoc,fmt='(9a)') '#METHODE_EXTRAPOLATION=Y_PUIS_X'
  elseif(icoo == 1) then
    if(cdlegx(1:1) == 't') then
      write(kuldoc,fmt='(9a)') '#FORMAT=XV'
    else
      write(kuldoc,fmt='(9a)') '#FORMAT=YV'
    endif
  else
    write(*,fmt=*) ' '
    write(*,fmt=*) 'mevol/WARNING: icoo should be 1 or 2!...'
    write(*,fmt=*) icoo
    write(*,fmt=*) 'Its looks like as if you have asked neither for an evolution nor a vertical profile,'
    write(*,fmt=*) 'so that the resulting data is a single number.'
    write(*,fmt=*) ' '
  endif
  !
  !-------------------------------------------------
  ! Légende Y.
  !-------------------------------------------------
  !
  if(icoo == 2 .or. (icoo == 1 .and. cdlegx(1:1) /= 't')) then
    if(cdcooy(1:len_trim(cdcooy)) == 'HEI') then
      write(kuldoc,fmt='(9a)') '#LEGENDE_Y=z (km)'
      write(kuldoc,fmt='(9a)') '#Y_MAX_LIMIT=15.'
    elseif(cdcooy(1:len_trim(cdcooy)) == 'PRE') then
      write(kuldoc,fmt='(9a)') '#LEGENDE_Y=p (hPa)'
    elseif(cdcooy(1:len_trim(cdcooy)) == 'LEV') then
      write(kuldoc,fmt='(9a)') '#LEGENDE_Y=levels'
    else
      write(*,fmt=*) 'mevol/WARNING: Y coordinate should be accessible!...'
    endif
  elseif(icoo == 1 .and. cdlegx(1:1) == 't') then
      write(kuldoc,fmt='(9a)') '#LEGENDE_Y=',cdna(1:len_trim(cdna))
  endif
  !
  !-------------------------------------------------
  ! Légende X.
  !-------------------------------------------------
  !
  write(kuldoc,fmt='(9a)') '#LEGENDE_X=',trim(cdlegx)
  !
  !-------------------------------------------------
  ! Titre.
  !-------------------------------------------------
  !
  write(kuldoc,fmt='(9a)') '#TITRE=',cdna(1:len_trim(cdna))
  write(kuldoc,fmt='(9a)') '#FICHIER=',cdfics(1:len_trim(cdfics))
  write(kuldoc,fmt='(9a)') '#UNITE=',trim(cduf)
  !
  !-------------------------------------------------
  ! Date des données.
  !-------------------------------------------------
  !
  call date_en_clair(kule,cldatc)
  write(kuldoc,fmt='(9a)') '#DATE=',cldatc(1:len_trim(cldatc))
  !
  !-------------------------------------------------
  ! Origine des données.
  !-------------------------------------------------
  !
  call lfacas(kule,'INDICE EXPERIENCE',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecc(kule,'INDICE EXPERIENCE',1,clexp,ilong,ierr)
  else
    clexp='SCM ARPEGE/ALADIN'
  endif
  write(kuldoc,fmt='(9a)') '#ORIGINE=',clexp(1:len_trim(clexp))
endif
end
