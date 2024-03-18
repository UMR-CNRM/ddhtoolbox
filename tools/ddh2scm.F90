#include"fonctions.F90"
program convert
! --------------------------------------------------------------
! Conversion DDH vers profil pour modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
USE CONSTANTES, ONLY : RCPD
USE CONSTANTES, ONLY : RCPV
USE CONSTANTES, ONLY : RG
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
integer idoc(17)
integer idate(11)
real zdoc(11)
real, allocatable :: zflux(:,:)
real, allocatable :: zvpp1(:,:),zvpp0(:,:),zvppm(:,:)
real, allocatable :: zvct1(:,:),zvct0(:,:),zvctm(:,:)
real, allocatable :: zvqv1(:,:),zvqv0(:,:),zvqvm(:,:)

real, allocatable :: ztctdivfluhor(:,:)
real, allocatable :: ztctconversi2(:,:)
real, allocatable :: zfctfluvertdyn(:,:)

real, allocatable :: ztqrsedi(:,:)
real, allocatable :: ztqisedi(:,:)
real, allocatable :: ztqssedi(:,:)
real, allocatable :: ztqgsedi(:,:)
real, allocatable :: ztqvdivfluhor(:,:)
real, allocatable :: zfqvfluvertdyn(:,:)
real, allocatable :: zfqvtur(:,:)
real, allocatable :: zfqvturconv(:,:)
real, allocatable :: zfqtprecicol(:,:)
real, allocatable :: zfqtprecicon(:,:)
real, allocatable :: zfqtprecistl(:,:)
real, allocatable :: zfqtprecistn(:,:)

real, allocatable :: zvnt1(:,:),zvnt0(:,:)
real, allocatable :: zvuu1(:,:),zvuu0(:,:)
real, allocatable :: zvvv1(:,:),zvvv0(:,:)
real, allocatable :: zvep1(:,:),zvep0(:,:)
real, allocatable :: zvab1(:),zvab0(:)
real, allocatable :: zdelp0(:,:),zaprs0(:,:),zaprsf0(:,:)
real, allocatable :: zdelp1(:,:),zaprs1(:,:),zaprsf1(:,:)
real, allocatable :: zsurf_frac_terre0(:)
real, allocatable :: zsurf_frac_terre1(:)
real, allocatable :: zsurf_ts0(:)
real, allocatable :: zsurf_ts1(:)
real, allocatable :: zsurf_tp0(:)
real, allocatable :: zsurf_tp1(:)
real, allocatable :: zprof(:)
real, allocatable :: zprec(:)
CHARACTER*200 :: CLARG
CHARACTER*200 :: CLDOC
CHARACTER*200 :: CLFS0
CHARACTER*200 :: CLFS1
integer, parameter :: jpliste=100
character*30 clliste_in(jpliste)
character*30 clliste_out(jpliste)
INTEGER :: IARG
INTEGER :: IARGCP
INTEGER :: IDIM
INTEGER :: IDOM
INTEGER :: IERR
INTEGER :: IINDAT
INTEGER :: ILEV
INTEGER :: ILEV1
INTEGER :: ILONG
INTEGER :: ISSSSS
INTEGER :: IULE
INTEGER :: IULS0
INTEGER :: IULS1
INTEGER :: JARG
INTEGER :: JDOM
INTEGER :: JLEV
REAL :: ZCP
REAL :: ZERO
REAL :: ZLAT
REAL :: ZLNP00
REAL :: ZLNP01
REAL :: ZLON
REAL :: ZOROG0
REAL :: ZOROG1
REAL :: ZP00
REAL :: ZP01
real :: ztab(1)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
zg=9.80665
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Ecriture des profils de fichiers DDH sur des fichiers type modèle 1D.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: ddh2scm FICDDH1 [FICDDH2 ... FICDDHn]'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
do jarg=1,iarg
  call getargp(jarg,clarg)
  !
  !-------------------------------------------------
  ! Ouverture du fichier LFA de DDH.
  !-------------------------------------------------
  !
  print*,'Fichier DDH ',clarg(1:len_trim(clarg))
  iule=23
  call lfaouv(iule,clarg,'R')
  !
  !-------------------------------------------------
  ! Nombre de niveaux, domaines, etc...
  !-------------------------------------------------
  !
  call lfaleci(iule,'DOCFICHIER',17,idoc,ilong,ierr)
  ilev=idoc(6)
  idom=idoc(15)
  print*,'	',idom,' domaine(s), ',ilev,' niveau(x).'
  !
  !-------------------------------------------------
  ! Echéance.
  !-------------------------------------------------
  !
  call lfalecr(iule,'ECHEANCE',1,ztab,ilong,ierr)
  zech=ztab(1)
  !
  !-------------------------------------------------
  ! Echéance des variables.
  !-------------------------------------------------
  !
  call lfacas(iule,'ECHEANCEDV',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,'ECHEANCEDV',1,ztab,ilong,ierr)
    zechdv=ztab(1)
  else
    zechdv=zech
  endif
  !
  !-------------------------------------------------
  ! Indice expérience.
  !-------------------------------------------------
  !
  call lfalecc(iule,'INDICE EXPERIENCE',1,clnamx,ilong,ierr)
  !
  !-------------------------------------------------
  ! On alloue les tableau 2D ().
  !-------------------------------------------------
  !
  if(.not. allocated(zvpp0)) then
    allocate(zvpp0(ilev,idom))
    allocate(zvppm(ilev,idom))
    allocate(zvct0(ilev,idom))
    allocate(zvctm(ilev,idom))
    allocate(zvqv0(ilev,idom))
    allocate(zvqvm(ilev,idom))
    allocate(ztqvdivfluhor(ilev,idom))
    allocate(ztqrsedi(ilev,idom))
    allocate(ztqisedi(ilev,idom))
    allocate(ztqssedi(ilev,idom))
    allocate(ztqgsedi(ilev,idom))
    allocate(ztctdivfluhor(ilev,idom))
    allocate(ztctconversi2(ilev,idom))
    allocate(zfqvfluvertdyn(ilev+1,idom))
    allocate(zfctfluvertdyn(ilev+1,idom))
    allocate(zfqvtur(ilev+1,idom))
    allocate(zfqvturconv(ilev+1,idom))
    allocate(zfqtprecicol(ilev+1,idom))
    allocate(zfqtprecicon(ilev+1,idom))
    allocate(zfqtprecistl(ilev+1,idom))
    allocate(zfqtprecistn(ilev+1,idom))
    allocate(zvnt0(ilev,idom))
    allocate(zvuu0(ilev,idom))
    allocate(zvvv0(ilev,idom))
    allocate(zvep0(ilev,idom))
    allocate(zvab0(ilev+1))
    allocate(zaprs0(ilev+1,idom))
    allocate(zaprsf0(ilev,idom))
    allocate(zdelp0(ilev,idom))
    allocate(zaprs1(ilev+1,idom))
    allocate(zaprsf1(ilev,idom))
    allocate(zdelp1(ilev,idom))

    allocate(zflux(ilev+1,idom))
    allocate(zvpp1(ilev,idom))
    allocate(zvct1(ilev,idom))
    allocate(zvqv1(ilev,idom))
    allocate(zvnt1(ilev,idom))
    allocate(zvuu1(ilev,idom))
    allocate(zvvv1(ilev,idom))
    allocate(zvep1(ilev,idom))
    allocate(zvab1(ilev+1))

    allocate(zsurf_frac_terre0(idom))
    allocate(zsurf_frac_terre1(idom))
    allocate(zsurf_ts0(idom))
    allocate(zsurf_ts1(idom))
    allocate(zsurf_tp0(idom))
    allocate(zsurf_tp1(idom))
    allocate(zprof(ilev+1))
    allocate(zprec(ilev+1))
  endif
  !
  !-------------------------------------------------
  ! Lecture sur le fichier de DDH.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Variables.
  !-------------------------------------------------
  !
  idim=idom*ilev
  idim1=idom*(ilev+1)
  call lfalecr(iule,'VPP0',idim,zvpp0,ilong,ierr)
  call lfalecr(iule,'VPP1',idim,zvpp1,ilong,ierr)
  call lfalecr(iule,'VCT0',idim,zvct0,ilong,ierr)
  call lfalecr(iule,'VCT1',idim,zvct1,ilong,ierr)
  call lfalecr(iule,'VQV0',idim,zvqv0,ilong,ierr)
  call lfalecr(iule,'VQV1',idim,zvqv1,ilong,ierr)
  call lfalecr(iule,'VNT0',idim,zvnt0,ilong,ierr)
  call lfalecr(iule,'VNT1',idim,zvnt1,ilong,ierr)
  call lfalecr(iule,'VUU0',idim,zvuu0,ilong,ierr)
  call lfalecr(iule,'VUU1',idim,zvuu1,ilong,ierr)
  call lfalecr(iule,'VVV0',idim,zvvv0,ilong,ierr)
  call lfalecr(iule,'VVV1',idim,zvvv1,ilong,ierr)
  call lfalecr(iule,'VEP0',idim,zvep0,ilong,ierr)
  call lfalecr(iule,'VEP1',idim,zvep1,ilong,ierr)
  !
  !-------------------------------------------------
  ! Flux d'humidité.
  !-------------------------------------------------
  !
  clna='FQVFLUVERTDYN'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqvfluvertdyn,ilong,ierr)
  else
    zfqvfluvertdyn=0.
  endif
  clna='FQVTUR'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqvtur,ilong,ierr)
  else
    zfqvtur=0.
  endif
  clna='FQVTURCONV'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqvturconv,ilong,ierr)
  else
    zfqvturconv=0.
  endif
  clna='FQTPRECICOL'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqtprecicol,ilong,ierr)
  else
    zfqtprecicol=0.
  endif
  clna='FQTPRECICON'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqtprecicon,ilong,ierr)
  else
    zfqtprecicon=0.
  endif
  clna='FQTPRECISTL'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqtprecistl,ilong,ierr)
  else
    zfqtprecistl=0.
  endif
  clna='FQTPRECISTN'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfqtprecistn,ilong,ierr)
  else
    zfqtprecistn=0.
  endif
  clna='TQVDIVFLUHOR'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,ztqvdivfluhor,ilong,ierr)
  else
    ztqvdivfluhor=0.
  endif
  !
  !-------------------------------------------------
  ! Flux de chaleur.
  !-------------------------------------------------
  !
  clna='FCTFLUVERTDYN'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,zfctfluvertdyn,ilong,ierr)
  else
    zfctfluvertdyn=0.
  endif
  !
  !-------------------------------------------------
  ! TCTDIVFLUHOR.
  !-------------------------------------------------
  !
  clna='TCTDIVFLUHOR'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,ztctdivfluhor,ilong,ierr)
  else
    ztctdivfluhor=0.
  endif
  !
  !-------------------------------------------------
  ! TCTCONVERSI2.
  !-------------------------------------------------
  !
  clna='TCTCONVERSI2'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,clna,idim1,ztctconversi2,ilong,ierr)
  else
    ztctconversi2=0.
  endif
  !
  !-------------------------------------------------
  ! Champs de surface.
  !-------------------------------------------------
  !
  clna='S01_0'
  call lfacas(iule,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    !
    !-------------------------------------------------
    ! Champ de surface: fraction de terre.
    !-------------------------------------------------
    !
    call lfalecr(iule,'S01_0',idom,zsurf_frac_terre0,ilong,ierr)
    call lfalecr(iule,'S01_1',idom,zsurf_frac_terre1,ilong,ierr)
    !
    !-------------------------------------------------
    ! Champ de surface: Ts.
    !-------------------------------------------------
    !
    call lfalecr(iule,'S02_0',idom,zsurf_ts0,ilong,ierr)
    call lfalecr(iule,'S02_1',idom,zsurf_ts1,ilong,ierr)
    zcoec=1.e-5 ! valeur du hsol de la physique.
    zsurf_ts0=zsurf_ts0*zcoec
    zsurf_ts1=zsurf_ts1*zcoec
    !
    !-------------------------------------------------
    ! Champ de surface: Tp.
    !-------------------------------------------------
    !
    call lfalecr(iule,'S03_0',idom,zsurf_tp0,ilong,ierr)
    call lfalecr(iule,'S03_1',idom,zsurf_tp1,ilong,ierr)
    zcoec=1.e-5 /5. ! valeur de hsol/rtiner de la physique.
    zsurf_tp0=zsurf_tp0*zcoec
    zsurf_tp1=zsurf_tp1*zcoec
  endif
  !
  !-------------------------------------------------
  ! Test d'existence de VPPM.
  !-------------------------------------------------
  !
  call lfacas(iule,'VPPM',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iule,'VPPM',idim,zvppm,ilong,ierr)
  else
    zvppm=0.5*(zvpp0+zvpp1)*zech
  endif
  do jdom=1,idom
    do jlev=1,ilev
      !
      !-------------------------------------------------
      ! Champs initiaux.
      !-------------------------------------------------
      !
      zvqv0(jlev,jdom)=zvqv0(jlev,jdom)/zvpp0(jlev,jdom)
      zvnt0(jlev,jdom)=zvnt0(jlev,jdom)/zvpp0(jlev,jdom)
      zcp=rcpd+(rcpv-rcpd)*zvqv0(jlev,jdom)
      zvct0(jlev,jdom)=zvct0(jlev,jdom)/zvpp0(jlev,jdom)/zcp
      zvuu0(jlev,jdom)=zvuu0(jlev,jdom)/zvpp0(jlev,jdom)
      zvvv0(jlev,jdom)=zvvv0(jlev,jdom)/zvpp0(jlev,jdom)
      zvep0(jlev,jdom)=zvep0(jlev,jdom)/zvpp0(jlev,jdom)
      !
      !-------------------------------------------------
      ! Champs finaux.
      !-------------------------------------------------
      !
      zvqv1(jlev,jdom)=zvqv1(jlev,jdom)/zvpp1(jlev,jdom)
      zvnt1(jlev,jdom)=zvnt1(jlev,jdom)/zvpp1(jlev,jdom)
      zcp=rcpd+(rcpv-rcpd)*zvqv1(jlev,jdom)
      zvct1(jlev,jdom)=zvct1(jlev,jdom)/zvpp1(jlev,jdom)/zcp
      zvuu1(jlev,jdom)=zvuu1(jlev,jdom)/zvpp1(jlev,jdom)
      zvvv1(jlev,jdom)=zvvv1(jlev,jdom)/zvpp1(jlev,jdom)
      zvep1(jlev,jdom)=zvep1(jlev,jdom)/zvpp1(jlev,jdom)
      !
      !-------------------------------------------------
      ! Champs de tendance.
      !-------------------------------------------------
      !
      zvqvm(jlev,jdom)=(zvqv1(jlev,jdom)-zvqv0(jlev,jdom))/zechdv
      zvctm(jlev,jdom)=(zvct1(jlev,jdom)-zvct0(jlev,jdom))/zechdv
    enddo
    !
    !-------------------------------------------------
    ! Ouverture du fichier de sortie.
    !-------------------------------------------------
    !

    write(clfs0,fmt='(2a,i3.3,a)') clarg(1:len_trim(clarg)),'.Dom',jdom,'.Var_ini.scm'
    iuls0=26
    call lfaouv(iuls0,clfs0,'W')

    write(clfs1,fmt='(2a,i3.3,a)') clarg(1:len_trim(clarg)),'.Dom',jdom,'.Var_fin.scm'
    iuls1=24
    call lfaouv(iuls1,clfs1,'W')
    !
    !-------------------------------------------------
    ! Lat-lon.
    !-------------------------------------------------
    !
    write(cldoc,fmt='(a,i3.3)') 'DOCD',jdom
    call lfalecr(iule,cldoc,11,zdoc,ilong,ierr)
    if(nint(zdoc(11)) == 1 .or. nint(zdoc(11)) == 4) then
      !
      !-------------------------------------------------
      ! Type point.
      !-------------------------------------------------
      !
      zlon=zdoc(3)
      zlat=asin(zdoc(4))
    elseif(nint(zdoc(11)) == 2) then
      !
      !-------------------------------------------------
      ! Type quadrilatère.
      !-------------------------------------------------
      !
      zlon=0.5*(zdoc(3)+zdoc(5))
      zlat=0.5*(asin(zdoc(4))+asin(zdoc(6)))
    elseif(nint(zdoc(11)) == 3) then
      !
      !-------------------------------------------------
      ! Type rectangle.
      !-------------------------------------------------
      !
      zlon=0.5*(zdoc(3)+zdoc(5))
      zlat=0.5*(asin(zdoc(4))+asin(zdoc(8)))
    elseif(nint(zdoc(11)) == 5) then
      !
      !-------------------------------------------------
      ! Type globe.
      !-------------------------------------------------
      !
      zlon=0.
      zlat=0.
    elseif(nint(zdoc(11)) == 6) then
      !
      !-------------------------------------------------
      ! Type bande de latitude.
      !-------------------------------------------------
      !
      zlon=0.
      zlat=asin(zdoc(4))
    else
      !
      !-------------------------------------------------
      ! Autres types.
      !-------------------------------------------------
      !
      zlon=0.
      zlat=0.
    endif
    ztab(1)=zlon
    call lfaecrr(iuls0,'LONGITUDE',ztab,1)
    call lfaecrr(iuls1,'LONGITUDE',ztab,1)

    ztab(1)=zlat
    call lfaecrr(iuls0,'LATITUDE',ztab,1)
    call lfaecrr(iuls1,'LATITUDE',ztab,1)
    !
    !-------------------------------------------------
    ! Date.
    !-------------------------------------------------
    !
    call lfaleci(iule,'DATE',11,idate,ilong,ierr)
    iindat=idate(1)*10000+idate(2)*100+idate(3)
    call lfaecri(iuls0,'KINDAT',iindat,1)
    call lfaecri(iuls1,'KINDAT',iindat,1)
    isssss=idate(4)*3600+idate(5)*60
    call lfaecri(iuls0,'KSSSSS',isssss,1)
    call lfaecri(iuls1,'KSSSSS',isssss,1)
    !if(idate(6) == 1) then
    !	rstati=3600.*real(idate(7))
    !else
    !	rstati=86400.*real(idate(7))
    !endif
    rstati=zech
    zero=0.
    ztab(1)=zero
    call lfaecrr(iuls0,'RSTATI',ztab,1)
    ztab(1)=rstati
    call lfaecrr(iuls1,'RSTATI',ztab,1)
    call lfaecri(iuls0,'KLEV',ilev,1)
    call lfaecri(iuls1,'KLEV',ilev,1)
    !
    !-------------------------------------------------
    ! Indice expérience.
    !-------------------------------------------------
    !
    call lfaecrc(iuls0,'INDICE EXPERIENCE',clnamx,1)
    call lfaecrc(iuls1,'INDICE EXPERIENCE',clnamx,1)
    !
    !-------------------------------------------------
    ! Pression.
    !-------------------------------------------------
    !
    zvab1=0.
    ilev1=ilev+1
    idim1=idom*ilev1
    call lfaecrr(iuls0,'VAH',zvab1,ilev1)
    call lfaecrr(iuls1,'VAH',zvab1,ilev1)
    !
    ! PDELP.
    !
    do jlev=1,ilev
      zdelp0(jlev,jdom)=zvpp0(jlev,jdom)*rg
      zdelp1(jlev,jdom)=zvpp1(jlev,jdom)*rg
    enddo
    zvab1(1)=0.
    zvab0=zvab1
    zaprs0(1,jdom)=0.
    zaprs1(1,jdom)=0.
    !
    ! A, B, PAPRS.
    !
    do jlev=2,ilev+1
      zvab0(jlev)=zvab0(jlev-1)+zvpp0(jlev-1,jdom)*rg
      zvab1(jlev)=zvab1(jlev-1)+zvpp1(jlev-1,jdom)*rg
      zaprs0(jlev,jdom)=zvab0(jlev)
      zaprs1(jlev,jdom)=zvab1(jlev)
    enddo
    !
    ! PAPRSF.
    !
    do jlev=1,ilev
      zaprsf0(jlev,jdom)=0.5*(zaprs0(jlev,jdom)+zaprs0(jlev+1,jdom))
      zaprsf1(jlev,jdom)=0.5*(zaprs1(jlev,jdom)+zaprs1(jlev+1,jdom))
    enddo
    !
    ! Ecriture sur fichier de PDELP.
    !
    do jlev=1,ilev
      zprof(jlev)=zdelp0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PDELP',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zdelp1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PDELP',zprof,ilev)
    !
    ! Ecriture sur fichier de PAPRSF.
    !
    do jlev=1,ilev
      zprof(jlev)=zaprsf0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PAPRSF',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zaprsf1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PAPRSF',zprof,ilev)
    !
    ! Ecriture sur fichier de PAPRS.
    !
    do jlev=1,ilev+1
      zprof(jlev)=zaprs0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PAPRS',zprof,ilev1)
    do jlev=1,ilev+1
      zprof(jlev)=zaprs1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PAPRS',zprof,ilev1)
    !
    ! Logarithme de la pression de surface.
    !
    zp00=zvab0(ilev+1)
    zp01=zvab1(ilev+1)
    zlnp00=log(zp00)
    zlnp01=log(zp01)
    zorog0=zvep0(ilev,jdom)-0.5*(zvep0(ilev-1,jdom)-zvep0(ilev,jdom))
    zorog1=zvep1(ilev,jdom)-0.5*(zvep1(ilev-1,jdom)-zvep1(ilev,jdom))
    ztab(1)=zlnp00
    call lfaecrr(iuls0,'PSPT0',ztab,1)
    ztab(1)=zlnp01
    call lfaecrr(iuls1,'PSPT0',ztab,1)

    ztab(1)=zorog0
    call lfaecrr(iuls0,'POROG',ztab,1)
    ztab(1)=zorog1
    call lfaecrr(iuls1,'POROG',ztab,1)

    do jlev=1,ilev+1
      zvab0(jlev)=zvab0(jlev)/zp00
      zvab1(jlev)=zvab1(jlev)/zp01
    enddo
    call lfaecrr(iuls0,'VBH',zvab0,ilev1)
    call lfaecrr(iuls1,'VBH',zvab1,ilev1)
    !
    !-------------------------------------------------
    ! Géopotentiel.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvep0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PAPHIF',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvep1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PAPHIF',zprof,ilev)
    !
    !-------------------------------------------------
    ! Géopotentiel, demi-niveaux.
    !-------------------------------------------------
    !
    do jlev=1,ilev+1
      if(jlev == 1) then
        !
        ! Sommet du modèle.
        !
        zprof(jlev)=zvep0(1,jdom)+0.5*(zvep0(1,jdom)-zvep0(2,jdom))
      elseif(jlev == ilev+1) then
        !
        ! Base du modèle.
        !
        zprof(jlev)=zvep0(jlev-1,jdom)-0.5*(zvep0(jlev-2,jdom)-zvep0(jlev-1,jdom))
      else
        !
        ! Cas général.
        !
        zprof(jlev)=0.5*(zvep0(jlev-1,jdom)+zvep0(jlev,jdom))
      endif
    enddo
    call lfaecrr(iuls0,'PAPHI',zprof,ilev1)
    do jlev=1,ilev+1
      if(jlev == 1) then
        !
        ! Sommet du modèle.
        !
        zprof(jlev)=zvep1(1,jdom)+0.5*(zvep1(1,jdom)-zvep1(2,jdom))
      elseif(jlev == ilev+1) then
        !
        ! Base du modèle.
        !
        zprof(jlev)=zvep1(jlev-1,jdom)-0.5*(zvep1(jlev-2,jdom)-zvep1(jlev-1,jdom))
      else
        !
        ! Cas général.
        !
        zprof(jlev)=0.5*(zvep1(jlev-1,jdom)+zvep1(jlev,jdom))
      endif
    enddo
    call lfaecrr(iuls1,'PAPHI',zprof,ilev1)
    !
    !-------------------------------------------------
    ! Température.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvct0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PTT0',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvct1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PTT0',zprof,ilev)
    !
    !-------------------------------------------------
    ! qv.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvqv0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PQT0',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvqv1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PQT0',zprof,ilev)
    !
    !-------------------------------------------------
    ! Nebulosité.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvnt0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PNEB',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvnt1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PNEB',zprof,ilev)
    !
    !-------------------------------------------------
    ! u.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvuu0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PUT0',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvuu1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PUT0',zprof,ilev)
    !
    !-------------------------------------------------
    ! v.
    !-------------------------------------------------
    !
    do jlev=1,ilev
      zprof(jlev)=zvvv0(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'PVT0',zprof,ilev)
    do jlev=1,ilev
      zprof(jlev)=zvvv1(jlev,jdom)
    enddo
    call lfaecrr(iuls1,'PVT0',zprof,ilev)
    !
    !-------------------------------------------------
    ! Flux.
    !-------------------------------------------------
    !
    iliste=0
    !
    ! Flux de qv.
    !
    iliste=iliste+1 ; clliste_in(iliste)='FQVTUR     ' ; clliste_out(iliste)='PDIFTQ'
    iliste=iliste+1 ; clliste_in(iliste)='FQTPRECISTL' ; clliste_out(iliste)='PFPLSL'
    iliste=iliste+1 ; clliste_in(iliste)='FQTPRECISTN' ; clliste_out(iliste)='PFPLSN'
    iliste=iliste+1 ; clliste_in(iliste)='FQTPRECICOL' ; clliste_out(iliste)='PFPLCL'
    iliste=iliste+1 ; clliste_in(iliste)='FQTPRECICON' ; clliste_out(iliste)='PFPLCN'
    iliste=iliste+1 ; clliste_in(iliste)='FQVTURCONV ' ; clliste_out(iliste)='PDIFCQ'
    !
    ! Flux de chaleur.
    !
    iliste=iliste+1 ; clliste_in(iliste)='FCTRAYSOL1  ' ; clliste_out(iliste)='PFRSO'
    iliste=iliste+1 ; clliste_in(iliste)='FCTRAYTER1  ' ; clliste_out(iliste)='PFRTH'

    iliste=iliste+1 ; clliste_in(iliste)='FCTTUR      ' ; clliste_out(iliste)='PDIFTS'

    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECISTL ' ; clliste_out(iliste)='PFHPSL'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECISTN ' ; clliste_out(iliste)='PFHPSN'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECCSSTL ' ; clliste_out(iliste)='PFHSSL'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECCSSTN ' ; clliste_out(iliste)='PFHSSN'

    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECICOL ' ; clliste_out(iliste)='PFHPCL'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECICON ' ; clliste_out(iliste)='PFHPCN'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECCSCOL ' ; clliste_out(iliste)='PFHSCL'
    iliste=iliste+1 ; clliste_in(iliste)='FCTPRECCSCON ' ; clliste_out(iliste)='PFHSCN'
    iliste=iliste+1 ; clliste_in(iliste)='FCTTURCONV  ' ; clliste_out(iliste)='PDIFCS'
    do jliste=1,iliste
      call lfacas(iule,clliste_in(jliste),cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,clliste_in(jliste),idim1,zflux,ilong,ierr)
        do jlev=1,ilev1
          zprof(jlev)=zflux(jlev,jdom)/zech
        enddo
        call lfaecrr(iuls0,clliste_out(jliste),zprof,ilev1)
        call lfaecrr(iuls1,clliste_out(jliste),zprof,ilev1)
      endif
    enddo
    !
    !-------------------------------------------------
    ! Bilan de qv DTM: Dynamique, Transport sous-maille, Microphysique.
    !-------------------------------------------------
    !
    !
    ! Tendance totale de qv.
    !
    do jlev=1,ilev
      zprof(jlev)=zvqvm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTTOT',zprof,ilev)
    call lfaecrr(iuls1,'QVTTOT',zprof,ilev)
    !
    ! Tendance dynamique.
    !
    do jlev=1,ilev
      zprof(jlev)=ztqvdivfluhor(jlev,jdom)/zvppm(jlev,jdom) &
      & -(zfqvfluvertdyn(jlev+1,jdom)-zfqvfluvertdyn(jlev,jdom))/zvppm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTDYN',zprof,ilev)
    call lfaecrr(iuls1,'QVTDYN',zprof,ilev)
    !
    ! Transport sous-maille.
    !
    do jlev=1,ilev
      zprof(jlev)=-( &
      &  zfqvtur(jlev+1,jdom)-zfqvtur(jlev,jdom) &
      & +zfqvturconv(jlev+1,jdom)-zfqvturconv(jlev,jdom) &
      & )/zvppm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTSGSTRANSP',zprof,ilev)
    call lfaecrr(iuls1,'QVTSGSTRANSP',zprof,ilev)
    !
    ! Turbulence.
    !
    do jlev=1,ilev
      zprof(jlev)=-( &
      &  zfqvtur(jlev+1,jdom)-zfqvtur(jlev,jdom) &
      & )/zvppm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTTURB',zprof,ilev)
    call lfaecrr(iuls1,'QVTTURB',zprof,ilev)
    !
    ! Transport sous-maille convectif.
    !
    do jlev=1,ilev
      zprof(jlev)=-( &
      & zfqvturconv(jlev+1,jdom)-zfqvturconv(jlev,jdom) &
      & )/zvppm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTSGSTRANSPCONV',zprof,ilev)
    call lfaecrr(iuls1,'QVTSGSTRANSPCONV',zprof,ilev)
    !
    ! Microphysique.
    !
    do jlev=1,ilev
      zprof(jlev)=-( &
      &  zfqtprecicol(jlev+1,jdom)-zfqtprecicol(jlev,jdom) &
      & +zfqtprecicon(jlev+1,jdom)-zfqtprecicon(jlev,jdom) &
      & +zfqtprecistl(jlev+1,jdom)-zfqtprecistl(jlev,jdom) &
      & +zfqtprecistn(jlev+1,jdom)-zfqtprecistn(jlev,jdom) &
      & )/zvppm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'QVTMICROPHYS',zprof,ilev)
    call lfaecrr(iuls1,'QVTMICROPHYS',zprof,ilev)
    !
    !-------------------------------------------------
    ! Cas de fichiers de DDH AROME.
    ! On convertit les tendances de QR en flux de précipitations.
    !-------------------------------------------------
    !
    clna='TQRSEDI'
    call lfacas(iule,clna,cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,clna,idim1,ztqrsedi,ilong,ierr)
      zprec(:)=0.
      do jlev=2,ilev
        zprof(jlev)=zprof(jlev-1)-ztqrsedi(jlev,jdom)*zvppm(jlev,jdom)
      enddo
      call lfaecrr(iuls1,'PFPLSL',zprof,ilev1)
    endif
    !
    !-------------------------------------------------
    ! Cas de fichiers de DDH AROME.
    ! On convertit les tendances de QI en flux de précipitations.
    !-------------------------------------------------
    !
    clna='TQISEDI'
    call lfacas(iule,clna,cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,clna,idim1,ztqisedi,ilong,ierr)
      call lfalecr(iule,clna,idim1,ztqssedi,ilong,ierr)
      call lfalecr(iule,clna,idim1,ztqgsedi,ilong,ierr)
      zprec(:)=0.
      do jlev=2,ilev
        zprof(jlev)=zprof(jlev-1)-(ztqisedi(jlev,jdom)+ztqssedi(jlev,jdom)+ztqgsedi(jlev,jdom))*zvppm(jlev,jdom)
      enddo
      call lfaecrr(iuls1,'PFPLSN',zprof,ilev1)
      !
      ! Dans le cas d'AROME les précipitations sous-maille sont nulles.
      !
      zprof(:)=0.
      call lfaecrr(iuls1,'PFPLCL',zprof,ilev1)
      call lfaecrr(iuls1,'PFPLCN',zprof,ilev1)
    endif
    !
    !-------------------------------------------------
    ! Bilan de T DTM: Dynamique, Transport sous-maille, Microphysique.
    !-------------------------------------------------
    !
    !
    ! Tendance totale de T.
    !
    do jlev=1,ilev
      zprof(jlev)=zvctm(jlev,jdom)
    enddo
    call lfaecrr(iuls0,'TTTOT',zprof,ilev)
    call lfaecrr(iuls1,'TTTOT',zprof,ilev)
    !
    ! Tendance dynamique.
    !
    do jlev=1,ilev
      zprof(jlev)=(ztctdivfluhor(jlev,jdom)+ztctconversi2(jlev,jdom) &
      & -(zfctfluvertdyn(jlev+1,jdom)-zfctfluvertdyn(jlev,jdom)))/zvppm(jlev,jdom)/zcp
    enddo
    call lfaecrr(iuls0,'TTDYN',zprof,ilev)
    call lfaecrr(iuls1,'TTDYN',zprof,ilev)
    !
    !-------------------------------------------------
    ! Ecriture de champs de surface sur le fichier de sortie.
    ! Ce code est commenté jusqu'à ce que le champ S01 des DDH
    ! ne soit plus pondéré par PCT dans le code 3D!...
    ! En effet actuellement il est contre-productif de sortir ce PTS0.
    !-------------------------------------------------
    !
    !if(zsurf_frac_terre0(jdom) > 0.) then
    !	ilong=1
    !	call lfaecrr(iuls0,'PTS0',zsurf_ts0(jdom),ilong)
    !	call lfaecrr(iuls1,'PTS0',zsurf_ts1(jdom),ilong)
    !	ilong=1
    !	call lfaecrr(iuls0,'PTP0',zsurf_tp0(jdom),ilong)
    !	call lfaecrr(iuls1,'PTP0',zsurf_tp1(jdom),ilong)
    !endif
    !
    !-------------------------------------------------
    ! Fermeture du fichier de sortie.
    !-------------------------------------------------
    !
    call lfafer(iuls1)
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier LFA de DDH.
  !-------------------------------------------------
  !
  call lfafer(iule)
  !
  !-------------------------------------------------
  ! On désalloue les tableaux 2D.
  !-------------------------------------------------
  !
  deallocate(zvpp0)
  deallocate(zvppm)
  deallocate(zvct0)
  deallocate(zvctm)
  deallocate(zvqv0)
  deallocate(zvqvm)
  deallocate(ztqvdivfluhor)
  deallocate(ztqrsedi)
  deallocate(ztqisedi)
  deallocate(ztqssedi)
  deallocate(ztqgsedi)
  deallocate(ztctdivfluhor)
  deallocate(ztctconversi2)
  deallocate(zfqvfluvertdyn)
  deallocate(zfctfluvertdyn)
  deallocate(zfqvtur)
  deallocate(zfqvturconv)
  deallocate(zfqtprecicol)
  deallocate(zfqtprecicon)
  deallocate(zfqtprecistl)
  deallocate(zfqtprecistn)
  deallocate(zvnt0)
  deallocate(zvuu0)
  deallocate(zvvv0)
  deallocate(zvep0)
  deallocate(zvab0)

  deallocate(zflux)
  deallocate(zvpp1)
  deallocate(zvct1)
  deallocate(zvqv1)
  deallocate(zvnt1)
  deallocate(zvuu1)
  deallocate(zvvv1)
  deallocate(zvep1)
  deallocate(zvab1)

  deallocate(zsurf_frac_terre0)
  deallocate(zsurf_frac_terre1)
  deallocate(zsurf_ts0)
  deallocate(zsurf_ts1)
  deallocate(zsurf_tp0)
  deallocate(zsurf_tp1)
  deallocate(zprof)
  deallocate(zprec)
  deallocate(zaprs0)
  deallocate(zaprs1)
  deallocate(zaprsf0)
  deallocate(zaprsf1)
  deallocate(zdelp0)
  deallocate(zdelp1)
enddo ! jarg
end
