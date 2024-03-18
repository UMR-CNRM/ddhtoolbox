#include"fonctions.F90"
program ms
! --------------------------------------------------------------
! Model Sounding: vertical profiles of potential temperatures.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use constantes
implicit character*200 (c)
implicit logical (l)
parameter(jplev=1000)
parameter(jplev1=jplev+1)
real zt(jplev)
real ztu(jplev)
real zqu(jplev)
real zqv(jplev)
real zql(jplev)
real zqi(jplev)
real zqc(jplev)
real zp(jplev)
real zcooy_pres(jplev)
real zcooy_heig(jplev)
real zcooy_heig_m(jplev)
real zcooy(jplev1)
real zu(jplev)
real zv(jplev)
real ztheta(jplev1)
real ztheta_ud(jplev1)
real zthetav(jplev1)
real zthetavl(jplev1)
real zphi(jplev1)
real zphif(jplev)
real zlu(jplev1)
real ztmp(jplev1)
real ztab(1)
logical :: lldebug
!
!-------------------------------------------------
! Ascendance convective, si disponible.
!-------------------------------------------------
!
real zasc_tt_ascendance(jplev)
real zasc_qv_ascendance(jplev)

!
!-------------------------------------------------
! Initialisations.
!-------------------------------------------------
!
lldebug=.false.
zcondr=atan(1.)/45.
zindef=8.25621e12
zsaut=999.999
zu=zindef
zv=zindef
clcooy='PRE'
clcooy='HEI'
zindef=-8.52145e30
zxnuage=zindef
!
! -------------------------------------------------
! Valeurs par défaut.
! -------------------------------------------------
!
llverbose=.false.

clmethode='PAH'
clmethode='AS'
clmethode='E761'

llasc=.false.
!
!-------------------------------------------------
! Valeurs par défaut de post-processing.
!-------------------------------------------------
!
llpptheta=.true.
llppthetal=.false.
llppthetav=.false.
llppthetavl=.false.
llppthetae_top=.false.
llppthetae=.false. ; llppthetaes=.false. ; llppthetae_bolton=.true. ; llppthetaes_bolton=.true.
llppthetad=.false.
llppthetaw=.false.
llppthetacin=.false.
llppthetacape=.false.
llpppointc=.true.
llpppointcl=.false.
llpppoints=.false.
llppnuage_conv=.true.
llppnuage_satu=.true.
llpp_fdc_thetapw=.true.
llpp_fdc_theta=.false.
llasctest=.false.
!
!-------------------------------------------------
! Afin de ne pas sortir sur le graphique
! les lieux voisins de la tropopause où theta
! atteint de très fortes valeurs,
! on fixe la limite en theta traçable à zxmax.
!-------------------------------------------------
!
zxmax=393.
ztheta=zindef
zthetav=zindef
zthetavl=zindef
!
! -------------------------------------------------
! Saisie de la ligne de commande.
! -------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  !
  ! -------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  ! -------------------------------------------------
  !
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Model Sounding: starting from a SCM file, compute vertical profiles of potential temperatures,'
  write(*,'(9a)') '  humidity, wind, etc...'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Usage: ms [-t] [-yY] [-verb] PROFILE1 [PROFILE2 ... PROFILEn]'
  write(*,'(9a)') 'with'
  write(*,'(9a)') '  -t kind of T-Phi-gram: '
  write(*,'(9a)') '    AS: adiabatic ascents are vertical'
  write(*,'(9a)') '    PAH: irreversible pseudo-adiabatic ascents are vertical'
  write(*,'(9a)') '    E761: local T corrected as a linear function of p'
  write(*,'(9a)') '    Default: ',trim(clmethode)
  write(*,'(9a)') '  -yY kind of vertical coordinate (y coordinate) to be computed:'
  write(*,'(9a)') '    if Y is LEV the vertical coordinate will be the levels.'
  write(*,'(9a)') '    if Y is PRE the vertical coordinate will be the pressure (hPa).'
  write(*,'(9a)') '    if Y is HEI the vertical coordinate will be the height (km).'
  write(*,'(9a)') '    Default: ',trim(clcooy)
  write(*,'(9a)') '  -verb: verbose mode: prints out details on the ascent based on a parcel raising from the lowest level.'
  write(*,'(9a)') '    Writes out some ascent profile data on default FORTRAN files (usually named fort.*)'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Remark: if the environment variable RS is put to a name'
  write(*,'(9a)') '        of a file containing LONG LAT NAME COUNTRY'
  write(*,'(9a)') '        ms will propose the nearest location to the current point.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'CIN/CAPE: the CIN and CAPE of the level for which the CAPE is maximum are printed.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Examples: '
  write(*,'(9a)') '  ms -tAS -yHEI Profile.007.00'
  write(*,'(9a)') '  ms -verb Profile.007.00'
  write(*,'(9a)') ' '
  ! -------------------------------------------------
  stop
else
  !
  ! -------------------------------------------------
  ! Nombre d'arguments OK.
  ! -------------------------------------------------
  !
  do jarg=1,iarg
    call getargp(jarg,clarg)
    if(clarg(1:2) == '-y') then
      !
      !-------------------------------------------------
      ! Saisie de la coordonnée verticale.
      !-------------------------------------------------
      !
      clcooy=clarg(3:)
    elseif(clarg(1:2) == '-t') then
      !
      !-------------------------------------------------
      ! Choix du T-Phi-gramme.
      !-------------------------------------------------
      !
      clmethode=clarg(3:)
    elseif(trim(clarg) == '-asctest') then
      !
      !-------------------------------------------------
      ! Test d'ascendance intra-nuageuse.
      !-------------------------------------------------
      !
      llasctest=.true.
    elseif(trim(clarg) == '-verb') then
      !
      !-------------------------------------------------
      ! Mode bavard du calcul de CIN et CAPE.
      !-------------------------------------------------
      !
      llverbose=.true.
    else
      if(trim(clmethode) == 'AS') then
        !
        !-------------------------------------------------
        ! Cas T-Phi-gramme à adiabatiques sèches verticales.
        ! Post-processing personnalisé.
        !-------------------------------------------------
        !
        llpptheta=.true.
        llppthetal=.false.
        llppthetav=.false.
        llppthetavl=.false.
        llppthetae_top=.false.
        llppthetae=.false.
        llppthetaes=.false.
        llppthetae_bolton=.true.
        llppthetaes_bolton=.true.
        llppthetad=.false.
        llppthetaw=.false.
        llppthetacin=.false.
        llppthetacape=.false.
        llpppointc=.true.
        llpppointcl=.false.
        llpppoints=.false.
        llppnuage_conv=.false.
        llppnuage_satu=.false.
        llpp_fdc_thetapw=.true.
        llpp_fdc_theta=.false.
      elseif(trim(clmethode) == 'PAH') then
        !
        !-------------------------------------------------
        ! Cas T-Phi-gramme à adiabatiques humides verticales.
        ! Post-processing personnalisé.
        !-------------------------------------------------
        !
        llpptheta=.true.
        llppthetal=.false.
        llppthetav=.false.
        llppthetavl=.false.
        llppthetae_top=.false.
        llppthetae=.false.
        llppthetaes=.false.
        llppthetae_bolton=.true.
        llppthetaes_bolton=.true.
        llppthetad=.false.
        llppthetaw=.true.
        llppthetacin=.false.
        llppthetacape=.false.
        llpppointc=.false.
        llpppointcl=.false.
        llpppoints=.false.
        llppnuage_conv=.false.
        llppnuage_satu=.false.
        llpp_fdc_thetapw=.true.
        llpp_fdc_theta=.true.
      elseif(trim(clmethode) == 'E761') then
        !
        !-------------------------------------------------
        ! Cas T-Phi-gramme type émagramme 761.
        ! Post-processing personnalisé.
        !-------------------------------------------------
        !
        llpptheta=.true.
        llppthetal=.false.
        llppthetav=.false.
        llppthetavl=.false.
        llppthetae_top=.false.
        llppthetae=.false.
        llppthetaes=.false.
        llppthetae_bolton=.false.
        llppthetaes_bolton=.false.
        llppthetad=.true.
        llppthetaw=.true.
        llppthetacin=.false.
        llppthetacape=.false.
        llpppointc=.false.
        llpppointcl=.false.
        llpppoints=.false.
        llppnuage_conv=.false.
        llppnuage_satu=.false.
        llpp_fdc_thetapw=.true.
        llpp_fdc_theta=.true.
      elseif(trim(clmethode) == 'p') then
        !
        !-------------------------------------------------
        ! Cas T-Phi-gramme type émagramme 761.
        ! Post-processing personnalisé.
        !-------------------------------------------------
        !
        llpptheta=.false.
        llppthetal=.false.
        llppthetav=.false.
        llppthetavl=.true.
        llppthetae_top=.false.
        llppthetae=.false.
        llppthetaes=.false.
        llppthetae_bolton=.true.
        llppthetaes_bolton=.true.
        llppthetad=.false.
        llppthetaw=.false.
        llppthetacin=.false.
        llppthetacape=.false.
        llpppointc=.false.
        llpppointcl=.false.
        llpppoints=.false.
        llppnuage_conv=.false.
        llppnuage_satu=.false.
        llpp_fdc_thetapw=.false.
        llpp_fdc_theta=.false.
        clmethode='AS'
        llasc=.true.
      endif
      !
      ! -------------------------------------------------
      ! L'argument est un nom de fichier.
      ! On va ouvrir ce fichier LFA.
      ! -------------------------------------------------
      !
      print*,'ms/open LFA file ',trim(clarg)
      iule=56
      if(lldebug) write(*,fmt=*) 'Debug pré lfaouv'
      call lfaouv(iule,clarg,'R')
      !
      !-------------------------------------------------
      ! Nombre de niveaux.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré KLEV'
      call lfaleci(iule,'KLEV',1,klev,ilong,ierr)
      if(klev > jplev) then
        print*,'ms/ERROR: recompile with a greater value of jplev!...'
        stop 'call abort'
      endif
      !
      !-------------------------------------------------
      ! Lecture de la température.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PTT0'
      call lfacas(iule,'PTT0',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PTT0',jplev,zt,ilong,ierr)
      else
        call lfacas(iule,'PT',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PT',jplev,zt,ilong,ierr)
        else
          print*,'ms/ERROR: neither PTT0 nor PT articles!...'
          stop 'call abort'
        endif
      endif
      !
      !-------------------------------------------------
      ! Lecture du vent.
      !-------------------------------------------------
      !
      call lfacas(iule,'PUT0',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PUT0',jplev,zu,ilong,ierr)
        call lfalecr(iule,'PVT0',jplev,zv,ilong,ierr)
        llvent=.true.
      else
        call lfacas(iule,'PU',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PU',jplev,zu,ilong,ierr)
          call lfalecr(iule,'PV',jplev,zv,ilong,ierr)
            llvent=.true.
        else
          llvent=.false.
        endif
      endif
      !
      !-------------------------------------------------
      ! Lecture de la vapeur d'eau.
      !-------------------------------------------------
      !
      call lfacas(iule,'PQT0',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PQT0',jplev,zqv,ilong,ierr)
      else
        call lfacas(iule,'PQ',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PQ',jplev,zqv,ilong,ierr)
        else
          print*,'ms/ERROR: neither PQT0 nor PQ articles!...'
          stop 'call abort'
        endif
      endif
      do jlev=1,klev
        zqv(jlev)=max(zqv(jlev),1.e-15)
      enddo
      !
      !-------------------------------------------------
      ! Lecture du profil de l'updraft.
      !-------------------------------------------------
      !
      call lfacas(iule,'PTU',cltype,ilong,ierr)
      if(ierr == 0) then
        llud=.true.
        call lfalecr(iule,'PTU',jplev,ztu,ilong,ierr)
        call lfalecr(iule,'PQU',jplev,zqu,ilong,ierr)
        do jlev=1,ilong
          if(ztu(jlev) == 0.) ztu(jlev)=zt(jlev)
          if(zqu(jlev) == 0.) zqu(jlev)=zqv(jlev)
        enddo
      else
        llud=.false.
      endif
      !
      !-------------------------------------------------
      ! Lecture des condensats.
      !-------------------------------------------------
      !
      call lfacas(iule,'PQLI',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PQLI',jplev,zql,ilong,ierr)
      else
        call lfacas(iule,'PQLT0',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PQLT0',jplev,zql,ilong,ierr)
        else
          call lfacas(iule,'PQL',cltype,ilong,ierr)
          if(ierr == 0) then
            call lfalecr(iule,'PQL',jplev,zql,ilong,ierr)
          else
            zql=0.
          endif
        endif
      endif
      call lfacas(iule,'PQICE',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PQICE',jplev,zqi,ilong,ierr)
      else
        call lfacas(iule,'PQIT0',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PQIT0',jplev,zqi,ilong,ierr)
        else
          call lfacas(iule,'PQI',cltype,ilong,ierr)
          if(ierr == 0) then
            call lfalecr(iule,'PQI',jplev,zqi,ilong,ierr)
          else
            zqi=0.
          endif
        endif
      endif
      do jlev=1,klev
        zqc(jlev)=zql(jlev)+zqi(jlev)
      enddo
      !
      !-------------------------------------------------
      ! Lecture des précipitations.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PFPLCL'
      call lfacas(iule,'PFPLCL',cltype,ilong,ierr)
      if(ierr == 0) then
        llprec=.true.
        call lfalecr(iule,'PFPLCL',jplev,zlu,ilong,ierr)
        zprec_modele_sm=zlu(ilong)*3600.
        call lfalecr(iule,'PFPLCN',jplev,zlu,ilong,ierr)
        zprec_modele_sm=zprec_modele_sm+zlu(ilong)*3600.
        call lfalecr(iule,'PFPLSL',jplev,zlu,ilong,ierr)
        zprec_modele_expl=zlu(ilong)*3600.
        call lfalecr(iule,'PFPLSN',jplev,zlu,ilong,ierr)
        zprec_modele_expl=zprec_modele_expl+zlu(ilong)*3600.
        zprec_modele=zprec_modele_sm+zprec_modele_expl
      else
        llprec=.false.
      endif
      !
      !-------------------------------------------------
      ! Lecture de l'ascendance convective.
      !-------------------------------------------------
      !
      call lfacas(iule,'T_ASC',cltype,ilong,ierr)
      if(ierr == 0) then
        llasc=.true.
        call lfalecr(iule,'T_ASC',jplev,zasc_tt_ascendance,ilong,ierr)
        call lfalecr(iule,'QV_ASC',jplev,zasc_qv_ascendance,ilong,ierr)
      else
        llasc=.false.
      endif
      call lfacas(iule,'T_ASC_CVP',cltype,ilong,ierr)
      if(ierr == 0 .and. llasctest) then
        llasc=.true.
        call lfalecr(iule,'T_ASC_CVP',jplev,zasc_tt_ascendance,ilong,ierr)
        call lfalecr(iule,'QV_ASC_CVP',jplev,zasc_qv_ascendance,ilong,ierr)
      endif
      !
      !-------------------------------------------------
      ! Altitude.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré POROG'
      zorog=zindef
      call lfacas(iule,'POROG',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'POROG',1,ztab,ilong,ierr)
        zorog=ztab(1)
      else
        call lfacas(iule,'PAPHI',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PAPHI',jplev1,ztmp,ilong,ierr)
          zorog=ztmp(ilong)
        elseif(clcooy(1:len_trim(clcooy)) == 'HEI') then
          print*,'ms/WARNING: neither POROG nor PAPHI articles!...'
          print*,'Altitude of the point arbitrarily set to 0!...'
          zorog=0.
        endif
      endif
      if(zorog /= zindef) then
        write(clalt,fmt='(a,f5.0,a)') 'Alt.=',zorog/rg,' m'
      else
        clalt=' '
      endif
      !
      !-------------------------------------------------
      ! Mer/terre.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PITM'
      call lfacas(iule,'PITM',cltype,ilong,ierr)
      if(ierr == 0) then
        llpitm=.true.
        call lfalecr(iule,'PITM',1,ztab,ilong,ierr)
        zitm=ztab(1)
        if(zitm > 0.5) then
          clitm='LAND'
        else
          clitm='SEA'
        endif
      else
        llpitm=.false.
        clitm=' '
      endif
      !
      !-------------------------------------------------
      ! Lecture de Ts.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PTS0'
      call lfacas(iule,'PTS0',cltype,ilong,ierr)
      if(ierr == 0) then
        llts=.true.
        call lfalecr(iule,'PTS0',jplev,ztab,ilong,ierr)
        zts=ztab(1)
      else
        call lfacas(iule,'PTS',cltype,ilong,ierr)
        if(ierr == 0) then
          llts=.true.
          call lfalecr(iule,'PTS',jplev,ztab,ilong,ierr)
          zts=ztab(1)
        else
          llts=.false.
        endif
      endif
      !
      !-------------------------------------------------
      ! Initialisation de la coordonnée verticale.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré coov PRE'
      call coov('PRE',klev,iule,zcooy_pres) ! pression en hPa.
      do jlev=1,klev
        zp(jlev)=-100.*zcooy_pres(jlev) ! pression en Pa.
      enddo
      !
      !-------------------------------------------------
      ! Pour que les isothermes soient cotées numériquement en face de la valeur de T du niveau le plus bas, il faut que dans le T-Phigramme on ramène à p=p_max et non pas à p=1000 hPa.
      !-------------------------------------------------
      !
      rpref=zp(klev)
      !
      !-------------------------------------------------
      ! Lecture de ps.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PSPT0'
      call lfacas(iule,'PSPT0',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PSPT0',jplev,ztab,ilong,ierr)
        zps=ztab(1)
        zps=exp(zps)
        if(zps < 10000.) then
          write(*,fmt=*) 'ms/WARNING: too small surface pressure: ',zps
          zps_arbitraire=101500.
          write(*,fmt=*) '  Taken arbitrarily equal to ',zps_arbitraire
          zps=zps_arbitraire
        endif
      else
        call lfacas(iule,'PAPRS',cltype,ilong,ierr)
        if(ierr == 0) then
          call lfalecr(iule,'PAPRS',jplev1,ztmp,ilong,ierr)
          zps=ztmp(ilong)
        else
          print*,'ms/WARNING: neither PSPT0 nor PAPRS articles!...'
          print*,'  surface pressure taken as lowest model level pressure.'
          zps=zcooy_pres(klev)
        endif
      endif
      !
      !-------------------------------------------------
      ! Choix de la coordonnée verticale de sortie.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré PRE'
      if(clcooy(1:len_trim(clcooy)) == 'PRE') then
        !
        !-------------------------------------------------
        ! p.
        !-------------------------------------------------
        !
        do jlev=1,klev
          zcooy(jlev)=zcooy_pres(jlev)
        enddo
        zcooy(klev+1)=-zps/100.
        cly='p'
        clunite_y='hPa'
        zymax=-100.
      elseif(clcooy(1:len_trim(clcooy)) == 'HEI') then
        !
        !-------------------------------------------------
        ! z.
        !-------------------------------------------------
        !
        call coov('HEI',klev,iule,zcooy_heig) ! altitude en km.
        do jlev=1,klev
          zcooy(jlev)=zcooy_heig(jlev)
        enddo
        zcooy(klev+1)=zorog/rg/1000.
        cly='altitude'
        clunite_y='km'
        zymax=11. ! sommet traçable en km.
        zymax=15. ! sommet traçable en km.
        zymax=16. ! sommet traçable en km.
      elseif(clcooy(1:len_trim(clcooy)) == 'LEV') then
        !
        !-------------------------------------------------
        ! Niveaux.
        !-------------------------------------------------
        !
        do jlev=1,klev
          zcooy(jlev)=real(-jlev)
        enddo
        zcooy(klev+1)=real(-klev-1)
        cly='levels'
        clunite_y=' '
        zymax=0. ! aucun contrôle.
      else
        print*,'ms/ERROR: unexpected vertical coordinate: ',clcooy(1:len_trim(clcooy))
        stop 'call abort'
      endif
      !
      !-------------------------------------------------
      ! La valeur max de X traçable ne doit pas excéder theta(zymax).
      !-------------------------------------------------
      !
      !zxmax=-99999.
      !do jlev=1,klev
      !  if(zcooy(jlev) < zymax) then
      !    zxmax=max(zxmax,theta(zp(jlev),zt(jlev)))
      !  endif
      !enddo
      !
      !-------------------------------------------------
      ! On ne fait la lecture de qs
      ! que si Ts est disponible.
      !-------------------------------------------------
      !
      if(llts) then
        call lfacas(iule,'PQS',cltype,ilong,ierr)
        if(ierr == 0) then
          !
          !-------------------------------------------------
          ! qs est fourni par le modèle.
          !-------------------------------------------------
          !
          call lfalecr(iule,'PQS',jplev,ztab,ilong,ierr)
          zqs=ztab(1)
          cldiag_qv='qv surf. read from file'
        elseif(llpitm .and. zitm < 0.5 .and. llts) then
          !
          !-------------------------------------------------
          ! qs n'est pas fourni par le modèle
          ! mais on sait qu'on est sur mer;
          ! qs vaut donc qsat(Ts).
          !-------------------------------------------------
          !
          zqs=fth_qs(zts,zps)
          cldiag_qv='qv surf. computed as qsat(Ts,ps)'
        else
          !
          !-------------------------------------------------
          ! qs est indisponible.
          ! On l'extrapole à partir de qv du plus bas niveau.
          !-------------------------------------------------
          !
          zqs=zqv(klev)+0.5*(zqv(klev)-zqv(klev-1))
          cldiag_qv='qv surf. extrapol. from qv(lowest level)'
        endif
      else
          cldiag_qv='No surface temperature available'
      endif
      !
      !-------------------------------------------------
      ! Valeur de theta servant à caler la gauche du graphique.
      !-------------------------------------------------
      !
      ztheta_gauche=fth_thetad(zp(klev),zqv(klev))-10.
      !
      !-------------------------------------------------
      ! Valeur de theta servant de "zone déportée" à droite du graphique.
      !-------------------------------------------------
      !
      ztheta_droite=rtt+90.
      !
      !-------------------------------------------------
      ! Fichiers recevant s, h, hs, etc...
      !-------------------------------------------------
      !
      lcpt=.false.
      if(lcpt) then
        !
        !-------------------------------------------------
        ! Fichier s.
        !-------------------------------------------------
        !
        clener_s=clarg(1:len_trim(clarg))//'.ms.tmp.ener_s.dta'
        print*,clener_s(1:len_trim(clener_s))
        open(78,file=clener_s,form='formatted')
        !
        !-------------------------------------------------
        ! Fichier h.
        !-------------------------------------------------
        !
        clener_s=clarg(1:len_trim(clarg))//'.ms.tmp.ener_h.dta'
        print*,clener_s(1:len_trim(clener_s))
        open(79,file=clener_s,form='formatted')
        !
        !-------------------------------------------------
        ! Fichier hs.
        !-------------------------------------------------
        !
        clener_s=clarg(1:len_trim(clarg))//'.ms.tmp.ener_hs.dta'
        print*,clener_s(1:len_trim(clener_s))
        open(80,file=clener_s,form='formatted')
        !
        !-------------------------------------------------
        ! Fichier hdiff.
        !-------------------------------------------------
        !
        clener_s=clarg(1:len_trim(clarg))//'.ms.tmp.ener_hdiff.dta'
        print*,clener_s(1:len_trim(clener_s))
        open(81,file=clener_s,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichier Theta.
      !-------------------------------------------------
      !
      cltheta=clarg(1:len_trim(clarg))//'.ms.tmp.theta.dta'
      print*,cltheta(1:len_trim(cltheta))
      open(22,file=cltheta,form='formatted')
      !
      !-------------------------------------------------
      ! Fichiers de theta_ud et thetaw_ud.
      !-------------------------------------------------
      !
      if(llud) then
        cltheta_ud=clarg(1:len_trim(clarg))//'.ms.tmp.theta_ud.dta'
        print*,trim(cltheta_ud)
        open(91,file=cltheta_ud,form='formatted')

        clthetaw_ud=clarg(1:len_trim(clarg))//'.ms.tmp.thetaw_ud.dta'
        print*,trim(clthetaw_ud)
        open(92,file=clthetaw_ud,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichier thetav.
      !-------------------------------------------------
      !
      clthetav=clarg(1:len_trim(clarg))//'.ms.tmp.thetav.dta'
      print*,clthetav(1:len_trim(clthetav))
      open(23,file=clthetav,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetal.
      !-------------------------------------------------
      !
      clthetal=clarg(1:len_trim(clarg))//'.ms.tmp.thetal.dta'
      print*,clthetal(1:len_trim(clthetal))
      open(41,file=clthetal,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetavl.
      !-------------------------------------------------
      !
      clthetavl=clarg(1:len_trim(clarg))//'.ms.tmp.thetavl.dta'
      print*,clthetavl(1:len_trim(clthetavl))
      open(42,file=clthetavl,form='formatted')
      if(llasc) then
        !
        !-------------------------------------------------
        ! Fichier ascendance thetavlasc.
        !-------------------------------------------------
        !
        clthetaasc=clarg(1:len_trim(clarg))//'.ms.tmp.thetaasc.dta'
        print*,trim(clthetaasc)
        open(88,file=clthetaasc,form='formatted')
        !
        !-------------------------------------------------
        ! Fichier ascendance thetaeasc.
        !-------------------------------------------------
        !
        if(llppthetaes .or. llppthetaes_bolton) then
          clthetaeasc=clarg(1:len_trim(clarg))//'.ms.tmp.thetaeasc.dta'
          print*,trim(clthetaeasc)
          open(89,file=clthetaeasc,form='formatted')
        endif
        !
        !-------------------------------------------------
        ! Fichier ascendance thetaesasc.
        !-------------------------------------------------
        !
        if(llppthetaes .or. llppthetaes_bolton) then
          clthetaesasc=clarg(1:len_trim(clarg))//'.ms.tmp.thetaesasc.dta'
          print*,trim(clthetaesasc)
          open(90,file=clthetaesasc,form='formatted')
        endif
        !
        !-------------------------------------------------
        ! Fichier ascendance hrasc.
        !-------------------------------------------------
        !
        clhrasc=clarg(1:len_trim(clarg))//'.ms.tmp.hrasc.dta'
        print*,trim(clhrasc)
        open(93,file=clhrasc,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichier thetad.
      !-------------------------------------------------
      !
      clthetad=clarg(1:len_trim(clarg))//'.ms.tmp.thetad.dta'
      print*,clthetad(1:len_trim(clthetad))
      open(24,file=clthetad,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetaw.
      !-------------------------------------------------
      !
      clthetaw=clarg(1:len_trim(clarg))//'.ms.tmp.thetaw.dta'
      print*,clthetaw(1:len_trim(clthetaw))
      open(87,file=clthetaw,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetae.
      !-------------------------------------------------
      !
      !clthetae=clarg(1:len_trim(clarg))//'.ms.tmp.thetae.dta'
      !print*,clthetae(1:len_trim(clthetae))
      !open(35,file=clthetae,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetae_bolton.
      !-------------------------------------------------
      !
      if(llppthetaes_bolton) then
        clthetae_bolton=clarg(1:len_trim(clarg))//'.ms.tmp.thetae_bolton.dta'
        print*,clthetae_bolton(1:len_trim(clthetae_bolton))
        open(25,file=clthetae_bolton,form='formatted')
      elseif(llppthetaes) then
        clthetae=clarg(1:len_trim(clarg))//'.ms.tmp.thetae.dta'
        print*,clthetae(1:len_trim(clthetae))
        open(25,file=clthetae,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichier hr.
      !-------------------------------------------------
      !
      clhr=clarg(1:len_trim(clarg))//'.ms.tmp.rel_hum.dta'
      print*,clhr(1:len_trim(clhr))
      open(37,file=clhr,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier qv.
      !-------------------------------------------------
      !
      clqv=clarg(1:len_trim(clarg))//'.ms.tmp.qv.dta'
      print*,clqv(1:len_trim(clqv))
      open(40,file=clqv,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier thetaes_bolton.
      !-------------------------------------------------
      !
      if(llppthetaes_bolton) then
        clthetaes_bolton=clarg(1:len_trim(clarg))//'.ms.tmp.thetaes_bolton.dta'
        print*,clthetaes_bolton(1:len_trim(clthetaes_bolton))
        open(26,file=clthetaes_bolton,form='formatted')
      elseif(llppthetaes) then
        clthetaes=clarg(1:len_trim(clarg))//'.ms.tmp.thetaes.dta'
        print*,clthetaes(1:len_trim(clthetaes))
        open(26,file=clthetaes,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichiers vent.
      !-------------------------------------------------
      !
      if(llvent) then
        clvent_ddff=clarg(1:len_trim(clarg))//'.ms.tmp.wind.ddff.dta'
        print*,clvent_ddff(1:len_trim(clvent_ddff))
        open(38,file=clvent_ddff,form='formatted')
        clvent_uv=clarg(1:len_trim(clarg))//'.ms.tmp.wind.uv.dta'
        print*,clvent_uv(1:len_trim(clvent_uv))
        open(39,file=clvent_uv,form='formatted')
      endif
      !
      !-------------------------------------------------
      ! Fichier "fond de carte" des theta'w.
      !-------------------------------------------------
      !
      cl_fdc_thetapw=clarg(1:len_trim(clarg))//'.ms.tmp.thetapw.dta'
      print*,cl_fdc_thetapw(1:len_trim(cl_fdc_thetapw))
      open(43,file=cl_fdc_thetapw,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier "fond de carte" des theta.
      !-------------------------------------------------
      !
      cl_fdc_theta=clarg(1:len_trim(clarg))//'.ms.tmp.fdc_theta.dta'
      print*,cl_fdc_theta(1:len_trim(cl_fdc_theta))
      open(44,file=cl_fdc_theta,form='formatted')
      !
      !-------------------------------------------------
      ! Fichier "fond de carte" des isothermes.
      !-------------------------------------------------
      !
      cl_fdc_isothermes=clarg(1:len_trim(clarg))//'.msa.tmp.isothermes.dta'
      print*,trim(cl_fdc_isothermes)
      open(45,file=cl_fdc_isothermes,form='formatted')
      !
      !-------------------------------------------------
      ! Calcul des températures potentielles.
      !-------------------------------------------------
      !
      !
      !-------------------------------------------------
      ! Variables de surface.
      !-------------------------------------------------
      !
      if(llts) then
        !
        !-------------------------------------------------
        ! Ts.
        !-------------------------------------------------
        !
        ztheta(klev+1)=fth_theta(zps,zts)
        ztheta_loc=fth_tphig(zps,zts,clmethode)
        if(ztheta_loc < zxmax .and. zcooy(klev+1) <= zymax) write(22,fmt='(9g16.7)') ztheta_loc,zcooy(klev+1)
        !
        !-------------------------------------------------
        ! qs.
        !-------------------------------------------------
        !
        !
        !-------------------------------------------------
        ! thetav.
        !-------------------------------------------------
        !
        zthetav(klev+1)=fth_thetav(zps,zts,zqs)
        if(ztheta_loc < zxmax .and. zcooy(klev+1) <= zymax) then
          write(23,fmt='(9g16.7)') fth_tphig(zps,fth_t(zps,zthetav(klev+1)),clmethode),zcooy(klev+1)
        endif
        !
        if(llppthetaes_bolton) then
          !-------------------------------------------------
          ! thetae_bolton.
          !-------------------------------------------------
          !
          zthetae_bolton=fth_thetae_bolton(zps,zts,zqs)
          if(zthetae_bolton < zxmax .and. zcooy(klev+1) <= zymax) write(25,fmt='(9g16.7)') fth_tphig(zps,fth_t(zps,zthetae_bolton),clmethode),zcooy(klev+1)
          !
          !-------------------------------------------------
          ! thetaes_bolton.
          !-------------------------------------------------
          !
          zthetaes_bolton=fth_thetaes_bolton(zps,zts)
          !
          !-------------------------------------------------
          ! On veut que la limite droite du graphique aille 30°
          ! à droite de thetaes en surface.
          !-------------------------------------------------
          !
          if(zthetaes_bolton < zxmax .and. zcooy(klev+1) <= zymax) then
            write(26,fmt='(9g16.7)') fth_tphig(zps,fth_t(zps,zthetaes_bolton),clmethode),zcooy(klev+1)
          else
            write(26,fmt='(9g16.7)') zsaut,zsaut
          endif
        elseif(llppthetaes) then
          !-------------------------------------------------
          ! thetae.
          !-------------------------------------------------
          !
          zthetae=fth_thetae(zps,zts,zqs)
          if(zthetae < zxmax .and. zcooy(klev+1) <= zymax) write(25,fmt='(9g16.7)') fth_tphig(zps,fth_t(zps,zthetae),clmethode),zcooy(klev+1)
          !
          !-------------------------------------------------
          ! thetaes.
          !-------------------------------------------------
          !
          zthetaes=fth_thetaes(zps,zts)
          !
          !-------------------------------------------------
          ! On veut que la limite droite du graphique aille 30°
          ! à droite de thetaes en surface.
          !-------------------------------------------------
          !
          if(zthetaes < zxmax .and. zcooy(klev+1) <= zymax) then
            write(26,fmt='(9g16.7)') fth_tphig(zps,fth_t(zps,zthetaes),clmethode),zcooy(klev+1)
          else
            write(26,fmt='(9g16.7)') zsaut,zsaut
          endif
        endif
      endif
      !
      !-------------------------------------------------
      ! Variables atmosphériques.
      !-------------------------------------------------
      !
      do jlev=klev,1,-1
        !
        !-------------------------------------------------
        ! On ne calcule qu'en dessous de 200 hPa.
        !-------------------------------------------------
        !
        !if(zp(jlev) < 20000.) cycle
        !
        !-------------------------------------------------
        ! Theta.
        !-------------------------------------------------
        !
        ztheta(jlev)=fth_theta(zp(jlev),zt(jlev))
        ztheta_loc=fth_tphig(zp(jlev),zt(jlev),clmethode)
        !
        !-------------------------------------------------
        ! On filtre les valeurs d'X ou Y trop marginales.
        !-------------------------------------------------
        !
        llecr=ztheta_loc < zxmax 
        if(llecr .and. zcooy(jlev) <= zymax) write(22,fmt='(9g16.7)') ztheta_loc,zcooy(jlev)

        if(llud) then
          !
          !-------------------------------------------------
          ! Theta_ud.
          !-------------------------------------------------
          !
          ztheta_ud(jlev)=fth_theta(zp(jlev),ztu(jlev))
          ztheta_loc=fth_tphig(zp(jlev),ztu(jlev),clmethode)
          !
          !-------------------------------------------------
          ! On filtre les valeurs d'X ou Y trop marginales.
          !-------------------------------------------------
          !
          llecr=ztheta_loc < zxmax 
          if(llecr .and. zcooy(jlev) <= zymax) write(91,fmt='(9g16.7)') ztheta_loc,zcooy(jlev)
        endif
        !
        !-------------------------------------------------
        ! Sortie des températures déduites des énergies statiques,
        ! i.e. s/cp, h/cp, hs/cp.
        !-------------------------------------------------
        !
        if(lcpt) then
          zeness=rcpd*zt(jlev)+rg*zcooy_heig(jlev) ! énergie statique sèche.
          zenesh=zeness+2.5e6*zqv(jlev) ! énergie statique humide.
          zeneshs=zeness+2.5e6*fth_qs(zt(jlev),zp(jlev)) ! énergie statique humide saturante.
          zeneshsgccsv1=zeness+2.5e6*(zqv(jlev)-fth_qs(zt(jlev),zp(jlev))) ! critère CVPP ARPEGE si GCCSV=1..
          if(llecr .and. zcooy(jlev) <= zymax) write(78,'(9g16.7)') zeness/rcpd-rtt,zcooy(jlev)
          if(llecr .and. zcooy(jlev) <= zymax) write(79,'(9g16.7)') zenesh/rcpd-rtt,zcooy(jlev)
          if(llecr .and. zcooy(jlev) <= zymax) write(80,'(9g16.7)') zeneshs/rcpd-rtt,zcooy(jlev)
          if(llecr .and. zcooy(jlev) <= zymax) write(81,'(9g16.7)') zeneshsgccsv1/rcpd-rtt,zcooy(jlev)
        endif
        !
        !-------------------------------------------------
        ! thetav.
        !-------------------------------------------------
        !
        zthetav(jlev)=fth_thetav(zp(jlev),zt(jlev),zqv(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) then
          write(23,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetav(jlev)),clmethode),zcooy(jlev)
        endif
        !
        !-------------------------------------------------
        ! thetal.
        !-------------------------------------------------
        !
        zthetal=fth_thetal(zp(jlev),zt(jlev),zqv(jlev),zqc(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) write(41,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetal),clmethode),zcooy(jlev)
        !
        !-------------------------------------------------
        ! thetavl.
        !-------------------------------------------------
        !
        zthetavl(jlev)=fth_thetavl(zp(jlev),zt(jlev),zqv(jlev),zqc(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) write(42,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetavl(jlev)),clmethode),zcooy(jlev)
        if(llasc) then
          !
          !-------------------------------------------------
          ! thetaasc.
          !-------------------------------------------------
          !
          ztloc=fth_theta(zp(jlev),zasc_tt_ascendance(jlev))
          if(llecr .and. zcooy(jlev) <= zymax) write(88,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),ztloc),clmethode),zcooy(jlev)
          !
          !-------------------------------------------------
          ! hrasc.
          !-------------------------------------------------
          !
          zhrasc=fth_hr(zp(jlev),zasc_tt_ascendance(jlev),zasc_qv_ascendance(jlev))
          if(llecr .and. zcooy(jlev) <= zymax) write(93,fmt='(9g16.7)') zhrasc,zcooy(jlev)
          !
          !-------------------------------------------------
          ! thetaeasc.
          !-------------------------------------------------
          !
          if(llppthetae_bolton) then
            ztloc=fth_thetae_bolton(zp(jlev),zasc_tt_ascendance(jlev),zasc_qv_ascendance(jlev))
          else
            ztloc=fth_thetae(zp(jlev),zasc_tt_ascendance(jlev),zasc_qv_ascendance(jlev))
          endif
          if(llecr .and. zcooy(jlev) <= zymax) write(89,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),ztloc),clmethode),zcooy(jlev)
          if(llppthetaes .or. llppthetaes_bolton) then
            !
            !-------------------------------------------------
            ! thetaesasc.
            !-------------------------------------------------
            !
            if(llppthetae_bolton) then
              ztloc=fth_thetaes_bolton(zp(jlev),zasc_tt_ascendance(jlev))
            else
              ztloc=fth_thetaes(zp(jlev),zasc_tt_ascendance(jlev))
            endif
            if(llecr .and. zcooy(jlev) <= zymax) write(90,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),ztloc),clmethode),zcooy(jlev)
          endif
        endif
        !
        !-------------------------------------------------
        ! thetad.
        !-------------------------------------------------
        !
        zthetad=fth_thetad(zp(jlev),zqv(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) write(24,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetad),clmethode),zcooy(jlev)
        !
        !-------------------------------------------------
        ! thetaw.
        !-------------------------------------------------
        !
        ztw=fth_tw(zp(jlev),zt(jlev),zqv(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) write(87,fmt='(9g16.7)') fth_tphig(zp(jlev),ztw,clmethode),zcooy(jlev)

        if(llud) then
          !
          !-------------------------------------------------
          ! thetaw_ud.
          !-------------------------------------------------
          !
          ztw_ud=fth_tw(zp(jlev),ztu(jlev),zqu(jlev))
          if(llecr .and. zcooy(jlev) <= zymax) write(92,fmt='(9g16.7)') fth_tphig(zp(jlev),ztw_ud,clmethode),zcooy(jlev)
        endif
        !
        !-------------------------------------------------
        ! thetae.
        !-------------------------------------------------
        !
        !zthetae=thetae(zp(jlev),zt(jlev),zqv(jlev))
        !if(zthetae < zxmax .and. llecr .and. zcooy(jlev) <= zymax) write(35,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetae),clmethode),zcooy(jlev)
        !
        !-------------------------------------------------
        ! thetae_bolton.
        !-------------------------------------------------
        !
        if(llppthetaes .or. llppthetaes_bolton) then
          if(llppthetaes_bolton) then
            zthetae=fth_thetae_bolton(zp(jlev),zt(jlev),zqv(jlev))
          else
            zthetae=fth_thetae(zp(jlev),zt(jlev),zqv(jlev))
          endif
          if(zthetae < zxmax .and. llecr .and. zcooy(jlev) <= zymax) write(25,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetae),clmethode),zcooy(jlev)
        endif
        !
        !-------------------------------------------------
        ! hr.
        !-------------------------------------------------
        !
        zhr=fth_hr(zp(jlev),zt(jlev),zqv(jlev))
        if(llecr .and. zcooy(jlev) <= zymax) write(37,fmt='(9g16.7)') zhr,zcooy(jlev)
        !
        !-------------------------------------------------
        ! qv.
        !-------------------------------------------------
        !
        if(llecr .and. zcooy(jlev) <= zymax) write(40,fmt='(9g16.7)') zqv(jlev),zcooy(jlev)
        if(llvent) then
          !
          !-------------------------------------------------
          ! Vent.
          !-------------------------------------------------
          !
          call recpol(zu(jlev),zv(jlev),zff,zdd)
          zdd=modulo(180.+90.+180.-zdd/zcondr,360.)-180. ! azimuth dont vient le vent en degrés.
          if(llecr .and. zcooy(jlev) <= zymax) write(38,fmt='(9g16.7)') 0.,zcooy(jlev),zdd,zff
          if(llecr .and. zcooy(jlev) <= zymax) write(39,fmt='(9g16.7)') 0.,zcooy(jlev),zu(jlev),zv(jlev)
        endif
        if(llppthetaes .or. llppthetaes_bolton) then
          !
          !-------------------------------------------------
          ! thetaes_bolton.
          !-------------------------------------------------
          !
          if(llppthetaes_bolton) then
            zthetaes=fth_thetaes_bolton(zp(jlev),zt(jlev))
          else
            zthetaes=fth_thetaes(zp(jlev),zt(jlev))
          endif
          !
          !-------------------------------------------------
          ! On veut que la limite droite du graphique aille 30°
          ! à droite de thetaes en surface, sans dépasser zxmax.
          !-------------------------------------------------
          !
          if(zthetaes < zxmax .and. llecr .and. zcooy(jlev) <= zymax) then
            write(26,fmt='(9g16.7)') fth_tphig(zp(jlev),fth_t(zp(jlev),zthetaes),clmethode),zcooy(jlev)
          else
            write(26,fmt='(9g16.7)') zsaut,zsaut
          endif
        endif
      enddo
      close(22)
      !
      !-------------------------------------------------
      ! Création du fichier documentant les champs produits.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré open doc'
      cldoc=clarg(1:len_trim(clarg))//'.ms.tmp.theta.doc'
      cldocuv=clarg(1:len_trim(clarg))//'.ms.tmp.wind.doc'
      open(18,file=cldoc,form='formatted')
      open(19,file=cldocuv,form='formatted')
      write(18,fmt='(a)') '#FORMAT=XVI'
      write(19,fmt='(a)') '#FORMAT=XYUVI'
      !
      !-------------------------------------------------
      ! Titre de la figure.
      !-------------------------------------------------
      !
      call lfacas(iule,'INDICE EXPERIENCE',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecc(iule,'INDICE EXPERIENCE',1,clexp,ilong,ierr)
      else
        clexp=clarg(1:len_trim(clarg))
      endif
      write(18,fmt='(3a)') '#TITRE=',clexp(1:len_trim(clexp)),': potential temperature'
      write(19,fmt='(3a)') '#TITRE=',clexp(1:len_trim(clexp)),': horizontal wind'
      !
      !-------------------------------------------------
      ! Date en clair.
      !-------------------------------------------------
      !
      if(lldebug) write(*,fmt=*) 'Debug pré date_en_clair'
      call date_en_clair(iule,cldatc)
      write(18,fmt='(9a)') '#DATE=',cldatc(1:len_trim(cldatc))
      write(19,fmt='(9a)') '#DATE=',cldatc(1:len_trim(cldatc))
      write(18,fmt='(a)') '#UNITE='
      write(19,fmt='(a)') '#UNITE=m/s'
      !
      !-------------------------------------------------
      ! Altitude en m.
      !-------------------------------------------------
      !
      call cooz(iule,klev,zphi,zphif)
      do jlev=1,klev
        zcooy_heig_m(jlev)=zphif(jlev)/rg
      enddo
      !
      !-------------------------------------------------
      ! HCLA/2: calcul de la HCLA.
      !-------------------------------------------------
      !
      if(zthetav(klev+1) == zindef) then
        !
        !-------------------------------------------------
        ! Theta(surface) est indisponible.
        ! On lance le calcul sur klev niveaux.
        !-------------------------------------------------
        !
        zhcla=fth_hcla(klev,zthetav,zcooy_heig_m)
      else
        !
        !-------------------------------------------------
        ! Theta(surface) est disponible.
        ! On lance le calcul sur (klev+1) niveaux.
        !-------------------------------------------------
        !
        ilev=klev+1
        zhcla=fth_hcla(ilev,zthetav,zcooy_heig_m)
      endif
      if(zorog /= zindef) then
        write(*,fmt='(a,f6.0,a,f6.0,a)') '  PBL depth=',zhcla,' m, alt. top of PBL=',zhcla+zorog/rg,' m'
      else
        write(*,fmt='(a,f6.0,a,f6.0,a)') '  PBL depth=',zhcla,' m'
      endif
      !
      !-------------------------------------------------
      ! "Fond de carte" des theta'w.
      !-------------------------------------------------
      !
      call fdc_thetapw(zsaut,klev,zp,zt,zxmax,zymax,zcooy,clmethode)
      !
      !-------------------------------------------------
      ! "Fond de carte" des theta.
      !-------------------------------------------------
      !
      call fdc_theta(zsaut,klev,zp,zt,zxmax,zymax,zcooy,clmethode)
      !
      !-------------------------------------------------
      ! "Fond de carte" des isothermes.
      !-------------------------------------------------
      !
      call fdc_isothermes(zsaut,klev,zp,zt,zxmax,zymax,zcooy,clmethode)
      !
      !-------------------------------------------------
      ! Hauteur de CLA.
      !-------------------------------------------------
      !
      if(zorog /= zindef) then
        write(clhcla_dint1,fmt='(a,i5,a)') 'Alt. top of PBL  = ',nint(zhcla+zorog/rg),' m'
      endif
      write(clhcla_dint2,fmt='(a,i5,a)') 'PBL depth = ',nint(zhcla),' m'
      !
      !-------------------------------------------------
      ! Localisation en clair, i.e. avec le nom du RS le plus proche.
      !-------------------------------------------------
      !
      call localisation_en_clair(iule,clorig)
      write(18,fmt='(2a)') '#ORIGINE=',clorig(1:len_trim(clorig))
      write(19,fmt='(2a)') '#ORIGINE=',clorig(1:len_trim(clorig))
      !
      !-------------------------------------------------
      ! Tracé éventuel d'un réticule.
      !-------------------------------------------------
      !
      if(trim(clmethode) == 'PAH' .or. trim(clmethode) == 'E761') then
        write(18,fmt='(a)') '#RETICULE=non'
      endif
      write(18,fmt='(a)') '#LEGENDE_X=T (°C)'
      write(19,fmt='(a)') '#LEGENDE_X='
      write(18,fmt='(9a)') '#LEGENDE_Y=',cly(1:len_trim(cly)),' (',clunite_y(1:len_trim(clunite_y)),')'
      write(19,fmt='(9a)') '#LEGENDE_Y=',cly(1:len_trim(cly)),' (',clunite_y(1:len_trim(clunite_y)),')'
      write(18,fmt='(a,g16.7)') '#Y_MAX_LIMIT=',zymax
      write(19,fmt='(a,g16.7)') '#Y_MAX_LIMIT=',zymax
      if(trim(clmethode) == 'AS') then
        write(18,fmt='(a,g16.7)') '#X_MIN_LIMIT=',17.
        write(19,fmt='(a,g16.7)') '#X_MIN_LIMIT=',17.
        write(18,fmt='(a,g16.7)') '#X_MAX_LIMIT=',65.
        write(19,fmt='(a,g16.7)') '#X_MAX_LIMIT=',65.
      else
        write(18,fmt='(a,g16.7)') '#X_MIN_LIMIT=',-20.
        write(19,fmt='(a,g16.7)') '#X_MIN_LIMIT=',-20.
        write(18,fmt='(a,g16.7)') '#X_MAX_LIMIT=',40.
        write(19,fmt='(a,g16.7)') '#X_MAX_LIMIT=',40.
      endif
      !
      !-------------------------------------------------
      ! Vent.
      !-------------------------------------------------
      !
      if(llvent) then
        write(19,fmt='(a)') ' '
        write(19,fmt='(a)') '#CHAMP=Horizontal wind'
        write(19,fmt='(2a)') '#FICHIER=',clvent_uv(1:len_trim(clvent_uv))
      endif
      !
      !-------------------------------------------------
      ! Theta.
      !-------------------------------------------------
      !
      if(llpptheta) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta'
        write(18,fmt='(2a)') '#FICHIER=',cltheta(1:len_trim(cltheta))
        write(18,fmt='(2a)') '#COUL=black'
        write(18,fmt='(2a)') '#POIN=0'
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta'
        write(18,fmt='(2a)') '!#FICHIER=',cltheta(1:len_trim(cltheta))
      endif
      if(llppthetaw) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta W'
        write(18,fmt='(2a)') '#FICHIER=',clthetaw(1:len_trim(clthetaw))
        write(18,fmt='(2a)') '#COUL=blue'
        write(18,fmt='(2a)') '#POIN=0'
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta W'
        write(18,fmt='(2a)') '!#FICHIER=',clthetaw(1:len_trim(clthetaw))
      endif
      if(llppthetad) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta D'
        write(18,fmt='(2a)') '#FICHIER=',clthetad(1:len_trim(clthetad))
        write(18,fmt='(2a)') '#POIN=0.7'
        write(18,fmt='(2a)') '#COUL=brown'
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta D'
        write(18,fmt='(2a)') '!#FICHIER=',clthetad(1:len_trim(clthetad))
        write(18,fmt='(2a)') '!#POIN=0.7'
        write(18,fmt='(2a)') '!#COUL=brown'
      endif
      if(llud) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta UD'
        write(18,fmt='(2a)') '#FICHIER=',trim(cltheta_ud)
        write(18,fmt='(2a)') '#COUL=black'
        write(18,fmt='(2a)') '#POIN=2,4'
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta W UD'
        write(18,fmt='(2a)') '#FICHIER=',trim(clthetaw_ud)
        write(18,fmt='(2a)') '#COUL=blue'
        write(18,fmt='(2a)') '#POIN=2,4'
      endif
      if(llppthetal) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta L'
        write(18,fmt='(2a)') '#FICHIER=',clthetal(1:len_trim(clthetal))
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta L'
        write(18,fmt='(2a)') '!#FICHIER=',clthetal(1:len_trim(clthetal))
      endif
      if(llppthetav) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta V'
        write(18,fmt='(2a)') '#FICHIER=',clthetav(1:len_trim(clthetav))
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta V'
        write(18,fmt='(2a)') '!#FICHIER=',clthetav(1:len_trim(clthetav))
      endif
      if(llppthetavl) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta VL'
        write(18,fmt='(2a)') '#FICHIER=',clthetavl(1:len_trim(clthetavl))
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Theta VL'
        write(18,fmt='(2a)') '!#FICHIER=',clthetavl(1:len_trim(clthetavl))
      endif
      if(llppthetae_top) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta E top'
        write(18,fmt='(2a)') '#FICHIER=',clthetae_top(1:len_trim(clthetae_top))
      endif
      if(llppthetae) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta E'
        write(18,fmt='(2a)') '#FICHIER=',clthetae(1:len_trim(clthetae))
      endif
      if(llppthetaes) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta ES'
        write(18,fmt='(2a)') '#FICHIER=',clthetaes(1:len_trim(clthetaes))
      endif
      if(llppthetae_bolton) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta E'
        write(18,fmt='(2a)') '#FICHIER=',clthetae_bolton(1:len_trim(clthetae_bolton))
      endif
      if(llppthetaes_bolton) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta ES'
        write(18,fmt='(2a)') '#FICHIER=',clthetaes_bolton(1:len_trim(clthetaes_bolton))
        write(18,fmt='(2a)') '#COUL=blue'
        write(18,fmt='(2a)') '#POIN=0'
      endif
      if(llpp_fdc_thetapw) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Iso-Theta''W'
        write(18,fmt='(2a)') '#FICHIER=',cl_fdc_thetapw(1:len_trim(cl_fdc_thetapw))
        write(18,fmt='(2a)') '#COUL=green'
        write(18,fmt='(2a)') '#LARG=0.3'
        write(18,fmt='(2a)') '#POIN=8,2'
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Iso-Theta''W'
        write(18,fmt='(2a)') '!#FICHIER=',cl_fdc_thetapw(1:len_trim(cl_fdc_thetapw))
      endif
      if(llpp_fdc_theta) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Iso-Theta'
        write(18,fmt='(2a)') '#FICHIER=',cl_fdc_theta(1:len_trim(cl_fdc_theta))
        write(18,fmt='(2a)') '#COUL=darkorange'
        write(18,fmt='(2a)') '#POIN=0'
        write(18,fmt='(2a)') '#LARG=0.4'
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Iso-T'
        write(18,fmt='(2a)') '#FICHIER=',trim(cl_fdc_isothermes)
        write(18,fmt='(2a)') '#COUL=grey'
        write(18,fmt='(2a)') '#POIN=0'
        write(18,fmt='(2a)') '#LARG=0.15'
      else
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '!#CHAMP=Iso-Theta'
        write(18,fmt='(2a)') '!#FICHIER=',cl_fdc_theta(1:len_trim(cl_fdc_theta))
      endif
      if(llnuage_conv) then
        if(llppnuage_conv) then
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '#CHAMP=Convective cloud'
          write(18,fmt='(2a)') '#FICHIER=',clnuage_conv(1:len_trim(clnuage_conv))
        else
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '!#CHAMP=Convective cloud'
          write(18,fmt='(2a)') '!#FICHIER=',clnuage_conv(1:len_trim(clnuage_conv))
        endif
      endif
      if(llnuage_satu) then
        if(llppnuage_satu) then
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '#CHAMP=Near saturation cloud'
          write(18,fmt='(2a)') '#FICHIER=',clnuage_satu(1:len_trim(clnuage_satu))
        else
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '!#CHAMP=Near saturation cloud'
          write(18,fmt='(2a)') '!#FICHIER=',clnuage_satu(1:len_trim(clnuage_satu))
        endif
      endif
      if(llasc) then
        write(18,fmt='(a)') ' '
        write(18,fmt='(a)') '#CHAMP=Theta ASC'
        write(18,fmt='(2a)') '#FICHIER=',trim(clthetaasc)
        if(llppthetaes .or. llppthetaes_bolton) then
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '#CHAMP=Theta E ASC'
          write(18,fmt='(2a)') '#FICHIER=',trim(clthetaeasc)
          write(18,fmt='(a)') ' '
          write(18,fmt='(a)') '#CHAMP=Theta ES ASC'
          write(18,fmt='(2a)') '#FICHIER=',trim(clthetaesasc)
          write(18,fmt='(2a)') '#COUL=blue'
          write(18,fmt='(2a)') '#POIN=0'
        endif
      endif
      !write(18,fmt='(a)') '#IMAGE=3000 2000'
      close(18)
      print*,'---------------------------------------'
      print*,'Wind:'
      print*,'  ',cldocuv(1:len_trim(cldocuv))
      print*,'Theta:'
      print*,'  ',cldoc(1:len_trim(cldoc))
      !
      ! -------------------------------------------------
      ! Fermeture du fichier.
      ! -------------------------------------------------
      !
      call lfafer(iule)
    endif
  enddo
endif
print*,'---------------------------------------'
end
subroutine fdc_thetapw(psaut,klev,pp,pt,pxmax,pymax,pcooy,cdmethode)
! --------------------------------------------------------------
! Fond de carte en theta'w.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use constantes
implicit character*200 (c)
implicit logical (l)
real pp(klev)
real pt(klev)
real pcooy(klev+1)
character*(*) cdmethode
!
!-------------------------------------------------
! On va tracer (2*inthetap+1) ascendances adiabatiques humides
! allant du niveau bas du modèle et montant vers le sommet pymax.
!-------------------------------------------------
!
zdelta=30. ! on trace les theta'w de Tbase-zdelta à Tbase+zdelta.
zecart=2. ! on trace les theta'w toutes les zecart Kelvin.
ztbase=zecart*real(nint((fth_theta(pp(klev),pt(klev)))/zecart))
inthetap=nint(2.*zdelta/zecart)
do jasc=-inthetap,inthetap
  !
  !-------------------------------------------------
  ! Température de départ.
  !-------------------------------------------------
  !
  zt=ztbase+real(jasc)*zecart
  do jlev=klev,1,-1
    !
    !-------------------------------------------------
    ! Ascendance adiabatique du bas au niveau courant.
    !-------------------------------------------------
    !
    if(pp(jlev) > 10000.) then
      zt_final=fth_t_evol_ad_hum(zt,ratm,pp(jlev))
      ztheta_loc=fth_theta(pp(jlev),zt_final)
      if(pcooy(jlev+1) <= pymax) write(43,fmt='(9g16.7)') fth_tphig(pp(jlev),fth_t(pp(jlev),ztheta_loc),cdmethode),pcooy(jlev)
    endif
  enddo
  write(43,fmt='(9g16.7)') psaut,psaut
enddo
end
subroutine fdc_theta(psaut,klev,pp,pt,pxmax,pymax,pcooy,cdmethode)
! --------------------------------------------------------------
! Fond de carte en theta.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use constantes
implicit character*200 (c)
implicit logical (l)
real pp(klev)
real pt(klev)
real pcooy(klev+1)
character*(*) cdmethode
!
!-------------------------------------------------
! On va tracer (2*inthetap+1) ascendances adiabatiques
! allant du niveau bas du modèle et montant vers le sommet pymax.
!-------------------------------------------------
!
zdelta=80. ! on trace les theta de Tbase-zdelta à Tbase+zdelta.
zecart=5. ! on trace les theta toutes les zecart Kelvin.
zqv=1.e-4
ztbase=zecart*real(nint((fth_theta(pp(klev),pt(klev)))/zecart))
inthetap=nint(2.*zdelta/zecart)
do jasc=-inthetap,inthetap
  !
  !-------------------------------------------------
  ! Température de départ.
  !-------------------------------------------------
  !
  zt=ztbase+real(jasc)*zecart
  do jlev=klev,1,-1
    !
    !-------------------------------------------------
    ! Ascendance adiabatique du bas au niveau courant.
    !-------------------------------------------------
    !
    if(pp(jlev) > 10000.) then
      zt_final=fth_t_evol_ad_seche(zt,zqv,ratm,pp(jlev))
      ztheta_loc=fth_theta(pp(jlev),zt_final)
      if(pcooy(jlev+1) <= pymax) write(44,fmt='(9g16.7)') fth_tphig(pp(jlev),fth_t(pp(jlev),ztheta_loc),cdmethode),pcooy(jlev)
    endif
  enddo
  write(44,fmt='(9g16.7)') psaut,psaut
enddo
end
subroutine coord_vert_pcond(ppcond,klev,pp,pcooy,pcooypc)
! --------------------------------------------------------------
! Interpolation entre deux niveaux de la coordonnée verticale.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  ppcond: pression à laquelle il faut interpoler.
!  klev: nombre de niveaux.
!  pp: pression de chaque niveau.
!  pcooy: cordonnée verticale de chaque niveau (elle peut être p, z, niveaux, ...).
! En sortie:
!  pcooypc: interpolation de pcooy au niveau ppcond.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
real pp(klev) 
real pcooy(klev) 
!
!-------------------------------------------------
! Boucle sur les niveaux d'entrée.
!-------------------------------------------------
!
do jlev=1,klev-1
  if(ppcond >= pp(jlev) .and. ppcond < pp(jlev+1)) then
    !
    !-------------------------------------------------
    ! ppcond est entre les niveaux jlev et (jlev+1).
    ! Interpolation.
    !-------------------------------------------------
    !
    pcooypc=pcooy(jlev)+(pcooy(jlev+1)-pcooy(jlev))*(ppcond-pp(jlev))/(pp(jlev+1)-pp(jlev))
    return
  endif
enddo
!
!-------------------------------------------------
! Si on est arrivé ici c'est que ppcond est hors domaine!...
!-------------------------------------------------
!
pcooypc=pp(1)
!write(*,fmt=*) 'coord_vert_pcond/WARNING: pressure cannot be interpolated at the level p=',ppcond,' Pa!...'
!write(*,fmt=*) 'Taken arbitrarily to the value ',pcooypc,' Pa'
end
subroutine fdc_isothermes(psaut,klev,pp,pt,pxmax,pymax,pcooy,cdmethode)
! --------------------------------------------------------------
! Fond de carte en isothermes.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
use constantes
implicit character*200 (c)
implicit logical (l)
real pp(klev)
real pt(klev)
real pcooy(klev+1)
character*(*) cdmethode
!
!-------------------------------------------------
! On va tracer (2*inthetap+1) isothermes
! allant du niveau bas du modèle et montant vers le sommet pymax.
!-------------------------------------------------
!
zdelta=100. ! on trace de Tbase-zdelta à Tbase+zdelta.
zecart=10. ! on trace tous les zecart Kelvin.
ztbase=zecart*real(nint((pt(klev)-rtt)/zecart))+rtt
inthetap=nint(2.*zdelta/zecart)
do jasc=-inthetap,inthetap
  !
  !-------------------------------------------------
  ! Température de départ.
  !-------------------------------------------------
  !
  zt=ztbase+real(jasc)*zecart
  write(45,fmt='(9g16.7)') psaut,psaut
  do jlev=klev,1,-1
    !
    !-------------------------------------------------
    ! Ascendance adiabatique du bas au niveau courant.
    !-------------------------------------------------
    !
    if(pcooy(jlev) <= pymax) write(45,fmt='(9g16.7)') fth_tphig(pp(jlev),zt,cdmethode),pcooy(jlev)
  enddo
  write(45,fmt='(9g16.7)') psaut,psaut
enddo
end
#include"dates.F90"
#include"coord_vert.F90"
#include"mautodoc.F90"
