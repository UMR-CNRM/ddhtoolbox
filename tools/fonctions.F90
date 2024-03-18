module constantes
! --------------------------------------------------------------
! //// Constantes physiques.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Auteur/author:   2006-01, J.M. Piriou d'après ARPEGE.
! Modifications:
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
save
!
!-------------------------------------------------
! Nombre d'itérations de la boucle de Newton.
!-------------------------------------------------
!
integer(kind=4), parameter :: nbiter=2
!      -----------------------------------------------------------------
!
!*       1.    DEFINE FUNDAMENTAL CONSTANTS.
!              -----------------------------
!
real(kind=8), parameter :: rpi=3.14159265358979 ! pi.
real(kind=8), parameter :: rclum=299792458. ! célérité de la lumière.
real(kind=8), parameter :: rhpla=6.6260755e-34 ! cte de Planck.
real(kind=8), parameter :: rkbol=1.380658e-23 ! cte de Bolzman.
real(kind=8), parameter :: rnavo=6.0221367e+23 ! nombre d'Avogadro.
!
!     ------------------------------------------------------------------
!
!*       2.    DEFINE ASTRONOMICAL CONSTANTS.
!              ------------------------------
!
real(kind=8), parameter :: rday=86400. ! jour solaire.
real(kind=8), parameter :: rea=149597870000. ! demi-grand axe de rév. terrestre.
real(kind=8), parameter :: rsiyea=365.25*rday*2.*rpi/6.283076 ! année sidérale.
real(kind=8), parameter :: rsiday=rday/(1.0_8+rday/rsiyea) ! jour sidéral.
real(kind=8), parameter :: romega=2.*rpi/rsiday ! vitesse angulaire terrestre.
!
! ------------------------------------------------------------------
!
! *       3.    DEFINE GEOIDE.
! --------------
!
real(kind=8), parameter :: rg=9.80665 ! accélération de la pesanteur.
real(kind=8), parameter :: ra=6371229. ! rayon terrestre.
!
! ------------------------------------------------------------------
!
! *       4.    DEFINE RADIATION CONSTANTS.
! ---------------------------
!
!REAL(KIND=8), parameter :: rsigma=2. * rpi**5 * rkbol**4 /(15.* rclum**2 * rhpla**3) ! cte de Stefan-Bolzman.
real(kind=8), parameter :: rsigma=5.670509e-08
real(kind=8), parameter :: ri0=1370. ! cte solaire.
!
! ------------------------------------------------------------------
!
! *       5.    DEFINE THERMODYNAMIC CONSTANTS, GAS PHASE.
! ------------------------------------------
!
real(kind=8), parameter :: r=rnavo*rkbol ! cte des gaz parfaits.
real(kind=8), parameter :: rmd=28.9644
real(kind=8), parameter :: rmv=18.0153
real(kind=8), parameter :: rmo3=47.9942
real(kind=8), parameter :: rd=1000.*r/rmd ! cte spécifique de l'air sec.
real(kind=8), parameter :: rv=1000.*r/rmv ! cte spécifique de la vapeur d'eau.
real(kind=8), parameter :: rcpd=3.5*rd ! chaleur massique de l'air sec.
real(kind=8), parameter :: rcvd=rcpd-rd
real(kind=8), parameter :: rcpv=4. *rv ! chaleur massique de la vapeur d'eau.
real(kind=8), parameter :: rcvv=rcpv-rv
real(kind=8), parameter :: rkappa=rd/rcpd
real(kind=8), parameter :: retv=rv/rd-1.0_8
!
real(kind=8), parameter :: ralpw =  .6022274788e+02
real(kind=8), parameter :: rbetw =  .6822400210e+04
real(kind=8), parameter :: rgamw =  .5139266694e+01
!
real(kind=8), parameter :: ralps =  .3262117981e+02
real(kind=8), parameter :: rbets =  .6295421339e+04
real(kind=8), parameter :: rgams =  .5631331575e+00
!
real(kind=8), parameter :: ralpd = -.2760156808e+02
real(kind=8), parameter :: rbetd = -.5269788712e+03
real(kind=8), parameter :: rgamd = -.4576133537e+01
! ------------------------------------------------------------------
!
! *       6.    DEFINE THERMODYNAMIC CONSTANTS, LIQUID PHASE.
! ---------------------------------------------
!
real(kind=8), parameter :: rcw=4218. ! chaleur massique de l'eau liquide.
!
! ------------------------------------------------------------------
!
! *       7.    DEFINE THERMODYNAMIC CONSTANTS, SOLID PHASE.
! --------------------------------------------
!
real(kind=8), parameter :: rcs=2106. ! chaleur massique de la glace.
!
! ------------------------------------------------------------------
!
! *       8.    DEFINE THERMODYNAMIC CONSTANTS, TRANSITION OF PHASE.
! ----------------------------------------------------
!
real(kind=8), parameter :: rtt=273.16 ! point triple de l'eau.
real(kind=8), parameter :: rdt=11.82
real(kind=8), parameter :: rlvtt=2.5008e+6 ! chaleur latente eau vapeur > eau liquide.
real(kind=8), parameter :: rlstt=2.8345e+6 ! chaleur latente eau vapeur > eau glace.
real(kind=8), parameter :: rlvzer=rlvtt+rtt*(rcw-rcpv) ! chaleur latente de fusion à 0°K!
real(kind=8), parameter :: rlszer=rlstt+rtt*(rcs-rcpv) ! chaleur latente de sublimation à 0°K!
real(kind=8), parameter :: rlmlt=rlstt-rlvtt ! chaleur latente eau liquide > eau glace.
real(kind=8), parameter :: ratm= 100000. ! pression standard non modifiable.
real(kind=8)            :: rpref=100000. ! pression standard modifiable par l'utilisateur, dont la valeur par défaut est celle-ci.
!
! ------------------------------------------------------------------
!
! *       9.    SATURATED VAPOUR PRESSURE.
! --------------------------
!
real(kind=8), parameter :: restt=611.14
!
!-------------------------------------------------
! Constante de Joule.
!-------------------------------------------------
!
real(kind=8), parameter :: rjoule=4.184
!
end
subroutine fth_cin_cape_old(ldverbose,klev,pt,pqv,pp,pretained,pentr,pcin,pcape,klc,klfc &
  & ,klnb,plc,plfc,plnb,kcloud,pmc,pthetae_top,pprec)
! --------------------------------------------------------------
! //// *fth_cin_cape_old* Compute CIN, CAPE, and vertical location of clouds.
! --------------------------------------------------------------
! Subject:
! Method:
!  Ascents will be computed starting from each level of the input profile.
!  The parcel is raised from its level of origin LO to the level of condensation LC,
!  then to its level of free convection LFC (the parcel becomes buoyant),
!  then to the level of neutral buoyancy LNB, where the the parcel becomes unbuoyant.
! Externals:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
!    2008-12-22, J.M. Piriou: output of real data for pressure levels of LC, LFC and LNB.
! --------------------------------------------------------------
! In input:
! ----------
!  LOGICAL: ldverbose: debugging mode only: prints on standard output, if .true.; should be .false., usually.
!  INTEGER: klev: number of levels.
!  REAL: pt: temperature (K).
!  REAL: pqv: specific humidity (no dim).
!  REAL: pp: pressure (Pa).
! WARNING: pressure values are supposed to icrease from pp(1) to pp(klev).
!  REAL pretained: fraction of the condensates which is retained, i.e. which does not precipitate.
!       if pretained=1. ==> reversible moist ascent.
!                      it is assumed that all the parcel's condensed
!                      water is retained, thus liquid and ice sustents reduce the buoyancy.
!        if pretained=0. ==> "irreversible" (pseudo-adiabatic) moist ascent.
!                       liquid and ice sustents precipitate
!                       instantaneously and thus do not affect the buoyancy.
!       pretained can be used with values between 0. and 1..
!  REAL pentr: vertical entrainment coefficient
!       (in m**-1, useful range: between 0. (no entrainment) and 3.E-03 m**-1).
!
! In output:
! -----------
!  REAL: pcin (J/kg): CIN, Convection INhibition,
!         massic energy to raise the parcel from
!         LO to LC then to LFC (see above).
!         Only negative terms are cumulated.
!  REAL: pcape (J/kg): CAPE, Convection Available Potential Energy,
!         massic energy  provided by the raise of the parcel
!         from LFC to LNB (see above).
!         Only positive terms are cumulated.
!  INTEGER: klc(jlev) LC of the parcel raised from jlev (level number).
!  INTEGER: klfc(jlev) LFC of the parcel raised from jlev (level number).
!  INTEGER: klnb(jlev) LNB of the parcel raised from jlev (level number).
!  REAL   : plc(jlev) LC of the parcel raised from jlev (pressure level in Pa).
!  REAL   : plfc(jlev) LFC of the parcel raised from jlev (pressure level in Pa).
!  REAL   : plnb(jlev) LNB of the parcel raised from jlev (pressure level in Pa).
!  INTEGER: kcloud: 1 if convective cloud at this level,
!          2 if "close to saturation" cloud at this level,
!          0 if no cloud at this level.
!          If both convective and "near saturation" are present, the output is 1.
!  REAL: pmc (kg/m2/s): convective mass flux profile, for a parcel starting from the lowest level.
!  REAL: pthetae_top (K): array receiving the potential temperature of the parcel raised up to the LNB.
!  REAL: pprec (kg/m2): precipitations cumulated over all ascents.
! --------------------------------------------------------------
!
!use parkind1, only : jpim, jprb
use constantes, only: rg,rtt,rd,rv
implicit none
!
integer(kind=4) :: ipos
integer(kind=4) :: jlev
integer(kind=4) :: jlev1
integer(kind=4) :: klev
integer(kind=4) :: kcloud(klev)
integer(kind=4) :: klc(klev), klfc(klev), klnb(klev)
integer(kind=4) :: ilevx
!
logical :: ldverbose
logical :: llsat
logical :: llbuoy
!
integer(kind=4) :: iulbuo
real(kind=8) :: plc(klev), plfc(klev), plnb(klev)
real(kind=8) :: pprec
real(kind=8) :: zdlog
real(kind=8) :: zprec
real(kind=8) :: zpress
real(kind=8) :: zqs
real(kind=8) :: zqv
real(kind=8) :: zqv1,zqv2
real(kind=8) :: zrt
real(kind=8) :: zt,pretained,zadd
real(kind=8) :: zt1,zt2,zql,zqi,zqc,fth_tv,fth_thetav,fth_thetavl,fth_thetal,fth_tvl
real(kind=8) :: zintegrande,zdz,zbuoy
real(kind=8) :: zintegrande_prec
real(kind=8) :: pcape(klev)
real(kind=8) :: zcape(klev)
real(kind=8) :: pcin(klev)
real(kind=8) :: pp(klev)
real(kind=8) :: pqv(klev)
real(kind=8) :: pt(klev)
real(kind=8) :: pthetae_top(klev)
real(kind=8) :: pentr,pmc(klev)
real(kind=8) :: fth_qs,fth_r_hum,fth_t_evol_ad_hum,fth_theta,fth_t_evol_ad_seche
real(kind=8) :: zcapex,zsat,zsat_prec,zpress_prec,fth_pinterpole
real(kind=8) :: zrhon,zrhoe,fth_entr
!
!-------------------------------------------------
! Default initializations.
!-------------------------------------------------
!
!-------------------------------------------------
! Initialize to zero.
!-------------------------------------------------
!
!
!-------------------------------------------------
! pcin: CIN: <= 0.: vertical integral from surf. to current level
!            of the buoyancy force, where <= 0.
!-------------------------------------------------
!
pcin=0.0_8
!
!-------------------------------------------------
! pcape: CAPE: >= 0.: vertical integral from surf. to current level
!            of the buoyancy force, where >= 0.
!-------------------------------------------------
!
pcape=0.0_8
!
!-------------------------------------------------
! zcape: vertical integral from surf. to current level
!        of the buoyancy force, everywhere!
!-------------------------------------------------
!
zcape=0.0_8
!
!-------------------------------------------------
! Mass flux.
!-------------------------------------------------
!
pmc=0.0_8
!
!-------------------------------------------------
! Integer levels.
!-------------------------------------------------
!
klc=0 ; klfc=0 ; klnb=0
plc=0.0_8 ; plfc=0.0_8 ; plnb=0.0_8
kcloud=0
pthetae_top=0.0_8
pprec=0.0_8
!
!-------------------------------------------------
! Write buoyancy profile on file.
!-------------------------------------------------
!
if(ldverbose) then
  iulbuo=97
  open(iulbuo,file='buoyancy.tmp.dta',form='formatted')
endif
!
!-------------------------------------------------
! Loop over origin of ascents.
! This loop can be done indifferently upwards or downwards.
!-------------------------------------------------
!
do jlev1=klev,1,-1
  !
  ! -------------------------------------------------
  ! ipos:
  ! 0 if parcel between LO and LC.
  ! 1 if parcel between LC and LFC.
  ! 2 if parcel between LFC and LNB.
  ! 3 if parcel between LNB and top of profile.
  ! -------------------------------------------------
  !
  ipos=0
  !
  ! -------------------------------------------------
  ! Diagnostic: kcloud=2 if close to saturation.
  ! -------------------------------------------------
  !
  if(pqv(jlev1)/fth_qs(pt(jlev1),pp(jlev1)) > 0.9_8) then
    !
    ! -------------------------------------------------
    ! "Near saturation" cloud.
    ! -------------------------------------------------
    !
    kcloud(jlev1)=2
  endif
  !
  ! -------------------------------------------------
  ! For each LO, one will raise the parcel up to the top.
  ! -------------------------------------------------
  !
  zt=pt(jlev1)
  zqv=pqv(jlev1)
  zprec=0.0_8
  zql=0.0_8
  zqi=0.0_8
  do jlev=jlev1,2,-1
    !
    ! -------------------------------------------------
    ! Saturation specific humidity.
    ! -------------------------------------------------
    !
    zqs=fth_qs(zt,pp(jlev))
    !
    ! -------------------------------------------------
    ! Pressure and saturation.
    ! -------------------------------------------------
    !
    zpress=pp(jlev)
    zsat=zqv - 0.99_8*zqs
    llsat=zsat > 0._8 ! true if saturated parcel.
    !
    ! -------------------------------------------------
    ! The parcel buoyancy is computed from the ratio
    ! between density of the parcel (which is a mixture of dry air, water vapour zqv
    ! and condensates zqc) and density of the environmental air.
    ! Note that zqc is zql+zqi and thus will be 0. if pretained=0.,
    ! i.e. if liquid and ice sustents are supposed to precipitate instantaneously.
    ! -------------------------------------------------
    !
    zqc=zql+zqi
    zrhon=pp(jlev)/(rd+(rv/rd-1.0_8)*zqv)/zt*(1.0_8+zqc) ! density of ascent parcel.
    zrhoe=pp(jlev)/(rd+(rv/rd-1.0_8)*pqv(jlev))/pt(jlev) ! density of environment parcel.
    zbuoy=rg*(zrhoe/zrhon-1.0_8)
    if(ldverbose .and. jlev1 == klev) write(iulbuo,fmt=*) -zpress/100.,zbuoy
    zintegrande=zbuoy*fth_r_hum(pqv(jlev))*pt(jlev)/rg
    llbuoy=zbuoy >= 0.0_8 ! TRUE IF BUOYANT PARCEL.
    !
    ! -------------------------------------------------
    ! CIN and CAPE integrals.
    ! -------------------------------------------------
    !
    if(jlev < jlev1) then
      zdlog=log(pp(jlev+1)/pp(jlev))
      zrt=0.5_8*(zintegrande+zintegrande_prec)*zdlog
      ! zrt=0.5_8*(ZINTEGRANDE+ZINTEGRANDE_prec)*(pp(jlev+1)-pp(jlev))/(0.5_8*(pp(jlev)+pp(jlev+1)))
      if(zrt > 0.0_8) then
        !
        ! -------------------------------------------------
        ! Cumulate CAPE if positive contribution.
        ! -------------------------------------------------
        !
        pcape(jlev1)=pcape(jlev1)+zrt
      else
        !
        ! -------------------------------------------------
        ! Cumulate CIN if negative contribution and below LFC.
        ! -------------------------------------------------
        !
        if(ipos <= 1) pcin(jlev1)=pcin(jlev1)+zrt
      endif
      zcape(jlev1)=zcape(jlev1)+zrt
    else
      zsat_prec=zsat
      zintegrande_prec=zintegrande
      zpress_prec=zpress
    endif
    if(jlev1 == klev) then
      !
      ! -------------------------------------------------
      ! Mass flux profile.
      ! -------------------------------------------------
      !
      pmc(jlev)=pp(jlev)/fth_r_hum(pqv(jlev))/pt(jlev)*sqrt(max(0.0_8,2.0_8*zcape(jlev1))) ! kg/m2/s.
    endif
    if(ldverbose .and. jlev1 == klev) then
      !
      ! -------------------------------------------------
      ! Saturation and CAPE profiles.
      ! -------------------------------------------------
      !
      write(77,fmt='(2i3,5(a,g9.3),a,i3,2(a,l3),6(a,g9.3))') &
        & jlev1,jlev,' qvn=',zqv,' qsn=',zqs,' thetan=',fth_theta(pp(jlev),zt)-rtt &
        & ,' theta=',fth_theta(pp(jlev),pt(jlev))-rtt,' pp=',pp(jlev)/100. &
        &,' ipos=',ipos,' sat=',llsat,' buoy=',llbuoy &
        &,' pcin=',pcin(jlev1),' pcape=',pcape(jlev1) &
        &,' psum=',zcape(jlev1),' tn=',zt,' t_env=',pt(jlev),' zbuoy=',zbuoy
      !
      ! -------------------------------------------------
      ! Theta, thetaV and thetaVL profiles.
      ! -------------------------------------------------
      !
      write(78,fmt='(1(a,i2.2),5(a,g10.4),2(a,l3),9(a,g10.4))') &
        & 'niv=',jlev,' theta = ',fth_theta(pp(jlev),zt),' thetaV = ',fth_thetav(pp(jlev),zt,zqv) &
        & ,' thetaVL = ',fth_thetavl(pp(jlev),zt,zqv,zqc)
      !
      ! -------------------------------------------------
      ! Theta, thetaV and thetaVL profiles.
      ! -------------------------------------------------
      !
      write(79,fmt='(9g14.5)') fth_theta(pp(jlev),zt),-pp(jlev)/100.
      write(80,fmt='(9g14.5)') fth_thetav(pp(jlev),zt,zqv),-pp(jlev)/100.
      write(81,fmt='(9g14.5)') fth_thetal(pp(jlev),zt,zqv,zqc),-pp(jlev)/100.
      write(82,fmt='(9g14.5)') fth_thetavl(pp(jlev),zt,zqv,zqc),-pp(jlev)/100.
      !
      ! -------------------------------------------------
      ! qv and qc=ql+qi profiles.
      ! -------------------------------------------------
      !
      write(83,fmt='(9g14.5)') zqv,-pp(jlev)/100.
      write(84,fmt='(9g14.5)') zqc,-pp(jlev)/100.
      write(85,fmt='(9g14.5)') pmc(jlev),-pp(jlev)/100.
      !
      ! -------------------------------------------------
      ! Buoyancy profile in m/s2.
      ! -------------------------------------------------
      !
      write(86,fmt='(9g14.5)') zbuoy,-pp(jlev)/100.
      !
      ! -------------------------------------------------
      ! Temperature profile in K, and buoyancy profile in m/s2.
      ! -------------------------------------------------
      !
      write(87,fmt='(9g14.5)') zt,pt(jlev),zt-pt(jlev),zbuoy,-pp(jlev)/100.
    endif
    !
    ! -------------------------------------------------
    ! Check-up transitions between LC, LFC and LNB.
    ! -------------------------------------------------
    !
    if(llsat) then
      !
      ! -------------------------------------------------
      ! Saturated parcel.
      ! -------------------------------------------------
      !
      if(llbuoy) then
        !
        ! -------------------------------------------------
        ! Buoyant parcel.
        ! -------------------------------------------------
        !
        if(ipos == 0) then
          !
          ! -------------------------------------------------
          ! While raising to LC, one has found both LC and LFC!...
          ! -------------------------------------------------
          !
          ipos=2
          klc(jlev1)=jlev
          plc(jlev1)=fth_pinterpole(zsat,zsat_prec,zpress,zpress_prec)
          klfc(jlev1)=jlev
          plfc(jlev1)=fth_pinterpole(zintegrande,zintegrande_prec,zpress,zpress_prec)
        elseif(ipos == 1) then
          !
          ! -------------------------------------------------
          ! While raising to LFC, one has found LFC.
          ! -------------------------------------------------
          !
          ipos=2
          klfc(jlev1)=jlev
          plfc(jlev1)=fth_pinterpole(zintegrande,zintegrande_prec,zpress,zpress_prec)
        elseif(ipos == 2) then
          !
          ! -------------------------------------------------
          ! While raising to LNB, one has to go on raising.
          ! -------------------------------------------------
          !
        elseif(ipos == 3) then
        else
          print*,'fth_cin_cape_old/ERROR: unexpected ipos!...',ipos
          call exit(1)
        endif
      else
        !
        ! -------------------------------------------------
        ! Unbuoyant parcel.
        ! -------------------------------------------------
        !
        if(ipos == 0) then
          !
          ! -------------------------------------------------
          ! While raising to LC, one has found LC.
          ! -------------------------------------------------
          !
          ipos=1
          klc(jlev1)=jlev
          plc(jlev1)=fth_pinterpole(zsat,zsat_prec,zpress,zpress_prec)
        elseif(ipos == 1) then
          !
          ! -------------------------------------------------
          ! While raising to LFC, one has to go on raising.
          ! -------------------------------------------------
          !
        elseif(ipos == 2) then
          !
          ! -------------------------------------------------
          ! While raising to LNB, one has found LNB.
          ! Raising from jlev1 can be stopped here.
          ! -------------------------------------------------
          !
          ipos=3
          klnb(jlev1)=jlev
          plnb(jlev1)=fth_pinterpole(zintegrande,zintegrande_prec,zpress,zpress_prec)
        elseif(ipos == 3) then
        else
          print*,'fth_cin_cape_old/ERROR: unexpected ipos!...',ipos
          call exit(1)
        endif
      endif
    else
      !
      ! -------------------------------------------------
      ! Unsaturated parcel.
      ! -------------------------------------------------
      !
      if(llbuoy) then
        !
        ! -------------------------------------------------
        ! Buoyant parcel.
        ! -------------------------------------------------
        !
        if(ipos == 0) then
          !
          ! -------------------------------------------------
          ! While raising to LC, one has to go on raising.
          ! -------------------------------------------------
          !
        elseif(ipos == 1) then
          !
          ! -------------------------------------------------
          ! The parcel is unsaturated and buoyant, above LC.
          ! Thus another LC point exists above.
          ! One restarts the search for LC.
          ! -------------------------------------------------
          !
          ipos=0
        elseif(ipos == 2) then
          !
          ! -------------------------------------------------
          ! Go on raising to LNB (ipos=2).
          ! -------------------------------------------------
          !
        elseif(ipos == 3) then
        else
          print*,'fth_cin_cape_old/ERROR: unexpected ipos!...',ipos
          call exit(1)
        endif
      else
        !
        ! -------------------------------------------------
        ! Unbuoyant parcel.
        ! -------------------------------------------------
        !
        if(ipos == 0) then
          !
          ! -------------------------------------------------
          ! Go on raising to LC (ipos=0).
          ! -------------------------------------------------
          !
        elseif(ipos == 1) then
          !
          ! -------------------------------------------------
          ! Go on raising to LFC (ipos=1).
          ! -------------------------------------------------
          !
        elseif(ipos == 2) then
          !
          ! -------------------------------------------------
          ! While raising to LNB, one has found LNB.
          ! Raising from jlev1 can be stopped here.
          ! -------------------------------------------------
          !
          ipos=3
          klnb(jlev1)=jlev
          plnb(jlev1)=fth_pinterpole(zintegrande,zintegrande_prec,zpress,zpress_prec)
        elseif(ipos == 3) then
        else
          print*,'fth_cin_cape_old/ERROR: unexpected ipos!...'
          call exit(1)
        endif
      endif
    endif
    if(llsat) then
      !
      ! -------------------------------------------------
      ! If the parcel is oversaturated, this oversaturation is removed.
      ! This is done through an isobaric transformation from (zt,zqv) to (zt1,zqv1).
      ! -------------------------------------------------
      !
      call fth_evol_ad_hum_isobare(zt,zqv,pp(jlev),zt1,zqv1)
      zadd=pretained*(zqv-zqv1)
      if(zt1 >= rtt) then
        zql=zql+zadd
      else
        zqi=zqi+zadd
      endif
      !
      ! -------------------------------------------------
      ! Moist adiabatic ascent.
      ! Transformation from (zt1,zqv1) to (zt2,zqv2).
      ! -------------------------------------------------
      !
      zt2=fth_t_evol_ad_hum(zt1,pp(jlev),pp(jlev-1))
      zqv2=fth_qs(zt2,pp(jlev-1))
      if(zqv1 > zqv2) then
        zadd=pretained*(zqv1-zqv2)
        if(zt2 >= rtt) then
          zql=zql+zadd
        else
          zqi=zqi+zadd
        endif
      endif
      !
      ! -------------------------------------------------
      ! Cumulate precipitations from ascent started at jlev1.
      ! -------------------------------------------------
      !
      zprec=zprec+max(0.0_8,(zqv-zqv2))*(pp(jlev)-pp(jlev-1))/rg
      !
      ! -------------------------------------------------
      ! Update parcel state.
      ! -------------------------------------------------
      !
      zt=zt2
      zqv=zqv2
    else
      !
      ! -------------------------------------------------
      ! Dry adiabatic ascent.
      ! -------------------------------------------------
      !
      zt=fth_t_evol_ad_seche(zt,zqv,pp(jlev),pp(jlev-1))
    endif
    !
    ! -------------------------------------------------
    ! Entrainment: relax towards the environment.
    ! -------------------------------------------------
    !
    zdz=(pp(jlev)-pp(jlev-1))/(0.5_8*(pp(jlev)+pp(jlev-1)))*fth_r_hum(0.5_8*(pqv(jlev)+(pqv(jlev-1)))) &
      & * 0.5_8*(pt(jlev)+(pt(jlev-1)))/rg
    if(zt > pt(jlev-1)) then
      zt=max(pt(jlev-1),fth_entr(zt,pt(jlev-1),zdz,pentr))
    else
      zt=min(pt(jlev-1),fth_entr(zt,pt(jlev-1),zdz,pentr))
    endif
    if(zqv > pqv(jlev-1)) then
      zqv=max(pqv(jlev-1),fth_entr(zqv,pqv(jlev-1),zdz,pentr))
    else
      zqv=min(pqv(jlev-1),fth_entr(zqv,pqv(jlev-1),zdz,pentr))
    endif
    !
    ! -------------------------------------------------
    ! Store values from previous level (just below).
    ! -------------------------------------------------
    !
    zsat_prec=zsat
    zintegrande_prec=zintegrande
    zpress_prec=zpress
  enddo
  if(ldverbose .and. jlev1 == klev) print*,'Final ql=',zql,', final qi=',zqi
  !
  ! -------------------------------------------------
  ! If CAPE is 0, CIN is put also to 0, in order
  ! not to saturate the graphics with very negative CINs
  ! where no convection is to be expected anyway.
  ! -------------------------------------------------
  !
  ! if(pcape(jlev1) == 0.0_8) pcin(jlev1) = 0.0_8
  !
  ! -------------------------------------------------
  ! If some CAPE is present, and CIN is not too large, all levels from LC to LNB are cloudy ones.
  ! -------------------------------------------------
  !
  if(pcape(jlev1) > 0. .and. abs(pcin(jlev1)) < 100.) then
    if(klc(jlev1) == 0 .or. klnb(jlev1) == 0) then
      !
      ! -------------------------------------------------
      ! Buoyancy in dry air. No cloud.
      ! -------------------------------------------------
      !
    else
      !
      ! -------------------------------------------------
      ! Usual case, for which buoyancy appeared
      ! at LFC, i.e. AFTER saturation.
      ! -------------------------------------------------
      !
      if(ldverbose) print*,'Parcel raised from level ',jlev1,': LNB at ',klnb(jlev1),', LC at ',klc(jlev1)
      if(ldverbose) print*,'CIN (jlev1=',jlev1,')=',pcin(jlev1)
      pprec=pprec+zprec
    endif
  endif
  !
  ! -------------------------------------------------
  ! Compute potential temperature at LNB.
  ! -------------------------------------------------
  !
  if(ipos == 3) then
    pthetae_top(jlev1)=fth_theta(zpress,zt)
  else
    pthetae_top(jlev1)=fth_theta(pp(jlev1),pt(jlev1))
  endif
enddo
!
!-------------------------------------------------
! Compute convective levels and write in kcloud:
! Firstly, diagnose which initial level has the highest (CIN+CAPE).
! Secondly, put kcloud=1 from LC to LNB of this ascent.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Diagnose which initial level has the highest (CIN+CAPE).
!-------------------------------------------------
!
zcapex=0.
ilevx=0
do jlev=1,klev
  if(pcape(jlev)+pcin(jlev) > zcapex) then
    zcapex=pcape(jlev)+pcin(jlev)
    ilevx=jlev
  endif
enddo
!
!-------------------------------------------------
! Put kcloud=1 from LC to LNB of this ascent.
!-------------------------------------------------
!
if(ilevx /= 0) then
  if(klc(ilevx) == 0) then
    !
    ! -------------------------------------------------
    ! No condensation was found ==> no cloud.
    ! -------------------------------------------------
    !
    kcloud=0
  elseif(klnb(ilevx) == 0) then
    !
    ! -------------------------------------------------
    ! No LNB found ==> cloud from LC to the top of the atmosphere.
    ! -------------------------------------------------
    !
    do jlev=klc(ilevx),1,-1
      kcloud(jlev)=1
    enddo
  else
    do jlev=klc(ilevx),klnb(ilevx),-1
      kcloud(jlev)=1
    enddo
  endif
endif
if(ldverbose) close(iulbuo)
end
function fth_hcla(klev,pthetav,pz)
! --------------------------------------------------------------
! //// *fth_hcla* Hauteur de la couche limite atmosphérique.
! --------------------------------------------------------------
! Subject:
!
! Explicit arguments:
!
! Implicit arguments:
!
! Method:
!  Inspirée de Ayotte 1996, Bound. Layer Meteor., vol 79, 131-175, equ (9) page 141.
!  Afin de rendre le diagnostic plus robuste aux couches minces
!  cette équation a été intégrée sur la verticale, avec fluctuations linéaires
!  de couche à couche, cf doc. interne Piriou et Geleyn 2002
!  "Diagnostics de hauteur de couche limite".
!
! Externals:
!
! Auteur/author:   2002-03, J.M. Piriou.
!
! Modifications:
! --------------------------------------------------------------
! Input:
!  klev: nombre de niveaux du profil de thetav.
!  pthetav: profil vertical de thetav (K).
!  pz: profil vertical de l'élévation (m).
!      ATTENTION: pz doit croître de klev à 1.
!      pz doit être une élévation au dessus de la surface,
!      i.e. ne doit pas être une altitude. pz(klev) peut être
!      nul ou non, i.e. les tableaux d'entrée
!      peuvent contenir ou non la surface.
! Output:
!  fth_hcla: hauteur de la couche limite atmosphérique (m).
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
!use parkind1, only : jpim, jprb
implicit none
integer(kind=4) klev,jlev
real(kind=8) pthetav(klev), pz(klev)
real(kind=8) zthetav(klev+1), zz(klev+1)
real(kind=8) fth_hcla,zkhi0,zkhi1,zint,zthetav_prime(klev+1),zeps
real(kind=8) zbig,zdelta,zarg1,zarg2,zpsi,zthetav_prime_2,zbin
integer(kind=4) ilev
logical lldebug
!
!-------------------------------------------------
! Si les tableaux d'entrée ne contiennent pas la surface, on l'y ajoute.
!-------------------------------------------------
!
if(pz(klev) /= 0.0_8) then
  !
  ! -------------------------------------------------
  ! Il faut ajouter la surface.
  ! -------------------------------------------------
  !
  ilev=klev+1
  do jlev=1,klev
    zz(jlev)=pz(jlev)
    zthetav(jlev)=pthetav(jlev)
  enddo
  zz(klev+1)=0.0_8
  zthetav(klev+1)=pthetav(klev)
else
  !
  ! -------------------------------------------------
  ! La surface est déjà présente. Rien à ajouter.
  ! -------------------------------------------------
  !
  ilev=klev
  do jlev=1,klev
    zz(jlev)=pz(jlev)
    zthetav(jlev)=pthetav(jlev)
  enddo
endif
zkhi0=0.25_8
zkhi1=zkhi0/4._8
zeps=0.001_8
zbig=30._8
!
!-------------------------------------------------
! Intégrale ascendante.
!-------------------------------------------------
!
fth_hcla=0.0_8
zint=0.0_8
zthetav_prime(ilev)=0.0_8
lldebug=.false.
if(lldebug) then
  write(*,fmt='(3a)') '  niveau       |   z           |  thetav       |  ' &
    &,'  zpsi       |   zdelta      |   zarg2       |  ' &
    &,' fth_hcla        |               |'
endif
do jlev=ilev-1,1,-1
  !
  ! -------------------------------------------------
  ! Intégrale de thetav.
  ! -------------------------------------------------
  !
  zint=zint+(zz(jlev)-zz(jlev+1))*0.5_8*(zthetav(jlev)+zthetav(jlev+1))
  !
  ! -------------------------------------------------
  ! thetav': différence entre thetav du niveau courant
  ! et la moyenne de thetav entre la surface et le niveau courant.
  ! -------------------------------------------------
  !
  zthetav_prime(jlev)=zthetav(jlev)-zint/zz(jlev)
  !
  ! -------------------------------------------------
  ! Ecart de thetav' du niveau précédent au courant.
  ! -------------------------------------------------
  !
  zdelta=zthetav_prime(jlev)-zthetav_prime(jlev+1)
  !
  ! -------------------------------------------------
  ! Sécurité en division: zdelta va servir au dénominateur.
  ! On le borne pour éviter la division par zéro, en conservant son signe.
  ! -------------------------------------------------
  !
  zbin=max(0.0_8,sign(1.0_8,abs(zdelta)-zeps))
  zdelta=zbin*zdelta+(1.0_8-zbin)*zeps*sign(1.0_8,zdelta)
  zthetav_prime_2=zthetav_prime(jlev+1)+zdelta
  !
  ! -------------------------------------------------
  ! Sécurité en débordement de l'exponentielle: on utilise
  ! la fonction log(cosh) pour les arguments inférieurs à zbig,
  ! et son asymptote au-delà.
  ! -------------------------------------------------
  !
  zarg2=(zthetav_prime_2-zkhi0)/zkhi1
  zarg1=(zthetav_prime(jlev+1)-zkhi0)/zkhi1
  zpsi=0.5_8*(1.0_8-zkhi1/zdelta &
    & *(max(log(cosh(min(abs(zarg2),zbig))),abs(zarg2)-log(2.0_8)) &
    & -max(log(cosh(min(abs(zarg1),zbig))),abs(zarg1)-log(2.0_8))))
  !
  ! -------------------------------------------------
  ! Intégrale de dz, avec pour poids zpsi.
  ! -------------------------------------------------
  !
  fth_hcla=fth_hcla+(zz(jlev)-zz(jlev+1))*zpsi
  !
  ! -------------------------------------------------
  ! Impressions.
  ! -------------------------------------------------
  !
  if(lldebug) write(*,fmt='(i16,9g14.5)') jlev,zz(jlev),zthetav(jlev),zpsi,zdelta,zarg2,fth_hcla
enddo
end
function fth_cp(pqv,pql,pqi)
! --------------------------------------------------------------
! //// *fth_cp* Chaleur massique de l'air humide.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! pqv  humidité spécifique vapeur (sans dimension).
! pql  humidité spécifique liquide (sans dimension).
! pqi  humidité spécifique glace (sans dimension).
! En sortie: chaleur massique de l'air humide (J/kg/K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rcw
use constantes, only : rcs
implicit none
real(kind=8) :: pqv
real(kind=8) :: pql
real(kind=8) :: pqi
real(kind=8) fth_cp
fth_cp=rcpd*(1.0_8-pqv-pql-pqi)+rcpv*pqv+rcw*pql+rcs*pqi
end
function fth_e(pq,pp)
! --------------------------------------------------------------
! //// *fth_e* Tension de vapeur en fonction de l'humidité spécifique et de la pression.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: pq humidité spécifique (sans dimension).
! pp pression en Pa.
! En sortie: fth_e en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rd
use constantes, only : rv
!
implicit none
real(kind=8) :: pp
real(kind=8) :: pq
real(kind=8) fth_e
fth_e=pp*pq/((1.0_8-pq)*rd/rv+pq)
end
function fth_es(pt)
! --------------------------------------------------------------
! //// *fth_es* Fonction fth_es(T) par rapport à l'eau ou la glace.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.F. Geleyn.
! Modifications:
! --------------------------------------------------------------
! En entree: température en K.
! En sortie: fth_es en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ralpd
use constantes, only : ralpw
use constantes, only : rbetd
use constantes, only : rbetw
use constantes, only : rgamd
use constantes, only : rgamw
use constantes, only : rtt
implicit none
real(kind=8) :: pt
real(kind=8) fth_es
fth_es=exp( &
  & (ralpw+ralpd*max(0.0_8,sign(1.0_8,rtt-pt))) &
  & -(rbetw+rbetd*max(0.0_8,sign(1.0_8,rtt-pt)))/pt &
  & -(rgamw+rgamd*max(0.0_8,sign(1.0_8,rtt-pt)))*log(pt))
if(fth_es == 0.) then
  !
  ! -------------------------------------------------
  ! Cas d'underflow.
  ! -------------------------------------------------
  !
  write(*,fmt=*) 'fth_es/ATTENTION: underflow!... PT=',pt
  write(*,fmt=*)
endif
end
function fth_es_francoise(pt)
! --------------------------------------------------------------
! //// *fth_es_francoise* Fonction fth_es(T) par rapport à l'eau ou la glace.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-07, Françoise Guichard
! Modifications:
! --------------------------------------------------------------
! En entree: température en K.
! En sortie: fth_es en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pt
real(kind=8) :: fth_es_francoise,wa,wb,wc
wa   = 6.11e+2
wb   = 17.269
wc   = 35.86
fth_es_francoise=wa * exp(wb * (pt-273.16)/(pt-wc ) )
end
function fth_ess(pt)
! --------------------------------------------------------------
! //// *fth_ess* Fonction fth_es(T) par rapport à la glace.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.F. Geleyn.
! Modifications:
! --------------------------------------------------------------
! En entree: température en K.
! En sortie: fth_es en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ralps
use constantes, only : rbets
use constantes, only : rgams
implicit none
real(kind=8) :: pt
real(kind=8) fth_ess
fth_ess=exp(ralps-rbets/pt-rgams*log(pt))
end
function fth_esw(pt)
! --------------------------------------------------------------
! //// *fth_esw* Fonction fth_es(T) par rapport à l'eau.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.F. Geleyn.
! Modifications:
! --------------------------------------------------------------
! En entree: température en K.
! En sortie: fth_es en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ralpw
use constantes, only : rbetw
use constantes, only : rgamw
implicit none
real(kind=8) :: pt,fth_esw
fth_esw=exp(ralpw-rbetw/pt-rgamw*log(pt))
end
subroutine fth_evol_ad_hum_isobare(pt0,pqv0,pp0,pt,pqv)
! --------------------------------------------------------------
! //// *fth_evol_ad_hum_isobare* Calcul de l'état final d'une condensation isobare.
! --------------------------------------------------------------
! Sujet:
! On passe d'un état (T0,qv0,p0) avec qv0 > fth_qs(T0,p0)
! à un état (T,qv,p0), en vérifiant cp*dt+L*dq=0, et qv=fth_qs(T,p0), et à pression constante.
! Arguments explicites:
! Arguments implicites:
! Methode:
!  On résout en T
!       f(T)=cp*(T-T0)+L*(q-q0)=0
!  avec la contrainte q=fth_qs(T,p0),
!  On résout par la méthode de Newton, en itérant
!  T --> T-f(T)/f'(T), avec pour point de départ T0.
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt0: température de départ (K).
!  pqv0: humidité spécifique de départ (kg/kg).
!  pp0: pression de départ et d'arrivée (Pa).
! En sortie:
!  pt: température d'arrivée (K).
!  pqv: humidité spécifique d'arrivée (kg/kg).
!       Elle est égale à fth_qs(pt,pp0).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rtt
implicit none
integer(kind=4) :: jit
real(kind=8) :: pp0
real(kind=8) :: pqv
real(kind=8) :: pqv0
real(kind=8) :: pt
real(kind=8) :: pt0
real(kind=8) :: zcp
real(kind=8) :: zglace
real(kind=8) :: zl
real(kind=8) :: zqi
real(kind=8) :: zql
real(kind=8) :: zt_depart,fth_qs,fth_folh,fth_cp,fth_fderqs,fth_fderfolh
!
pt=pt0
pqv=fth_qs(pt,pp0)
zql=0.0_8
zqi=0.0_8
do jit=1,10
  zt_depart=pt
  !
  ! -------------------------------------------------
  ! Chaleur latente.
  ! -------------------------------------------------
  !
  zglace=max(0.0_8,sign(1.0_8,rtt-pt))
  zl=fth_folh(pt,zglace)
  !
  ! -------------------------------------------------
  ! Itération de Newton.
  ! -------------------------------------------------
  !
  zcp=fth_cp(pqv,zql,zqi)
  pt=pt-(zcp*(pt-pt0)+zl*(pqv-pqv0)) &
    & /(zcp+(rcpv-rcpd+zl)*fth_fderqs(pt,pp0)+pqv*fth_fderfolh(zglace))
  !
  ! -------------------------------------------------
  ! Vapeur saturante.
  ! -------------------------------------------------
  !
  pqv=fth_qs(pt,pp0)
  !
  ! -------------------------------------------------
  ! On sort de la boucle de Newton
  ! si on a la solution à epsilon près.
  ! -------------------------------------------------
  !
  if(abs(pt-zt_depart) < 0.01) exit
  zt_depart=pt
enddo
end
subroutine fth_evapo_isobare(pt0,pqv0,pqc0,pp,pt,pqv,pqc)
! --------------------------------------------------------------
! //// *fth_evapo_isobare* Calcul de l'état final d'une évaporation isobare.
! --------------------------------------------------------------
! Sujet:
!
! On passe d'un état (T0,qv0,qc0,p) avec qv0 < fth_qs(T0,p)
! à un état (T,qv,qc,p), en vérifiant cp*dt+L*dq=0, et qv=fth_qs(T,p), et à pression constante.
! On va évaporer à concurrence
!   - d'atteindre le qv saturant.
!   - d'épuiser qc0.
!
! Arguments explicites:
! Arguments implicites:
!
! Auteur/author:   2004-05, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt0: température de départ (K).
!  pqv0: quantité spécifique vapeur d'eau de départ (kg/kg).
!  pqc0: quantité spécifique d'eau condensée (liquide + glace) de départ (kg/kg).
!  pp: pression de départ et d'arrivée (Pa).
! En sortie:
!  pt: température d'arrivée (K).
!  pqv: quantité spécifique vapeur d'eau d'arrivée (kg/kg).
!  pqc: quantité spécifique d'eau condensée (liquide + glace) d'arrivée (kg/kg).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rtt
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pqv0
real(kind=8) :: pqc
real(kind=8) :: pqc0
real(kind=8) :: pt
real(kind=8) :: pt0
real(kind=8) :: zcp
real(kind=8) :: zglace
real(kind=8) :: zl
real(kind=8) :: fth_qs,fth_folh,fth_cp,zqs,zt1,zqv1
!
zqs=fth_qs(pt0,pp)
if(pqv0 < zqs) then
  !
  ! -------------------------------------------------
  ! L'humidité spécifique vapeur est soussaturante.
  ! -------------------------------------------------
  !
  if(pqc0 < 0.0_8) then
    write(*,fmt=*)
    write(*,fmt=*) 'fth_evapo_isobare/ERREUR: l''eau condensée est négative en entrée!...'
    write(*,fmt=*) pqc0
    call exit(1)
  endif
  if(pt0 > rtt) then
    zglace=0.0_8
    zl=fth_folh(pt0,zglace)
    zcp=fth_cp(pqv0,pqc0,0.0_8)
  else
    zglace=1.0_8
    zl=fth_folh(pt0,zglace)
    zcp=fth_cp(pqv0,0.0_8,pqc0)
  endif
  !
  ! -------------------------------------------------
  ! On évapore d'un coup une quantité de condensat égale à pqc0.
  ! On arrive à un état (T1,qv1,qc1=0).
  ! -------------------------------------------------
  !
  zqv1=pqv0+pqc0
  zt1=pt0-zl/zcp*pqc0
  !
  ! -------------------------------------------------
  ! Si après cette opération on est sursaturé, on revient
  ! à la valeur fth_hr=1.
  ! -------------------------------------------------
  !
  zqs=fth_qs(zt1,pp)
  if(zqv1 > zqs) then
    !
    ! -------------------------------------------------
    ! On est sursaturé. Retour à la valeur fth_hr=1.
    ! -------------------------------------------------
    !
    call fth_evol_ad_hum_isobare(zt1,zqv1,pp,pt,pqv)
    pqc=zqv1-pqv
  else
    pt=zt1
    pqv=zqv1
    pqc=0.0_8
  endif
else
  !
  ! -------------------------------------------------
  ! L'humidité spécifique vapeur est saturée ou sursaturée.
  ! On ne fait rien ici, car la présente routine est dévolue
  ! à l'évaporation seule.
  ! -------------------------------------------------
  !
  pt=pt0
  pqv=pqv0
  pqc=pqc0
endif
end
function fth_fderqs(pt,pp)
! --------------------------------------------------------------
! //// *fth_fderqs* Fonction dérivée partielle par rapport à la température de l'humidité spécifique saturante.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: pt température en K.
! pp pression en Pa.
! En sortie: humidité spécifique saturante (sans dimension).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rd
use constantes, only : retv
use constantes, only : rv
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: zes
real(kind=8) :: zrapp,fth_fderqs,fth_es,fth_fodles
!
zes=fth_es(pt)
zrapp=rv/rd*pp/zes
fth_fderqs=zrapp/(zrapp-retv)**2*fth_fodles(pt)
end
function fth_fodles(pt)
! --------------------------------------------------------------
! //// *fth_fodles* Fonction d(ln(fth_es(T)))/dT.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.F. Geleyn.
! Modifications:
! --------------------------------------------------------------
! En entree: température en K.
! En sortie: fth_es en Pa.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rbetd
use constantes, only : rbetw
use constantes, only : rgamd
use constantes, only : rgamw
use constantes, only : rtt
implicit none
real(kind=8) :: pt
real(kind=8) :: zglace,fth_fodles
!
!
! FONCTION DERIVEE DU LOGARITHME NEPERIEN DE LA PRECEDENTE (FOEW) .
! INPUT : pt = TEMPERATURE
! PDELARG = 0 SI EAU (QUELQUE SOIT pt)
! 1 SI GLACE (QUELQUE SOIT pt).
zglace=max(0.0_8,sign(1.0_8,rtt-pt))
fth_fodles = ( &
  & ( rbetw+zglace*rbetd ) &
  & - ( rgamw+zglace*rgamd ) * pt ) &
  & / ( pt*pt )
end
function fodlew(PTARG,PDELARG)
! --------------------------------------------------------------
! //// *FODLEW*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  2021-03, J.M. Piriou: interface entre ARPEGE et source local.
! Modifications:
! --------------------------------------------------------------
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes
implicit none
real(kind=8) :: PTARG,PDELARG
real(kind=8) :: FODLEW
FODLEW = (&
      &( RBETW+PDELARG*RBETD )&
    &- ( RGAMW+PDELARG*RGAMD ) * PTARG )&
    &/ ( PTARG*PTARG )
end
function foqs(PESPFAR)
! --------------------------------------------------------------
! //// *foqs*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  2021-03, J.M. Piriou: interface entre ARPEGE et source local.
! Modifications:
! --------------------------------------------------------------
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes
implicit none
real(kind=8) :: PESPFAR
real(kind=8) :: foqs
FOQS = PESPFAR / ( 1.0_8+RETV*MAX(0.0_8,&
    &(1.0_8-PESPFAR)) )
end
function FODQS( PQSFARG,PESPFAR,PDLEFAR )
! --------------------------------------------------------------
! //// *FODQS*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  2021-03, J.M. Piriou: interface entre ARPEGE et source local.
! Modifications:
! --------------------------------------------------------------
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes
implicit none
real(kind=8) :: PQSFARG,PESPFAR,PDLEFAR
real(kind=8) :: FODQS
FODQS = ( PQSFARG &
   &* (1.0_8-PQSFARG)*PDLEFAR ) / (1.0_8-PESPFAR)
end
function foew(pt,pdelta)
! --------------------------------------------------------------
! //// *foew* es(T, delta)
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  2021-03, J.M. Piriou: interface entre ARPEGE et source local: on veut éviter l'usage de delta.
! Modifications:
! --------------------------------------------------------------
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pt
real(kind=8) :: pdelta
real(kind=8) :: foew
real(kind=8) :: fth_esw
real(kind=8) :: fth_ess
if(pdelta > 0.5_8) then
  !
  ! Par rapport à l'eau.
  !
  foew=fth_esw(pt)
else
  !
  ! Par rapport à la glace.
  !
  foew=fth_ess(pt)
endif
end
function fth_folh(pt,pdelarg)
! --------------------------------------------------------------
! //// *fth_folh* Fonction chaleur latente vapeur/eau ou vapeur/glace.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-09, J.F. Geleyn.
! Modifications:
!         2004-04, J.M. Piriou: formule heuristique.
! --------------------------------------------------------------
! FONCTION CHALEUR LATENTE .
! Entrée: pt = TEMPERATURE
! PDELARG = 0 SI EAU (QUELQUE SOIT pt)
! 1 SI GLACE (QUELQUE SOIT pt).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes
implicit none
real(kind=8) :: pdelarg
real(kind=8) :: pt
real(kind=8) fth_folh
!
!
!-------------------------------------------------
! L complet heuristique (eau liquide/glace).
!-------------------------------------------------
!
fth_folh= pdelarg          *(rlstt-(pt-rtt)*(rcs-rcpv)) &
  &     +(1.0_8-pdelarg) *(rlvtt-(pt-rtt)*(rcw-rcpv))
!
!-------------------------------------------------
! L complet à la ARPEGE (eau liquide/glace).
!-------------------------------------------------
!
!fth_folh=rv*((rbetw+pdelarg*rbetd)-(rgamw+pdelarg*rgamd)*pt)
!
!-------------------------------------------------
! L Bolton (i.e. L vapeur/eau liquide).
!-------------------------------------------------
!
!fth_folh=(2.501-0.00237*(pt-rtt))*1.e6
!
!-------------------------------------------------
! L constant.
!-------------------------------------------------
!
!fth_folh=2.5e6
!
end
function fth_fderfolh(pdelarg)
! --------------------------------------------------------------
! //// *fth_fderfolh* Fonction dérivée partielle par rapport à la température de la fonction chaleur latente vapeur/eau ou vapeur/glace.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!     On calcule cette dérivée en faisant explicitement
!     le calcul d'un taux de variation.
!     Pourquoi faire cela pour arriver à une constante?
!     Simplement pour être SUR que la dérivée obtenue ici
!     soit consistante avec la fonction fth_folh,
!     fournie ailleurs dans ce même source.
!     Ainsi une modification du calcul de fth_folh
!     sera répercutée de facto ici.
! Externes:
! Auteur/author:  2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! FONCTION CHALEUR LATENTE .
! Entrée:
! PDELARG = 0 SI EAU (QUELQUE SOIT pt)
! 1 SI GLACE (QUELQUE SOIT pt).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rtt
implicit none
real(kind=8) :: pdelarg
real(kind=8) :: fth_fderfolh
real(kind=8) :: fth_folh
real(kind=8) :: zdeltat
!
zdeltat=10._8
fth_fderfolh=(fth_folh(rtt+zdeltat,pdelarg)-fth_folh(rtt,pdelarg))/zdeltat
!
end
function fth_hr(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_hr* Fonction humidité relative.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_hr (sans dimension).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt,fth_hr,fth_e,fth_es
!
fth_hr=fth_e(max(0.0_8,pqv),pp)/fth_es(pt)
end
subroutine fth_point_condens(pt0,pqv0,pp0,ptcond,ppcond)
! --------------------------------------------------------------
! //// *fth_point_condens* Calcul du point de condensation d'une particule donnée par (T0, qv0, p0).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!  On résout en p
!       fth_qs(T,p)=qv0
!       avec T=T0*(p0/p)**(R/cp)
!  On résout par la méthode de Newton, en itérant
!  p --> p-f(p)/f'(p), avec pour point de départ p0.
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt0: température de départ (K).
!  pqv0: humidité spécifique de départ (Pa).
!  pp0: pression de départ (Pa).
! En sortie:
!  ptcond température du point de condensation (K).
!  ppcond pression du point de condensation (Pa).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
integer(kind=4) :: jit
real(kind=8) :: pp0
real(kind=8) :: ppcond
real(kind=8) :: pqv0
real(kind=8) :: pt0
real(kind=8) :: ptcond
real(kind=8) :: zderi,zpcond2
real(kind=8) :: zdp
real(kind=8) :: zfonc
real(kind=8) :: zpprec
real(kind=8) :: zqv
real(kind=8) :: ztplus,fth_qs,fth_t_evol_ad_seche
!
ptcond=pt0
ppcond=pp0
zdp=5.
zqv=0.0_8
do jit=1,10
  zpprec=ppcond
  zfonc=fth_qs(ptcond,ppcond) ! valeur en p.
  ztplus=fth_t_evol_ad_seche(ptcond,zqv,ppcond,ppcond+zdp) ! T(p+dp).
  zderi=(fth_qs(ztplus,ppcond+zdp)-zfonc)/zdp ! dérivée [fth_qs(T(p+dp),p+dp)-fth_qs(T,p)]/dp.
  if(zderi == 0.) then
    write(*,fmt=*) 'FTH_POINT_CONDENS/ATTENTION ! dérivée nulle si entrée =',pt0,pqv0,pp0
    write(*,fmt=*)
    zderi=1.
  endif
  zpcond2=ppcond-(zfonc-pqv0)/zderi
  if(zpcond2 > 0.) then
    !
    ! -------------------------------------------------
    ! L'algorithme fournit une valeur de pression plausible.
    ! On la prend pour ébauche de Newton suivante.
    ! Dans les autres cas on laisse la valeur de pression stationner.
    ! -------------------------------------------------
    !
    ppcond=zpcond2
  endif
  ptcond=fth_t_evol_ad_seche(ptcond,zqv,zpprec,ppcond)
  if(abs(ppcond-zpprec) < 50.) exit
enddo
end
function fth_qs(pt,pp)
! --------------------------------------------------------------
! //// *fth_qs* humidité spécifique saturante en fonction de T et p.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   96-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: pt température en K.
! pp pression en Pa.
! En sortie: humidité spécifique saturante (sans dimension).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: zes,fth_qs,fth_es,fth_qv
!
zes=fth_es(pt)
fth_qs=fth_qv(zes,pp)
end
function fth_qs_francoise(pt,pp)
! --------------------------------------------------------------
! //// *fth_qs_francoise* humidité spécifique saturante en fonction de T et p.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-07, Françoise Guichard.
! Modifications:
! --------------------------------------------------------------
! En entree: pt température en K.
! pp pression en Pa.
! En sortie: humidité spécifique saturante (sans dimension).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: zes,fth_qs_francoise,fth_es_francoise,veps
!
zes=fth_es_francoise(pt)
veps = .62198
fth_qs_francoise=veps * zes / (pp + (veps-1.)*zes )
end
function fth_qv(pe,pp)
! --------------------------------------------------------------
! //// *fth_qv* qv en fonction de la tension de vapeur et de la pression.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2003-07, J.M. Piriou, d'après ARPEGE.
! Modifications:
! --------------------------------------------------------------
! En entree: pe tension de vapeur en Pa.
! pp pression en Pa.
! En sortie: q (sans dimension).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : retv
implicit none
real(kind=8) :: pe
real(kind=8) :: pp,fth_qv,zesp
!
zesp=pe/pp
fth_qv = zesp / ( 1.0_8+retv*max(0.0_8,(1.0_8-zesp)) )
!
end
function fth_r_hum(pqv)
! --------------------------------------------------------------
! //// *fth_r_hum* Constante spécifique de l'air humide.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! pqv  humidité spécifique vapeur (sans dimension).
! En sortie: constante spécifique de l'air humide (J/kg/K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rd
use constantes, only : rv
implicit none
real(kind=8) :: pqv,fth_r_hum
!
fth_r_hum=rd+(rv-rd)*pqv
end
function fth_delta_h_ad_hum(pp,pt,pp0,pt0,pglace,plog)
! --------------------------------------------------------------
! //// *fth_delta_h_ad_hum* Calcul de la fonction delta(h) dont on cherche le zéro
! pour calculer les évolutions pseudo-adiabatiques humides.
! Appelé par fth_t_evol_ad_hum.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2003-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt: température (K).
!  pglace: 1 si phase glace, 0 si phase liquide.
!  plog: constante à soustraire.
! En sortie:
!  delta(h) (J/kg).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp,pt,pp0,pt0,pglace,plog
real(kind=8) :: fth_delta_h_ad_hum,fth_qs,fth_r_hum,zqv,zl,zcp,zrdlog,fth_folh,zql,zqi,fth_cp,zqv0
!
!-------------------------------------------------
! Humidité saturante.
!-------------------------------------------------
!
zqv=fth_qs(pt,pp)
zqv0=fth_qs(pt0,pp0)
!
!-------------------------------------------------
! Chaleur latente.
!-------------------------------------------------
!
! Calcul où l'on considère que toute la chaleur latente libérée
! est récupérée par la vapeur d'eau pour se réchauffer.
!
zl=fth_folh(pt,pglace)
!
! Calcul où l'on considère qu'une part seulement de la chaleur latente libérée
! est récupérée par la vapeur d'eau pour se réchauffer:
! le reste reste dans l'eau liquide ou glace.
! Ce calcul fournit des CAPE plus réalistes,
! mais n'est pas conservatif d'une énergie totale
! qui ne prendrait pas en compte l'énergie interne des particules
! d'eau et de glace.
!
!zratio=0.92 ; zl=zratio*fth_folh(pt,pglace) ; zl0=zratio*fth_folh(pt0,pglace)
!
!
!-------------------------------------------------
! Cp et dlog.
!-------------------------------------------------
!
zql=0.
zqi=0.
zcp=fth_cp(zqv,zql,zqi)
zrdlog=fth_r_hum(zqv)*plog
fth_delta_h_ad_hum=zcp*(pt-pt0)+zl*(zqv-zqv0)-pt*zrdlog
end
function fth_t_evol_ad_seche(pt0,pqv0,pp0,pp)
! --------------------------------------------------------------
! //// *fth_t_evol_ad_seche* Calcul de l'état final d'une évolution adiabatique sèche.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt0: température de départ (K).
!  pqv0: humidité spécifique de départ (et d'arrivée, puisqu'aucune condensation ici) (kg/kg).
!        l'humidité spécifique sert simplement à calculer R et cp.
!  pp0: pression de départ (Pa).
!  pp: pression d'arrivée (Pa).
! En sortie:
!  fth_t_evol_ad_seche: température d'arrivée (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pp0
real(kind=8) :: pqv0
real(kind=8) :: pt0
real(kind=8) :: zqi
real(kind=8) :: zql,fth_t_evol_ad_seche,fth_r_hum,fth_cp
!
zql=0.0_8
zqi=0.0_8
fth_t_evol_ad_seche=pt0*(pp/pp0)**(fth_r_hum(pqv0)/fth_cp(pqv0,zql,zqi))
end
function fth_td_et(pe,pt)
! --------------------------------------------------------------
! //// *fth_td_et* Calcul de Td en fonction de la tension de vapeur.
! Cette fonction demande une ébauche en T pour initialiser la boucle de Newton.
! Quelle que soit la valeur de cette ébauche la boucle de Newton va converger vers la solution.
! Elle converge en moins d'itérations si l'ébauche est proche de la solution Td.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2018-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree: pe tension de vapeur en Pa.
!            pt température-ébauche en K.
! En sortie: fth_td_et en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
integer(kind=4) :: jit
real(kind=8),intent(in) :: pe
real(kind=8),intent(in) :: pt
real(kind=8) :: zderi
real(kind=8) :: zdt
real(kind=8) :: zes
real(kind=8) :: zt
real(kind=8) :: ztprec,fth_es,fth_td_et
real(kind=8) :: zinct,zhr,zdiff
logical :: llvalide
!
llvalide=.false. ! vrai si on cherche à valider la présente fonction.
zt=pt
!
! On itère une boucle de Newton
! pour annuler la fonction fth_es(t)-e.
!
zdt=0.1
do jit=1,100
  zes=fth_es(zt)
  zderi=(fth_es(zt+zdt)-zes)/zdt ! dérivée.
  ztprec=zt
  !
  !-------------------------------------------------
  ! Incrément de T: du type -f/f', où f' est la dérivée de f en T, et f la fonction dont on cherche le zéro..
  !-------------------------------------------------
  !
  zinct=-(zes-pe)/zderi ! incrément de T.
  if (llvalide) write(*,fmt=*) 'dans fth_td_et(e) : jit,zt avant = ',jit,zt,zes,pe,zinct
  zt=zt+zinct
  if (llvalide) write(*,fmt=*) 'dans fth_td_et(e) : jit,zt après = ',jit,zt,zes,pe,zinct
  if(abs(zt-ztprec) < 0.1) then
    !
    ! -------------------------------------------------
    ! On a atteint la précision demandée. Convergence de l'algorithme de Newton.
    ! -------------------------------------------------
    !
    fth_td_et=zt
    !
    !-------------------------------------------------
    ! Ecriture de diagnostics si on est en mode validation:
    ! nombre d'itérations nécessaires à la convergence, écart Td-T final.
    !-------------------------------------------------
    !
    if(llvalide) then
      zhr=pe/fth_es(pt)
      write(78,*) pt,zhr,jit
      zdiff=fth_td_et-pt
      write(79,*) pt,zhr,zdiff
    endif
    return
  endif
enddo
!
!-------------------------------------------------
! Si on est arrivé ici c'est que la boucle de Newton ne converge pas.
!-------------------------------------------------
!
write(*,fmt=*) 'fth_td_et : ATTENTION ! Non convergence algo Newton pour e=',pe,' Pa, T=' ,pt,' K'
!
!-------------------------------------------------
! On met une valeur manquante.
! On pourrait également mettre zt comme valeur, si on préfère se contenter de la valeur non convergée.
! Au 22.6.2018 une validation n'a montré aucun cas de non convergence, avec une plage
! en T de 100 à 320 K, et en hr de 0. à 2..
!----------- --------------------------------------
!
fth_td_et=999.999
end
function fth_td(pe)
! --------------------------------------------------------------
! //// *fth_td* Calcul de Td en fonction de la tension de vapeur.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:  1996-04, J.M. Piriou.
! Modifications:  2018-08, J.M. Piriou: fth_td appelle maintenant fth_td_et en lui fournissant une ébauche en T pour la boucle de Newton.
! --------------------------------------------------------------
! En entree: pe tension de vapeur en Pa.
! En sortie: fth_td en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8), intent(in) :: pe
real(kind=8) :: zt,fth_td_et,fth_td
!
zt=280.
fth_td=fth_td_et(pe,zt)
end
function fth_theta(pp,pt)
! --------------------------------------------------------------
! //// *fth_theta* Fonction température potentielle.
! --------------------------------------------------------------
! Sujet:
!  On ramène une particule donnée par (p,T) au niveau standard via une adiabatique sèche.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   98-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt température en K.
!  pp pression en Pa.
! En sortie:
!  fth_theta en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) fth_theta
!
fth_theta=pt*(ratm/pp)**(rd/rcpd)
end
function fth_thetas(pp,pt,pqv,pql,pqi,kopt)
! --------------------------------------------------------------
! //// *fth_thetas* The moist-air entropy potential temperature 
! --------------------------------------------------------------
! Sujet:
! Explicit Arguments:
! Implicit arguments:
! Method:
! See QJRMS 2011 and 2019 papers of P. Marquet, free copies available at
! https://arxiv.org/abs/1401.1097 and https://arxiv.org/abs/1901.08108
! External:
! Author:   2019-01, P. Marquet.
! Modifications:
! --------------------------------------------------------------
! Input:
!  pp pressure (Pa).
!  pt temperature (K).
!  pqv = specific content of water vapour (kg/kg).
!  pql = specific content of liquid water +rain? (kg/kg).
!  pqi = specific content of ice +snow? +graupel? (kg/kg).
!  kopt : 1=first-order 2=second-order 3=full-accuracy
! Output:
!  fth_thetas (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm,rcpd,rcpv,rd,rcpv,rkappa,rv,rtt
implicit none
!
real(kind=8) :: pp,pt,pqv,pql,pqi
integer(kind=4) :: kopt
!
real(kind=8) :: fth_folh
real(kind=8) :: zpr,ztr,zrr,zlambdar,zrstar,zqt,zrv,zlv,zls
real(kind=8) :: zepsq,zeta,zdelta,zepsil,zgamma,zlambda,zfirst
!
real(kind=8) :: fth_thetas
!
!-------------------------
! Derived basic constants
!-------------------------
zeta    = rv/rd
zdelta  = zeta-1._8
zepsil  = 1._8/zeta
zgamma  = rv/rcpd
zlambda = rcpv/rcpd-1._8
!
!----------------
! Security value
!----------------
zepsq = 1.E-9_8 ! kg/kg
!
!------------------------------
! zpr = reference pressure
! ztr = reference temperature
! zrr = reference water content
!------------------------------
zpr = 100000._8 ! Pa
ztr = 273.16_8  ! K
zrr = 0.00384_8 ! kg/kg 
!
!------------------
! Third law values
!------------------
zlambdar=5.8683_8 ! (-)
zrstar = 0.0124_8 ! (-)
!
!--------------------------------------
! zqt = "Total water" specific contant 
! zrv = "water vapor" mixing ratio
!--------------------------------------
zqt=pqv+pql+pqi
zrv=pqv/(1._8-zqt)
!
!--------------
! Latent heats
!--------------
!
zlv=fth_folh(pt,0.0_8) ! latent heat of vaporization
zls=fth_folh(pt,1.0_8) ! latent heat of sublimation
!
!-----------------------
! The first-order value
!-----------------------
!
zfirst=pt*(ratm/pp)**rkappa &
  & * exp(-(zlv*pql+zls*pqi)/rcpd/pt) &
  & * exp(zlambdar*zqt)
!
!-----------------------------------------
! The value of thetas depending on "kopt"
!-----------------------------------------
!
if (kopt == 1) then
 !print*,' KOPT =1 / First-order '
  !
  fth_thetas = zfirst
  !
else if (kopt == 2) then
 !print*,' KOPT =2 / Second-order '
  !
  fth_thetas = zfirst &
  & * exp(-zgamma*log(max(zepsq,zrv)/zrstar)*zqt) &
  & * exp(-zgamma*(pql+pqi))
  !
else if (kopt == 3) then
 !print*,' KOPT =3 / full-accuracy '
  !
  fth_thetas = zfirst &
  & * (pt/ztr)**(zlambda*zqt) &
  & * (zpr/pp)**(rkappa*zdelta*zqt) &
  & * (zrr/max(zepsq,zrv))**(zgamma*max(zepsq,zqt)) &
  & * ((1._8+zeta*zrv)/(1._8+zeta*zrr))**(rkappa*zdelta*zqt) &
  &  * (1._8+zeta*zrv)**rkappa
  !
else
  print*,' KOPT different from (1,2,3): ',KOPT
  call exit(1)
endif
!
end
function fth_thetapw(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetapw* Fonction theta'w.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2002-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique (kg/kg).
! En sortie:
!  fth_thetapw en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only: ratm
implicit none
real(kind=8) :: pp,pt,pqv,fth_thetapw,zt_cond,zp_cond,fth_t_evol_ad_hum
!
!-------------------------------------------------
! On recherche le point de condensation issu du point courant.
!-------------------------------------------------
!
call fth_point_condens(pt,pqv,pp,zt_cond,zp_cond)
!
!-------------------------------------------------
! On va du point de condensation au niveau p standard
! via une theta'w (évolution adiabatique humide irréversible).
!-------------------------------------------------
!
fth_thetapw=fth_t_evol_ad_hum(zt_cond,zp_cond,ratm)
end
function fth_thetap(pp,pt)
! --------------------------------------------------------------
! //// *fth_thetap* Fonction theta': température potentielle pseudo-adiabatique.
! --------------------------------------------------------------
! Sujet:
!  On ramène une particule donnée par (p,T) au niveau standard via une pseudo-adiabatique humide.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2003-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt température en K.
!  pp pression en Pa.
! En sortie:
!  fth_thetap en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) fth_thetap,fth_t_evol_ad_hum
!
!
!-------------------------------------------------
! On va du point courant au niveau p standard
! via une theta'w (évolution adiabatique humide irréversible).
!-------------------------------------------------
!
fth_thetap=fth_t_evol_ad_hum(pt,pp,ratm)
end
function fth_t(pp,ptheta)
! --------------------------------------------------------------
! //// *fth_t* Inversion en T de la température potentielle.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2001-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  ptheta potential temperature in K.
!  pp pressure in Pa.
! En sortie:
!  fth_t in K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
implicit none
real(kind=8) :: pp
real(kind=8) :: ptheta
real(kind=8) fth_t
!
fth_t=ptheta*(pp/ratm)**(rd/rcpd)
end
function fth_thetad(pp,pqv)
! --------------------------------------------------------------
! //// *fth_thetad* Fonction température de rosée potentielle.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_thetad en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv,fth_thetad,fth_e,fth_td
real(kind=8) :: zqv
!
zqv=max(1.e-7_8,pqv) ! protection.
fth_thetad=fth_td(fth_e(zqv,pp))*(ratm/pp)**(rd/rcpd)
end
function fth_thetaw(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetaw* Température du thermomètre mouillé ramenée adiabatiquement au niveau standard.
! ATTENTION: il ne faut pas la confondre avec la theta'w, qui est elle ramenée au niveau
! standard via une pseudo-adiabatique humide.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2003-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_thetaw en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
implicit none
real(kind=8) :: pp,pt,pqv
real(kind=8) :: fth_thetaw,fth_tw
!
fth_thetaw=fth_tw(pp,pt,pqv)*(ratm/pp)**(rd/rcpd)
end
function fth_thetae(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetae* Fonction température potentielle équivalente (calcul discret précis).
! --------------------------------------------------------------
! Sujet:
! Le thetae d'une particule est la température qu'elle aurait
! si on la montait selon une adiabatique sèche jusqu'en son point
! de condensation, puis selon une adiabatique humide jusqu'à
! épuiser son humidité spécifique, puis on la redescendait
! selon une adiabatique sèche jusqu'au niveau de pression standard (ratm dans le code).
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_thetae en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
integer(kind=4) :: ietapes
integer(kind=4) :: jetapes
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt
real(kind=8) :: zetape
real(kind=8) :: zp
real(kind=8) :: zplim
real(kind=8) :: zpprec
real(kind=8) :: zqv
real(kind=8) :: zqv1
real(kind=8) :: zqvprec
real(kind=8) :: zt
real(kind=8) :: zt1
real(kind=8) :: ztprec,fth_thetae,fth_qs,fth_t_evol_ad_hum,fth_t_evol_ad_seche,fth_theta
!
logical llsat
!
!-------------------------------------------------
! Ascendance jusqu'à épuisement de l'humidité spécifique,
! ce qu'on suppose arriver au niveau p=zplim
!-------------------------------------------------
!
llsat=pqv >= fth_qs(pt,pp)
if(llsat) then
  !
  ! -------------------------------------------------
  ! La particule est sursaturée dès le départ.
  ! On élimine cette sursaturation.
  ! -------------------------------------------------
  !
  call fth_evol_ad_hum_isobare(pt,pqv,pp,zt,zqv)
else
  zt=pt
  zqv=pqv
endif
zplim=10000. ! pression d'arrêt d'ascendance (Pa).
zetape=1000. ! pas de pression pour la discrétisation verticale (Pa).
ietapes=nint(abs(pp-zplim)/zetape)+1
zpprec=pp
ztprec=zt
zqvprec=zqv
do jetapes=1,ietapes
  zp=pp+(zplim-pp)*real(jetapes)/real(ietapes)
  if(llsat) then
    !
    ! -------------------------------------------------
    ! Particule saturée. Ascendance adiabatique humide.
    ! -------------------------------------------------
    !
    zt=fth_t_evol_ad_hum(ztprec,zpprec,zp)
    zqv=fth_qs(zt,zp)
  else
    !
    ! -------------------------------------------------
    ! Particule insaturée. Ascendance adiabatique sèche.
    ! -------------------------------------------------
    !
    zt=fth_t_evol_ad_seche(ztprec,zqvprec,zpprec,zp)
    if(zqvprec > fth_qs(zt,zp)) then
      !
      ! -------------------------------------------------
      ! On atteint le point de condensation.
      ! On élimine cette sursaturation.
      ! -------------------------------------------------
      !
      call fth_evol_ad_hum_isobare(zt,zqvprec,zp,zt1,zqv1)
      llsat=.true.
      zt=zt1
      zqv=zqv1
    else
      !
      ! -------------------------------------------------
      ! On n'atteint pas le point de condensation.
      ! qv est reconduit égal à lui-même.
      ! -------------------------------------------------
      !
      zqv=zqvprec
    endif
  endif
  !
  ! -------------------------------------------------
  ! Le niveau courant devient le précédent.
  ! -------------------------------------------------
  !
  ztprec=zt
  zqvprec=zqv
  zpprec=zp
enddo
!
!-------------------------------------------------
! thetae n'est autre que le theta de la particule
! parvenue au sommet.
!-------------------------------------------------
!
fth_thetae=fth_theta(zpprec,zt)
end
function fth_thetae_betts(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetae_betts* Betts equivalent potential temperature
! --------------------------------------------------------------
! Sujet:
! Explicit Arguments:
! Implicit arguments:
! Method:
!  See the Betts (1973) QJRMS paper (Vol 99, issue 419, p178-196)
!  "Non-precipitating cumulus convection and its parameterization"
!  available at
! https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/qj.49709941915
! External:
! Author:   2019-01, P. Marquet.
! Modifications:
! --------------------------------------------------------------
! Input:
!  pp pressure (Pa).
!  pt temperature (K).
!  pqv = specific content of water vapour (kg/kg).
! Output:
!  fth_thetae_betts (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm,rcpd,rkappa
implicit none
!
real(kind=8) :: pp,pt,pqv
!
real(kind=8) :: fth_folh
real(kind=8) :: zlv
!
real(kind=8) :: fth_thetae_betts
!
!--------------
! Latent heats
!--------------
!
zlv=fth_folh(pt,0.0_8) ! latent heat of vaporization
!
!----------------------------
! The equiv. potential temp.
!----------------------------
!
fth_thetae_betts=pt*(ratm/pp)**rkappa &
  & * exp(zlv*pqv/rcpd/pt)
!
end
function fth_thetae_emanuel(pp,pt,pqv,pql,pqi)
! -------------------------------------------------------------------
! //// *fth_thetae_emanuel* Emanuel equivalent potential temperature
! -------------------------------------------------------------------
! Sujet:
! Explicit Arguments:
! Implicit arguments:
! Method:
!  See Emanuel (1994) "Atmospheric Convection", page 120, Eq.(4.5.11)
! External:
! Author:   2019-01, P. Marquet.
! Modifications:
! --------------------------------------------------------------
! Input:
!  pp pressure (Pa).
!  pt temperature (K).
!  pqv = specific content of water vapour (kg/kg).
!  pql = specific content of liquid water +rain? (kg/kg).
!  pqi = specific content of ice +snow? +graupel? (kg/kg).
! Output:
!  fth_thetae_emanuel (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm,rd,rv,rcpd,rcw,rkappa,retv
implicit none
!
real(kind=8) :: pp,pt,pqv,pql,pqi
!
real(kind=8) :: fth_folh,fth_esw,fth_hr
real(kind=8) :: zrv,zrt,zcpast,zpdry,ze,zes,zhu,zlv
!
real(kind=8) :: fth_thetae_emanuel
!
!--------------------------------------
! zrv = "water vapour" mixing ratio
! zrt = "total water" mixing ratio
!--------------------------------------
zrv=pqv/(1._8-pqv-pql-pqi)
zrt=(pqv+pql+pqi)/(1._8-pqv-pql-pqi)
!
!----------------------
! A certain moist "cp"
!----------------------
zcpast = rcpd + zrt * rcw
!
!--------------------------------------
! zpdry = dry-air pressure (Pa)
! ze   = water vapour pressure (Pa)
! zes  = saturating value of "ze" (Pa)
! zhu  = relative humidity (0-1)
!--------------------------------------
zpdry = pp/(1.0_8+zrv*(1.0_8+retv))
ze    = pp-zpdry
zes   = fth_esw(pt)
zhu   = ze/zes
!
!--------------
! Latent heats
!--------------
!
zlv=fth_folh(pt,0.0_8) ! latent heat of vaporization
!
! function fth_esw(pt)
! function fth_hr(pp,pt,pqv)
!
!----------------------------
! The equiv. potential temp.
!----------------------------
!
fth_thetae_emanuel=pt*(ratm/zpdry)**(rd/zcpast) &
  & * (zhu)**(-zrv*rv/zcpast) &
  & * exp(zlv*zrv/zcpast/pt)
!
end
function fth_thetae_bolton(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetae_bolton* Fonction température potentielle équivalente (calcul direct approché).
! --------------------------------------------------------------
! Sujet:
! Le thetae d'une particule est la température qu'elle aurait
! si on la montait selon une adiabatique sèche jusqu'en son point
! de condensation, puis selon une adiabatique humide jusqu'à
! épuiser son humidité spécifique, puis on la redescendait
! selon une adiabatique sèche jusqu'au niveau de pression standard.
! Arguments explicites:
! Arguments implicites:
! Methode: David Bolton, MWR 1980.
! La formule se veut rapide à calculer, et donc
! fait des hypothèses, telle une dépendance affine de L en T
! sur toute la plage de températures, etc...
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_thetae_bolton en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt
real(kind=8) :: ze
real(kind=8) :: zr
real(kind=8) :: ztcond,fth_thetae_bolton,fth_e
!
ze=fth_e(max(0.0_8,pqv),pp)
zr=pqv/(1.0_8-pqv)*1000. ! r en g/kg.
if(ze /= 0.0_8) then
  ztcond=2840./(3.5*log(pt)-log(ze/100.)-4.805)+55. ! equ. (21) de Bolton 1980.
  ! zhr=ze/fth_es(pt) ; ztcond=1.0_8/(1.0_8/(pt-55.)-log(zhr)/2840.)+55. ! equ. (22) de Bolton 1980.
else
  ztcond=pt
endif
fth_thetae_bolton=pt*(ratm/pp)**(0.2854*(1.0_8-0.28e-3*zr)) &
  &*exp((3.376/ztcond-0.00254)*zr*(1+0.81e-3*zr)) ! equ. (43) de Bolton 1980.
end
function fth_thetaes(pp,pt)
! --------------------------------------------------------------
! //// *fth_thetaes* Fonction température potentielle équivalente de saturation (calcul discret précis).
! --------------------------------------------------------------
! Sujet:
! Le thetae d'une particule est la température qu'elle aurait
! si elle était saturée au départ (qv=fth_qs(T,p)),
! si on la montait selon une adiabatique sèche jusqu'en son point
! de condensation, puis selon une adiabatique humide jusqu'à
! épuiser son humidité spécifique, puis on la redescendait
! selon une adiabatique sèche jusqu'au niveau de pression standard (ratm dans le code).
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
! En sortie:
!  fth_thetaes en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: fth_thetaes,fth_thetae,fth_qs
!
fth_thetaes=fth_thetae(pp,pt,fth_qs(pt,pp))
end
function fth_thetaes_bolton(pp,pt)
! --------------------------------------------------------------
! //// *fth_thetaes_bolton* Fonction température potentielle équivalente de saturation (calcul direct approché).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
! En sortie:
!  fth_thetaes_bolton en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pt,fth_thetae_bolton,fth_qs,fth_thetaes_bolton
fth_thetaes_bolton=fth_thetae_bolton(pp,pt,fth_qs(pt,pp))
end
function fth_thetapw_arp(pp,pt,pqv,pql,pqi)
! --------------------------------------------------------------
! //// *fth_thetapw_arp* theta'w.
! --------------------------------------------------------------
! Sujet:
! CALCUL DE LA TEMPERATURE POTENTIELLE PSEUDO-ADIABATIQUE
! DU THERMOMETRE MOUILLE.
! Arguments explicites:
! Arguments implicites:
!
! Methode.
! --------
!
! CETTE ROUTINE N'A QU'UNE DIMENSION POUR SES VARIABLES D'ENTREE
! AFIN D'ETRE LA PLUS GENERALE POSSIBLE (UTILISATION SUR LES NIVEAUX
! DU MODELE COMME DU POST-PROCESSING, PAR EXEMPLE). TOUT ETAT DE
! L'AIR REALISTE EST ADMIS EN ENTREE ET L'ALGORITHME PREND EN COMPTE
! AUTOMATIQUEMENT UNE POSSIBLE TRANSITION DE PHASE LIQUIDE/GLACE.
! TROIS EQUATIONS IMPLICITES SONT RESOLUES PAR METHODE DE NEWTON:
! - RECHERCHE DU POINT DE SATURATION D'ENTROPIE EGALE PAR
! TRANSFORMATION REVERSIBLE ;
! - RECHERCHE DU POINT DE TEMPERATURE EGALE A CELLE DU POINT
! TRIPLE LE LONG DE L'ADIABATIQUE SATUREE IRREVERSIBLE ;
! - RECHERCHE DU POINT DE PRESSION EGALE A LA REFERENCE
! ATMOSPHERIQUE LE LONG D'UNE AUTRE (PARFOIS LA MEME) ADIABATIQUE
! IRREVERSIBLE.
! REMARQUES :
! - POUR LA PREMIERE ETAPE LA FORME SYMETRIQUE DE L'ENTROPIE
! HUMIDE PROPOSEE PAR P. MARQUET EST UTILISEE AFIN DE PERMETTRE UN
! MELANGE DE PHASES LIQUIDE ET GLACE DANS L'ETAT DE L'AIR ;
! - POUR LES DEUX DERNIERES ETAPES, PLUTOT QUE DE NEGLIGER
! COMME DE COUTUME LE TERME CONTENANT LE CP DU CONDENSAT, L'AUTEUR
! DE LA ROUTINE EN A DERIVE UNE APPROXIMATION QUASI-EXACTE ET PLUTOT
! BON MARCHE ;
! - POUR CES DEUX MEMES ETAPES, LES EBAUCHES DES BOUCLES DE
! NEWTON SONT OBTENUES PAR EXTRAPOLATION D'UNE LINEARISATION LOCALE
! APPROCHEE DES EQUATIONS ADIABATIQUE SATUREES.
!
! THIS ROUTINE HAS ONLY ONE DIMENSIONAL INPUT/OUTPUT ARRAYS IN
! ORDER TO BE THE MOST GENERAL POSSIBLE (USE ON MODEL OR ON POST-
! PROCESSING LEVELS, FOR EXAMPLE). ALL POSSIBLE REALISTIC INPUT
! STATES ARE ALLOWED AND THE ALGORITHM AUTOMATICALLY TAKES INTO
! ACCOUNT THE POTENTIAL LIQUID/ICE WATER TRANSITION.
! THREE IMPLICIT EQUATIONS ARE SOLVED BY NEWTON METHODS :
! - SEARCH OF THE SATURATION POINT OF EQUAL ENTROPY UNDER A
! REVERSIBLE TRANSFORM ;
! - SEARCH OF THE POINT OF TEMPERATURE EQUAL TO THAT OF THE
! TRIPLE POINT ALONG THE IRREVERSIBLE MOIST ADIABAT ;
! - SEARCH OF THE POINT OF REFERENCE ATMOSPHERIC PRESSURE
! ALONG ANOTHER (SOMETIMES IDENTICAL) IRREVERSIBLE MOIST ADIABAT.
! REMARKS :
! - FOR THE FIRST STEP THE SYMETRIC FORM OF THE MOIST ENTROPY
! PROPOSED BY P. MARQUET IS USED IN ORDER TO ALLOW A MIX OF LIQUID
! AND ICE WATER IN THE ATMOSPHERIC STATE ;
! - FOR THE TWO LAST STEPS, RATHER THAN THE USUAL NEGLECTION
! OF THE TERM MULTIPLIED BY CP OF THE CONDENSATE, THE ROUTINE'S
! AUTHOR DERIVED A QUASI EXACT AND NOT TOO EXPENSIVE ANALYTICAL
! APPROXIMATION FOR IT ;
! - FOR THE SAME STEPS, THE GUESSES OF THE NEWTON LOOP ARE
! OBTAINED BY VERTICAL EXTRAPOLATION OF A LINEAR LOCAL APPROXIMATION
! OF THE MOIST ADIABATS.
!
! Auteur/author: 92-09, J.F. Geleyn.
!
! Modifications.
! --------------
! 96-04, J. Calvo: Introduced a minimun in RH instead of a mini-
! mun in PQV. Added a security threshold in the
! calculation of the triple  point pressure
! first guess.
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rcs
use constantes, only : rcw
use constantes, only : rd
use constantes, only : restt
use constantes, only : rkappa
use constantes, only : rtt
use constantes, only : rv, nbiter
implicit none
integer(kind=4) :: jit
real(kind=8) :: zcplntt
real(kind=8) :: zcws
real(kind=8) :: zdelta
real(kind=8) :: zdf
real(kind=8) :: zdlew
real(kind=8) :: zdlstt
real(kind=8) :: ze
real(kind=8) :: zepsp
real(kind=8) :: zepsrh
real(kind=8) :: zew
real(kind=8) :: zf
real(kind=8) :: zkappa
real(kind=8) :: zkdi
real(kind=8) :: zkdw
real(kind=8) :: zkni
real(kind=8) :: zknw
real(kind=8) :: zlh
real(kind=8) :: zlhz
real(kind=8) :: zlhzi
real(kind=8) :: zlhzw
real(kind=8) :: zlstc
real(kind=8) :: zlstt
real(kind=8) :: zlstti
real(kind=8) :: zlsttw
real(kind=8) :: zprs
real(kind=8) :: zqi
real(kind=8) :: zql
real(kind=8) :: zqv
real(kind=8) :: zqvsat
real(kind=8) :: zrdsv
real(kind=8) :: zri
real(kind=8) :: zrl
real(kind=8) :: zrs
real(kind=8) :: zrv
real(kind=8) :: ztin
real(kind=8) :: ztmax
real(kind=8) :: ztmin,fth_folh,fth_qs,fth_es,fth_fodles,fth_thetapw_arp
!
real(kind=8) pp,pt,pqv,pql,pqi
real(kind=8) zrt,zcpt,zrrt,zs1,zcons,ztiter,zdel,zs2,zfunct,zpiter
!
! *
! ------------------------------------------------------------------
! I - CALCUL DES CONSTANTES DERIVEES DE CELLES ISSUES DE YOMCST ET
! CONSTANTES DE SECURITE.
!
! COMPUTATION OF CONSTANTS DERIVED FROM THOSE CONTAINED IN
! YOMCST AND SECURITY CONSTANTS.
!
zrdsv=rd/rv
zlsttw= fth_folh (rtt,0.0_8)/rtt+rcw*rv/( fth_folh (rtt,0.0_8)/rtt)
zlstti= fth_folh (rtt,1.0_8)/rtt+rcs*rv/( fth_folh (rtt,1.0_8)/rtt)
zlhzw= fth_folh (0.0_8,0.0_8)
zlhzi= fth_folh (0.0_8,1.0_8)
zknw=restt* fth_folh (rtt,0.0_8)/(rv*rtt)
zkni=restt* fth_folh (rtt,1.0_8)/(rv*rtt)
zkdw=rkappa*restt*( fth_folh (rtt,0.0_8)/(rv*rtt))**2
zkdi=rkappa*restt*( fth_folh (rtt,1.0_8)/(rv*rtt))**2
zcplntt=rcpd*log(rtt)
!
zepsp=10.
zepsrh=0.001
ztmin=155.
ztmax=355.
!
!
! *
! ------------------------------------------------------------------
! II - CALCUL DE LA TEMPERATURE DE SATURATION (EN GENERAL MAIS PAS
! FORCEMENT TEMPERATURE DU POINT DE CONDENSATION). LE RAPPORT DE
! MELANGE TOTAL ET LA TEMPERATURE POTENTIELLE HUMIDE -REVERSIBLE-
! SONT GARDES CONSTANTS DURANT L'OPERATION.
!
! COMPUTATION OF THE SATURATION TEMPERATURE (IN GENERAL BUT NOT
! SYSTEMATICALLY LIFTING CONDENSATION TEMPERATURE). THE TOTAL MIXING
! RATIO AND THE MOIST -REVERSIBLE- POTENTIAL TEMPERATURE ARE KEPT
! CONSTANT DURING THE PROCESS.
!
! - TEMPORAIRES .
!
! ZRT        : RAPPORT DE MELANGE TOTAL DE L'EAU.
! : TOTAL WATER MIXING RATIO.
! ZCPT       : PARTIE "CP" DU "KAPPA" IMPLICITE DE LA TEMP. CONSERVEE.
! : "CP" PART OF THE IMPLICIT "KAPPA" OF THE CONSERVED TEMP..
! ZRRT       : PARTIE "R" DU "KAPPA" IMPLICITE DE LA TEMP. CONSERVEE.
! : "R" PART OF THE IMPLICIT "KAPPA" OF THE CONSERVED TEMP..
! ZS1        : EXPRESSION DE L'ENTROPIE ASSOCIE A LA TEMP. CONSERVEE.
! : ENTROPY'S EXPRESSION LINKED TO THE CONSERVED TEMPERATURE.
! ZCONS      : CONSTANTE AUXILIAIRE POUR LA BOUCLE DE NEWTON.
! : AUXILIARY CONSTANT FOR THE NEWTON LOOP.
! ZTITER     : EBAUCHE POUR LA SOLUTION DE LA BOUCLE DE NEWTON EN TEMP..
! : FIRST GUESS FOR THE SOLUTION OF THE NEWTON LOOP ON TEMP..
! ZQVSAT     :
! : SATURATED SPECIFIC HUMIDITY
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! SECURITES.
! SECURITIES.
!
! QVSAT CALCULATION DEPENING ON THE SNOW OPTION.
!
zdelta=max(0.0_8,sign(1.0_8,rtt-pt))
!
zql=max(0.0_8,pql)
zqi=max(0.0_8,pqi)
ztin=max(ztmin,min(ztmax,pt))
zprs=max(zepsp,pp)
zqvsat=fth_qs(ztin,zprs)
zqv=max(zepsrh*zqvsat,pqv)
!
zrv=zqv/(1.0_8-zqv-zql-zqi)
zrl=zql/(1.0_8-zqv-zql-zqi)
zri=zqi/(1.0_8-zqv-zql-zqi)
zrt=zrv+zrl+zri
zcpt=rcpd+rcpv*zrt
zrrt=rd+rv*zrt
ze=(zrv*zprs)/(zrv+zrdsv)
zs1=zcpt*log(ztin)-rd*log(zprs-ze)-rv*zrt &
  & *log(ze)-( fth_folh (ztin,0.0_8)*zrl+ fth_folh (ztin,1.0_8)*zri)/ztin
zcons=zs1+rd*log(zrdsv/zrt)
ztiter=ztin
!
! BOUCLE DE NEWTON.
! NEWTON LOOP.
!
do jit=1,nbiter
  !
  ! CALCULS DEPENDANT DE L'OPTION NEIGE.
  ! SNOW OPTION DEPENDENT CALCULATIONS.
  !
  zdelta=max(0.0_8,sign(1.0_8,rtt-ztiter))
  !
  zew= fth_es (ztiter)
  zdlew= fth_fodles (ztiter)
  zf=zcons+zrrt*log(zew)-zcpt*log(ztiter)
  zdf=zrrt*zdlew-zcpt/ztiter
  ztiter=ztiter-zf/zdf
enddo
!
! *
! ------------------------------------------------------------------
! III - CALCUL DE LA PRESSION CORRESPONDANT AU POINT TRIPLE LE LONG
! DE L'ADIABATIQUE SATUREE IRREVERSIBLE PASSANT PAR LE POINT CALCULE
! PRECEDEMMENT. DANS LE CAS "LNEIGE=.T." LA TEMPERATURE DU POINT EN
! QUESTION DETERMINE LE CHOIX DES PARAMETRES LIES A "L" ET "CP".
!
! COMPUTATION OF PRESSURE CORRESPONDING TO THE TRIPLE POINT ON
! THE IRREVERSIBLE SATURATED ADIABAT PASSING THROUGH THE PREVIOUSLY
! OBTAINED POINT. IN THE "LNEIGE=.T." CASE THE LATTER'S TEMPERATURE
! DETERMINES THE CHOICE OF THE PARAMETERS LINKED TO "L" AND "CP".
!
!
! - TEMPORAIRES .
!
! ZDEL       : "MEMOIRE" DE LA VALEUR ZDELTA (EAU 0 / GLACE 1).
! : "MEMORY" OF THE ZDELTA (WATER 0 / ICE 1) VALUE.
! ZFUNCT     : EXPRESSION UTILISEE DANS LA BOUCLE DE NEWTON.
! : FUNCTIONAL EXPRESSION USED IN THE NEWTON LOOP.
! ZS2        : EXPRESSION DE LA PSEUDO ENTROPIE DE L'AD. IRREVERSIBLE.
! : PSEUDO ENTROPY'S EXPRESSION FOR THE IRREVERSIBLE AD..
! ZPITER     : EBAUCHE POUR LA SOLUTION DE LA BOUCLE DE NEWTON EN PRES..
! : FIRST GUESS FOR THE SOLUTION OF THE NEWTON LOOP ON PRES..
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! CALCULS DEPENDANT DE L'OPTION NEIGE.
! SNOW OPTION DEPENDENT CALCULATIONS.
!
zdelta=max(0.0_8,sign(1.0_8,rtt-ztiter))
!
zdel=zdelta
zew= fth_es (ztiter)
zlstt=zlsttw+zdelta*(zlstti-zlsttw)
zcws=rcw+zdelta*(rcs-rcw)
zlstc= fth_folh (ztiter,zdelta)/ztiter+zcws*rv &
  & /( fth_folh (ztiter,zdelta)/ztiter)
zfunct=zrdsv*restt*zlstt
zs2=zs1+zrt*(zlstc+rv*log(zew)-rcpv &
  & *log(ztiter))
zcons=zs2-zcplntt
zkappa=rkappa*(1.0_8+zrt* fth_folh (ztiter,zdelta)/(rd &
  & *ztiter))/(1.0_8+rkappa*zrt &
  & * fth_folh (ztiter,zdelta)**2/(rd*rv*ztiter**2))
zpiter=(zrdsv*zew/zrt)*(rtt/ztiter)**(1.0_8/zkappa) &
  & -restt
zpiter=max(zpiter,zepsp)
!
! BOUCLE DE NEWTON (UNE ITERATION DE PLUS POUR P QUE POUR T).
! NEWTON LOOP (ONE MORE ITERATION FOR P THAN FOR T).
!
do jit=1,nbiter+1
  zf=zcons+rd*log(zpiter)-zfunct/zpiter
  zdf=(rd*zpiter+zfunct)/zpiter**2
  zpiter=zpiter-zf/zdf
enddo
!
! RETOUR A LA PRESSION REELLE.
! RETURN TO THE REAL PRESSURE.
!
zpiter=zpiter+restt
!
! *
! ------------------------------------------------------------------
! IV - CALCUL DE LA TEMPERATURE CORRESPONDANT A P STANDARD LE LONG
! DE L'ADIABATIQUE SATUREE IRREVERSIBLE PASSANT PAR LE POINT CALCULE
! PRECEDEMMENT. DANS LE CAS "LNEIGE=.T." LA PRESSION DU POINT EN
! QUESTION DETERMINE LE CHOIX DES PARAMETRES LIES A "L" ET "CP".
!
! COMPUTATION OF THE TEMPERATURE CORRESPONDING TO THE STD. P ON
! THE IRREVERSIBLE SATURATED ADIABAT PASSING THROUGH THE PREVIOUSLY
! OBTAINED POINT. IN THE "LNEIGE=.T." CASE THE LATTER'S PRESSURE
! DETERMINES THE CHOICE OF THE PARAMETERS LINKED TO "L" AND "CP".
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! CALCULS DEPENDANT DE L'OPTION NEIGE.
! SNOW OPTION DEPENDENT CALCULATIONS.
!
zdelta=max(0.0_8,sign(1.0_8,zpiter-ratm))
!
zdlstt=(zdelta-zdel)*(zlstti-zlsttw)
zdel=zdelta
zs2=zs2+zdlstt*zrdsv*restt/(zpiter-restt)
zkappa=rkappa*(1.0_8+(zknw+zdelta*(zkni-zknw))/zpiter)/(1.0_8 &
  & +(zkdw+zdelta*(zkdi-zkdw))/zpiter)
ztiter=rtt*(ratm/zpiter)**zkappa
!
! BOUCLE DE NEWTON.
! NEWTON LOOP.
!
do jit=1,nbiter
  zew= fth_es(ztiter)
  zcws=rcw+zdel*(rcs-rcw)
  zlhz=zlhzw+zdel*(zlhzi-zlhzw)
  zlh= fth_folh (ztiter,zdel)
  zlstc=zlh/ztiter+zcws*rv/(zlh/ztiter)
  zrs=zrdsv*zew/(ratm-zew)
  zf=zs2-rcpd*log(ztiter)+rd*log(ratm-zew)-zrs*zlstc
  zdf=-rcpd/ztiter-zrs*((ratm/(ratm-zew))*zlstc*zlh/(rv &
    &   *ztiter**2)+zcws*rv*zlhz/zlh**2+(rcpv-zcws) &
    &   /ztiter)
  ztiter=ztiter-zf/zdf
enddo
!
! STOCKAGE DU RESULTAT.
! RESULT'S STORAGE.
!
fth_thetapw_arp=ztiter
end
function fth_thetav(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_thetav* Fonction température virtuelle potentielle.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_thetav en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt
real(kind=8) :: fth_thetav,fth_tv
!
fth_thetav=fth_tv(pt,pqv)*(ratm/pp)**(rd/rcpd)
end
function fth_thetal(pp,pt,pqv,pqc)
! --------------------------------------------------------------
! //// *fth_thetal* Fonction température potentielle avec prise en compte de la flottabilité des condensats.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!  Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press, 200 Madison Avenue, New York.
!  thetal calculé ici est celui associé à une transformation adiabatique humide réversible.
!  Si pqc est non nul, il est donc plus consistant d'avoir pqv=qsat.
! Externes:
! Auteur/author:   2001-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
!  pqc humidité spécifique de la somme de tous les condensats ayant atteint leur vitesse limite (qliq+qice+qrain+...) (sans dimension).
! En sortie:
!  fth_thetal en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
use constantes, only : rcpv
use constantes, only : rv
use constantes, only : rtt
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: pqv
real(kind=8) :: pqc
real(kind=8) :: fth_thetal,fth_folh
real(kind=8) :: zrt,zrl,zkappa,zgamma,zglace,zlv,zmult1,zmult2,zmult3
!
!-------------------------------------------------
! zrt: rapport de mélange total: vapeur + liq + glace + pluie + ...
!-------------------------------------------------
!
zrt=(pqv+pqc)/(1.0_8-pqv-pqc)
!
!-------------------------------------------------
! zrl: rapport de mélange des condensats.
!-------------------------------------------------
!
zrl=pqc/(1.0_8-pqv-pqc)
!
!-------------------------------------------------
! Kappa equ. (4.5.16) p122 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zkappa=(rd+zrt*rv)/(rcpd+zrt*rcpv)
!
!-------------------------------------------------
! Gamma equ. (4.5.16) p122 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zgamma=zrt*rv/(rcpd+zrt*rcpv)
!
!-------------------------------------------------
! zlv: chaleur latente.
!-------------------------------------------------
!
zglace=max(0.0_8,sign(1.0_8,rtt-pt))
zlv=fth_folh(pt,zglace)
!
!-------------------------------------------------
! thetal equ. (4.5.15) p121 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zmult1=(1.0_8-zrl/(rd/rv+zrt))**zkappa
if(zrl < zrt) then
  zmult2=(1.0_8-zrl/zrt)**(-zgamma)
else
  zmult2=1.0_8
endif
zmult3=exp(-zlv*zrl/((rcpd+zrt*rcpv)*pt))
fth_thetal=pt*(ratm/pp)**zkappa &
  & *zmult1*zmult2*zmult3
end
function fth_thetavl(pp,pt,pqv,pqc)
! --------------------------------------------------------------
! //// *fth_thetavl* Fonction température virtuelle potentielle avec prise en compte du poids des condensats.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
!  pqc humidité spécifique de la somme de tous les condensats ayant atteint leur vitesse limite (qliq+qice+qrain+...) (sans dimension).
! En sortie:
!  fth_thetavl en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: pqv
real(kind=8) :: pqc
real(kind=8) :: fth_thetavl,fth_tvl,fth_theta
fth_thetavl=fth_theta(pp,fth_tvl(pt,pqv,pqc))
end
function fth_thetavl_emanuel(pp,pt,pqv,pqc)
! --------------------------------------------------------------
! //// *fth_thetavl_emanuel* Fonction température virtuelle potentielle avec prise en compte de la chaleur latente potentielle des condensats.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!  Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press, 200 Madison Avenue, New York.
!  Thetavl_emanuel calculé ici est celui associé à une transformation adiabatique humide réversible.
!  Si pqc est non nul, il est donc plus consistant d'avoir pqv=qsat.
! Externes:
! Auteur/author:   2001-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
!  pqc humidité spécifique de la somme de tous les condensats ayant atteint leur vitesse limite (qliq+qice+qrain+...) (sans dimension).
! En sortie:
!  fth_thetavl_emanuel en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : ratm
use constantes, only : rcpd
use constantes, only : rd
use constantes, only : rcpv
use constantes, only : rv
use constantes, only : rtt
implicit none
real(kind=8) :: pp
real(kind=8) :: pt
real(kind=8) :: pqv
real(kind=8) :: pqc
real(kind=8) :: fth_thetavl_emanuel,fth_tv,fth_folh
real(kind=8) :: zrt,zrl,zkappa,zgamma,zglace,zlv,zmult1,zmult2,zmult3,zmult4
!
!-------------------------------------------------
! zrt: rapport de mélange total: vapeur + liq + glace + pluie + ...
!-------------------------------------------------
!
zrt=(pqv+pqc)/(1.0_8-pqv-pqc)
!
!-------------------------------------------------
! zrl: rapport de mélange des condensats.
!-------------------------------------------------
!
zrl=pqc/(1.0_8-pqv-pqc)
!
!-------------------------------------------------
! Kappa equ. (4.5.16) p122 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zkappa=(rd+zrt*rv)/(rcpd+zrt*rcpv)
!
!-------------------------------------------------
! Gamma equ. (4.5.16) p122 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zgamma=zrt*rv/(rcpd+zrt*rcpv)
!
!-------------------------------------------------
! zlv: chaleur latente.
!-------------------------------------------------
!
zglace=max(0.0_8,sign(1.0_8,rtt-pt))
zlv=fth_folh(pt,zglace)
!
!-------------------------------------------------
! Thetavl equ. (4.5.18) p122 Kerry A. Emanuel, "Atmospheric Convection", 1994 Oxford University Press.
!-------------------------------------------------
!
zmult1=(1.0_8-zrl/(1.0_8+zrt))
zmult2=(1.0_8-zrl/(rd/rv+zrt))**(zkappa-1.0_8)
if(zrl < zrt) then
  zmult3=(1.0_8-zrl/zrt)**(-zgamma)
else
  zmult3=1.0_8
endif
zmult4=exp(-zlv*zrl/((rcpd+zrt*rcpv)*pt))
fth_thetavl_emanuel=fth_tv(pt,pqv)*(ratm/pp)**zkappa &
  & *zmult1*zmult2*zmult3*zmult4
end
function fth_tv(pt,pqv)
! --------------------------------------------------------------
! //// *fth_tv* Fonction température virtuelle, aussi appelée température de densité.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_tv en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rd
use constantes, only : rv
implicit none
real(kind=8) :: pqv
real(kind=8) :: pt,fth_tv
!
!-------------------------------------------------
! Expression de Tv.
!-------------------------------------------------
!
fth_tv=pt*(1.0_8+(rv/rd-1.0_8)*pqv)
end
function fth_tw(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_tw* TEMPERATURE PSEUDO-ADIABATIQUE DU THERMOMETRE MOUILLE (point "bleu").
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_tw (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt
real(kind=8) :: zpcond
real(kind=8) :: ztcond,fth_tw,fth_t_evol_ad_hum
!
!
!-------------------------------------------------
! On cherche le point de condensation.
!-------------------------------------------------
!
call fth_point_condens(pt,pqv,pp,ztcond,zpcond)
!
!-------------------------------------------------
! On revient au niveau courant
! selon une adiabatique humide.
!-------------------------------------------------
!
fth_tw=fth_t_evol_ad_hum(ztcond,zpcond,pp)
end
function fth_qw(pp,pt,pqv)
! --------------------------------------------------------------
! //// *fth_qw* HUMIDITE SPECIFIQUE PSEUDO-ADIABATIQUE DU THERMOMETRE MOUILLE (point "bleu").
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp pression en Pa.
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
! En sortie:
!  fth_qw (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8) :: pp
real(kind=8) :: pqv
real(kind=8) :: pt
real(kind=8) :: fth_tw,fth_qw,fth_qs
!
!-------------------------------------------------
! On cherche le point bleu.
!-------------------------------------------------
!
fth_qw=fth_qs(fth_tw(pp,pt,pqv),pp)
end
function fth_tw_arp(pp,pt,pqv,pql,pqi)
! --------------------------------------------------------------
! //// *fth_tw_arp* TEMPERATURE PSEUDO-ADIABATIQUE DU THERMOMETRE MOUILLE (point "bleu").
! --------------------------------------------------------------
! Sujet:
! CALCUL DE LA TEMPERATURE PSEUDO-ADIABATIQUE DU THERMOMETRE MOUILLE.
! Arguments explicites:
! Arguments implicites:
!
! Methode.
! --------
!
! CETTE ROUTINE N'A QU'UNE DIMENSION POUR SES VARIABLES D'ENTREE
! AFIN D'ETRE LA PLUS GENERALE POSSIBLE (UTILISATION SUR LES NIVEAUX
! DU MODELE COMME DU POST-PROCESSING, PAR EXEMPLE). TOUT ETAT DE
! L'AIR REALISTE EST ADMIS EN ENTREE ET L'ALGORITHME PREND EN COMPTE
! AUTOMATIQUEMENT UNE POSSIBLE TRANSITION DE PHASE LIQUIDE/GLACE.
! TROIS EQUATIONS IMPLICITES SONT RESOLUES PAR METHODE DE NEWTON:
! - RECHERCHE DU POINT DE SATURATION D'ENTROPIE EGALE PAR
! TRANSFORMATION REVERSIBLE ;
! - RECHERCHE DU POINT DE TEMPERATURE EGALE A CELLE DU POINT
! TRIPLE LE LONG DE L'ADIABATIQUE SATUREE IRREVERSIBLE ;
! - RECHERCHE DU POINT DE PRESSION EGALE A LA REFERENCE
! ATMOSPHERIQUE LE LONG D'UNE AUTRE (PARFOIS LA MEME) ADIABATIQUE
! IRREVERSIBLE.
! REMARQUES :
! - POUR LA PREMIERE ETAPE LA FORME SYMETRIQUE DE L'ENTROPIE
! HUMIDE PROPOSEE PAR P. MARQUET EST UTILISEE AFIN DE PERMETTRE UN
! MELANGE DE PHASES LIQUIDE ET GLACE DANS L'ETAT DE L'AIR ;
! - POUR LES DEUX DERNIERES ETAPES, PLUTOT QUE DE NEGLIGER
! COMME DE COUTUME LE TERME CONTENANT LE CP DU CONDENSAT, L'AUTEUR
! DE LA ROUTINE EN A DERIVE UNE APPROXIMATION QUASI-EXACTE ET PLUTOT
! BON MARCHE ;
! - POUR CES DEUX MEMES ETAPES, LES EBAUCHES DES BOUCLES DE
! NEWTON SONT OBTENUES PAR EXTRAPOLATION D'UNE LINEARISATION LOCALE
! APPROCHEE DES EQUATIONS ADIABATIQUE SATUREES.
!
! THIS ROUTINE HAS ONLY ONE DIMENSIONAL INPUT/OUTPUT ARRAYS IN
! ORDER TO BE THE MOST GENERAL POSSIBLE (USE ON MODEL OR ON POST-
! PROCESSING LEVELS, FOR EXAMPLE). ALL POSSIBLE REALISTIC INPUT
! STATES ARE ALLOWED AND THE ALGORITHM AUTOMATICALLY TAKES INTO
! ACCOUNT THE POTENTIAL LIQUID/ICE WATER TRANSITION.
! THREE IMPLICIT EQUATIONS ARE SOLVED BY NEWTON METHODS :
! - SEARCH OF THE SATURATION POINT OF EQUAL ENTROPY UNDER A
! REVERSIBLE TRANSFORM ;
! - SEARCH OF THE POINT OF TEMPERATURE EQUAL TO THAT OF THE
! TRIPLE POINT ALONG THE IRREVERSIBLE MOIST ADIABAT ;
! - SEARCH OF THE POINT OF REFERENCE ATMOSPHERIC PRESSURE
! ALONG ANOTHER (SOMETIMES IDENTICAL) IRREVERSIBLE MOIST ADIABAT.
! REMARKS :
! - FOR THE FIRST STEP THE SYMETRIC FORM OF THE MOIST ENTROPY
! PROPOSED BY P. MARQUET IS USED IN ORDER TO ALLOW A MIX OF LIQUID
! AND ICE WATER IN THE ATMOSPHERIC STATE ;
! - FOR THE TWO LAST STEPS, RATHER THAN THE USUAL NEGLECTION
! OF THE TERM MULTIPLIED BY CP OF THE CONDENSATE, THE ROUTINE'S
! AUTHOR DERIVED A QUASI EXACT AND NOT TOO EXPENSIVE ANALYTICAL
! APPROXIMATION FOR IT ;
! - FOR THE SAME STEPS, THE GUESSES OF THE NEWTON LOOP ARE
! OBTAINED BY VERTICAL EXTRAPOLATION OF A LINEAR LOCAL APPROXIMATION
! OF THE MOIST ADIABATS.
!
! Auteur/author: 92-09, J.F. Geleyn.
!
! Modifications.
! --------------
! 96-04, J. Calvo: Introduced a minimun in RH instead of a mini-
! mun in PQV. Added a security threshold in the
! calculation of the triple  point pressure
! first guess.
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rcs
use constantes, only : rcw
use constantes, only : rd
use constantes, only : restt
use constantes, only : rkappa
use constantes, only : rtt
use constantes, only : rv,nbiter
implicit none
integer(kind=4) :: jit
real(kind=8) :: zcplntt
real(kind=8) :: zcws
real(kind=8) :: zdelta
real(kind=8) :: zdf
real(kind=8) :: zdlew
real(kind=8) :: zdlstt
real(kind=8) :: ze
real(kind=8) :: zepsp
real(kind=8) :: zepsrh
real(kind=8) :: zew
real(kind=8) :: zf
real(kind=8) :: zkappa
real(kind=8) :: zkdi
real(kind=8) :: zkdw
real(kind=8) :: zkni
real(kind=8) :: zknw
real(kind=8) :: zlh
real(kind=8) :: zlhz
real(kind=8) :: zlhzi
real(kind=8) :: zlhzw
real(kind=8) :: zlstc
real(kind=8) :: zlstt
real(kind=8) :: zlstti
real(kind=8) :: zlsttw
real(kind=8) :: zprs
real(kind=8) :: zqi
real(kind=8) :: zql
real(kind=8) :: zqv
real(kind=8) :: zqvsat
real(kind=8) :: zrdsv
real(kind=8) :: zri
real(kind=8) :: zrl
real(kind=8) :: zrs
real(kind=8) :: zrv
real(kind=8) :: ztin
real(kind=8) :: ztmax
real(kind=8) :: ztmin,fth_tw_arp
!
real(kind=8) pp,pt,pqv,pql,pqi,fth_folh,fth_qs,fth_es,fth_fodles
real(kind=8) zrt,zcpt,zrrt,zs1,zcons,ztiter,zdel,zs2,zfunct,zpiter
!
! *
! ------------------------------------------------------------------
! I - CALCUL DES CONSTANTES DERIVEES DE CELLES ISSUES DE YOMCST ET
! CONSTANTES DE SECURITE.
!
! COMPUTATION OF CONSTANTS DERIVED FROM THOSE CONTAINED IN
! YOMCST AND SECURITY CONSTANTS.
!
zrdsv=rd/rv
zlsttw= fth_folh (rtt,0.0_8)/rtt+rcw*rv/( fth_folh (rtt,0.0_8)/rtt)
zlstti= fth_folh (rtt,1.0_8)/rtt+rcs*rv/( fth_folh (rtt,1.0_8)/rtt)
zlhzw= fth_folh (0.0_8,0.0_8)
zlhzi= fth_folh (0.0_8,1.0_8)
zknw=restt* fth_folh (rtt,0.0_8)/(rv*rtt)
zkni=restt* fth_folh (rtt,1.0_8)/(rv*rtt)
zkdw=rkappa*restt*( fth_folh (rtt,0.0_8)/(rv*rtt))**2
zkdi=rkappa*restt*( fth_folh (rtt,1.0_8)/(rv*rtt))**2
zcplntt=rcpd*log(rtt)
!
zepsp=10.
zepsrh=0.001
ztmin=155.
ztmax=355.
!
!
! *
! ------------------------------------------------------------------
! II - CALCUL DE LA TEMPERATURE DE SATURATION (EN GENERAL MAIS PAS
! FORCEMENT TEMPERATURE DU POINT DE CONDENSATION). LE RAPPORT DE
! MELANGE TOTAL ET LA TEMPERATURE POTENTIELLE HUMIDE -REVERSIBLE-
! SONT GARDES CONSTANTS DURANT L'OPERATION.
!
! COMPUTATION OF THE SATURATION TEMPERATURE (IN GENERAL BUT NOT
! SYSTEMATICALLY LIFTING CONDENSATION TEMPERATURE). THE TOTAL MIXING
! RATIO AND THE MOIST -REVERSIBLE- POTENTIAL TEMPERATURE ARE KEPT
! CONSTANT DURING THE PROCESS.
!
! - TEMPORAIRES .
!
! ZRT        : RAPPORT DE MELANGE TOTAL DE L'EAU.
! : TOTAL WATER MIXING RATIO.
! ZCPT       : PARTIE "CP" DU "KAPPA" IMPLICITE DE LA TEMP. CONSERVEE.
! : "CP" PART OF THE IMPLICIT "KAPPA" OF THE CONSERVED TEMP..
! ZRRT       : PARTIE "R" DU "KAPPA" IMPLICITE DE LA TEMP. CONSERVEE.
! : "R" PART OF THE IMPLICIT "KAPPA" OF THE CONSERVED TEMP..
! ZS1        : EXPRESSION DE L'ENTROPIE ASSOCIE A LA TEMP. CONSERVEE.
! : ENTROPY'S EXPRESSION LINKED TO THE CONSERVED TEMPERATURE.
! ZCONS      : CONSTANTE AUXILIAIRE POUR LA BOUCLE DE NEWTON.
! : AUXILIARY CONSTANT FOR THE NEWTON LOOP.
! ZTITER     : EBAUCHE POUR LA SOLUTION DE LA BOUCLE DE NEWTON EN TEMP..
! : FIRST GUESS FOR THE SOLUTION OF THE NEWTON LOOP ON TEMP..
! ZQVSAT     :
! : SATURATED SPECIFIC HUMIDITY
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! SECURITES.
! SECURITIES.
!
! QVSAT CALCULATION DEPENING ON THE SNOW OPTION.
!
zdelta=max(0.0_8,sign(1.0_8,rtt-pt))
!
zql=max(0.0_8,pql)
zqi=max(0.0_8,pqi)
ztin=max(ztmin,min(ztmax,pt))
zprs=max(zepsp,pp)
zqvsat=fth_qs(ztin,zprs)
zqv=max(zepsrh*zqvsat,pqv)
!
zrv=zqv/(1.0_8-zqv-zql-zqi)
zrl=zql/(1.0_8-zqv-zql-zqi)
zri=zqi/(1.0_8-zqv-zql-zqi)
zrt=zrv+zrl+zri
zcpt=rcpd+rcpv*zrt
zrrt=rd+rv*zrt
ze=(zrv*zprs)/(zrv+zrdsv)
zs1=zcpt*log(ztin)-rd*log(zprs-ze)-rv*zrt &
  & *log(ze)-( fth_folh (ztin,0.0_8)*zrl+ fth_folh (ztin,1.0_8)*zri)/ztin
zcons=zs1+rd*log(zrdsv/zrt)
ztiter=ztin
!
! BOUCLE DE NEWTON.
! NEWTON LOOP.
!
do jit=1,nbiter
  !
  ! CALCULS DEPENDANT DE L'OPTION NEIGE.
  ! SNOW OPTION DEPENDENT CALCULATIONS.
  !
  zdelta=max(0.0_8,sign(1.0_8,rtt-ztiter))
  !
  zew= fth_es (ztiter)
  zdlew= fth_fodles (ztiter)
  zf=zcons+zrrt*log(zew)-zcpt*log(ztiter)
  zdf=zrrt*zdlew-zcpt/ztiter
  ztiter=ztiter-zf/zdf
enddo
!
! *
! ------------------------------------------------------------------
! III - CALCUL DE LA PRESSION CORRESPONDANT AU POINT TRIPLE LE LONG
! DE L'ADIABATIQUE SATUREE IRREVERSIBLE PASSANT PAR LE POINT CALCULE
! PRECEDEMMENT. DANS LE CAS "LNEIGE=.T." LA TEMPERATURE DU POINT EN
! QUESTION DETERMINE LE CHOIX DES PARAMETRES LIES A "L" ET "CP".
!
! COMPUTATION OF PRESSURE CORRESPONDING TO THE TRIPLE POINT ON
! THE IRREVERSIBLE SATURATED ADIABAT PASSING THROUGH THE PREVIOUSLY
! OBTAINED POINT. IN THE "LNEIGE=.T." CASE THE LATTER'S TEMPERATURE
! DETERMINES THE CHOICE OF THE PARAMETERS LINKED TO "L" AND "CP".
!
! - TEMPORAIRES .
!
! ZDEL       : "MEMOIRE" DE LA VALEUR ZDELTA (EAU 0 / GLACE 1).
! : "MEMORY" OF THE ZDELTA (WATER 0 / ICE 1) VALUE.
! ZFUNCT     : EXPRESSION UTILISEE DANS LA BOUCLE DE NEWTON.
! : FUNCTIONAL EXPRESSION USED IN THE NEWTON LOOP.
! ZS2        : EXPRESSION DE LA PSEUDO ENTROPIE DE L'AD. IRREVERSIBLE.
! : PSEUDO ENTROPY'S EXPRESSION FOR THE IRREVERSIBLE AD..
! ZPITER     : EBAUCHE POUR LA SOLUTION DE LA BOUCLE DE NEWTON EN PRES..
! : FIRST GUESS FOR THE SOLUTION OF THE NEWTON LOOP ON PRES..
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! CALCULS DEPENDANT DE L'OPTION NEIGE.
! SNOW OPTION DEPENDENT CALCULATIONS.
!
zdelta=max(0.0_8,sign(1.0_8,rtt-ztiter))
!
zdel=zdelta
zew= fth_es (ztiter)
zlstt=zlsttw+zdelta*(zlstti-zlsttw)
zcws=rcw+zdelta*(rcs-rcw)
zlstc= fth_folh (ztiter,zdelta)/ztiter+zcws*rv &
  & /( fth_folh (ztiter,zdelta)/ztiter)
zfunct=zrdsv*restt*zlstt
zs2=zs1+zrt*(zlstc+rv*log(zew)-rcpv &
  & *log(ztiter))
zcons=zs2-zcplntt
zkappa=rkappa*(1.0_8+zrt* fth_folh (ztiter,zdelta)/(rd &
  & *ztiter))/(1.0_8+rkappa*zrt &
  & * fth_folh (ztiter,zdelta)**2/(rd*rv*ztiter**2))
zpiter=(zrdsv*zew/zrt)*(rtt/ztiter)**(1.0_8/zkappa) &
  & -restt
zpiter=max(zpiter,zepsp)
!
! BOUCLE DE NEWTON (UNE ITERATION DE PLUS POUR P QUE POUR T).
! NEWTON LOOP (ONE MORE ITERATION FOR P THAN FOR T).
!
do jit=1,nbiter+1
  zf=zcons+rd*log(zpiter)-zfunct/zpiter
  zdf=(rd*zpiter+zfunct)/zpiter**2
  zpiter=zpiter-zf/zdf
enddo
!
! RETOUR A LA PRESSION REELLE.
! RETURN TO THE REAL PRESSURE.
!
zpiter=zpiter+restt
!
! *
! ------------------------------------------------------------------
! IV - CALCUL DE LA TEMPERATURE CORRESPONDANT A P STANDARD LE LONG
! DE L'ADIABATIQUE SATUREE IRREVERSIBLE PASSANT PAR LE POINT CALCULE
! PRECEDEMMENT. DANS LE CAS "LNEIGE=.T." LA PRESSION DU POINT EN
! QUESTION DETERMINE LE CHOIX DES PARAMETRES LIES A "L" ET "CP".
!
! COMPUTATION OF THE TEMPERATURE CORRESPONDING TO THE STD. P ON
! THE IRREVERSIBLE SATURATED ADIABAT PASSING THROUGH THE PREVIOUSLY
! OBTAINED POINT. IN THE "LNEIGE=.T." CASE THE LATTER'S PRESSURE
! DETERMINES THE CHOICE OF THE PARAMETERS LINKED TO "L" AND "CP".
!
! CALCULS PRELIMINAIRES.
! PRELIMINARY COMPUTATIONS.
!
!
! CALCULS DEPENDANT DE L'OPTION NEIGE.
! SNOW OPTION DEPENDENT CALCULATIONS.
!
zdelta=max(0.0_8,sign(1.0_8,zpiter-pp))
!
zdlstt=(zdelta-zdel)*(zlstti-zlsttw)
zdel=zdelta
zs2=zs2+zdlstt*zrdsv*restt/(zpiter-restt)
zkappa=rkappa*(1.0_8+(zknw+zdelta*(zkni-zknw))/zpiter)/(1.0_8 &
  & +(zkdw+zdelta*(zkdi-zkdw))/zpiter)
ztiter=rtt*(pp/zpiter)**zkappa
!
! BOUCLE DE NEWTON.
! NEWTON LOOP.
!
do jit=1,nbiter
  zew= fth_es(ztiter)
  zcws=rcw+zdel*(rcs-rcw)
  zlhz=zlhzw+zdel*(zlhzi-zlhzw)
  zlh= fth_folh (ztiter,zdel)
  zlstc=zlh/ztiter+zcws*rv/(zlh/ztiter)
  zrs=zrdsv*zew/(pp-zew)
  zf=zs2-rcpd*log(ztiter)+rd*log(pp-zew)-zrs*zlstc
  zdf=-rcpd/ztiter-zrs*((pp/(pp-zew))*zlstc*zlh/(rv &
    &   *ztiter**2)+zcws*rv*zlhz/zlh**2+(rcpv-zcws) &
    &   /ztiter)
  ztiter=ztiter-zf/zdf
enddo
!
! STOCKAGE DU RESULTAT.
! RESULT'S STORAGE.
!
fth_tw_arp=ztiter
end
function fth_tvl(pt,pqv,pqc)
! --------------------------------------------------------------
! //// *fth_tvl* Fonction température de densité: température virtuelle plus effets de flottabilité des condensats.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pt température en K.
!  pqv humidité spécifique de la vapeur d'eau (sans dimension).
!  pqc humidité spécifique de la somme de tous les condensats ayant atteint leur vitesse limite (qliq+qice+qrain+...) (sans dimension).
! En sortie:
!  fth_tvl en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rd
use constantes, only : rv
implicit none
real(kind=8) :: pqv,pqc
real(kind=8) :: pt,fth_tvl
!
!-------------------------------------------------
! Expression de tvl.
!-------------------------------------------------
!
fth_tvl=pt*(1.0_8+(rv/rd-1.0_8)*pqv-pqc)
end
function fth_t_evol_ad_hum(pt0,pp0,pp1)
! --------------------------------------------------------------
! //// *fth_t_evol_ad_hum* Calcul de l'état final d'une évolution pseudo-adiabatique humide.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
!  On résout en T
!       f(T)=cp*(T-T0)+L*(q-q0)+phi1-phi0=0
!  soit f(T)=cp*(T-T0)+L*(q-q0)-R*T*log(p1/p0)=0
!  avec la contrainte q=fth_qs(T,p1), et sachant que q0=fth_qs(T0,p0).
!  On résout par la méthode de Newton, en itérant
!  T --> T-f(T)/f'(T), avec pour point de départ T0.
! Externes:
! Auteur/author:   2000-09, J.M. Piriou.
! Modifications:
!     2010-02-04, J.M. Piriou. Suppression du calcul adiabatique au-dessus de l'altitude 150hPa.
! --------------------------------------------------------------
! En entree:
!  pt0: température de départ (K).
!  pp0: pression de départ (Pa).
!  pp1: pression d'arrivée (Pa).
! En sortie:
!  température d'arrivée (K).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rcpd
use constantes, only : rcpv
use constantes, only : rd
use constantes, only : rtt
use constantes, only : rv
implicit none
integer(kind=4) :: ietapes,itrac,jtrac
integer(kind=4) :: jetapes
integer(kind=4) :: jit,itermax,iul
logical :: llfin_adiab
real(kind=8) :: pp1
real(kind=8) :: pp0
real(kind=8) :: pt0
real(kind=8) :: zglace
real(kind=8) :: zetape
real(kind=8) :: zlog
real(kind=8) :: zp_arrivee
real(kind=8) :: zp_depart
real(kind=8) :: zt
real(kind=8) :: zt_depart,fth_t_evol_ad_hum
real(kind=8) :: zt_prec
real(kind=8) :: zderi,zf0,zf1,fth_delta_h_ad_hum,ztderi,zxt
real(kind=8) :: zqs,fth_t_evol_ad_seche,zfrac
character(len=200) clfic
zt=pt0
!
!-------------------------------------------------
! L'écart de pression à effectuer doit être
! cassé en suffisamment d'étapes pour que le calcul
! discret soit suffisamment précis.
!-------------------------------------------------
!
zetape=1000. ! pas de pression pour la discrétisation verticale (Pa).
ietapes=nint(abs(pp1-pp0)/zetape)+1
do jetapes=1,ietapes
  zp_depart=pp0+(pp1-pp0)*real(jetapes-1)/real(ietapes)
  zp_arrivee=pp0+(pp1-pp0)*real(jetapes)/real(ietapes)
  zt_depart=zt
  zlog=log(zp_arrivee/zp_depart)
  !
  ! -------------------------------------------------
  ! Chaleur latente.
  ! -------------------------------------------------
  !
  zglace=max(0.0_8,sign(1.0_8,rtt-zt_depart))
  if(.false.) then
    !
    ! -------------------------------------------------
    ! Mode mise au point.
    ! On écrit sur fichier la courbe fonction de T
    ! dont on cherche ici le zéro.
    ! -------------------------------------------------
    !
    write(clfic,fmt='(a,i2.2,a)') 'fonctionnelle.',jetapes,'.tmp.dta'
    iul=40 ; open(iul,file=clfic,form='formatted')
    write(iul,fmt=*) 'ZT_DEPART=',zt_depart
    write(iul,fmt=*) 'ZP_DEPART=',zp_depart
    write(iul,fmt=*) 'ZP_ARRIVEE=',zp_arrivee
    write(iul,fmt=*) ' '
    itrac=130
    do jtrac=1,itrac
      zfrac=real(jtrac-1)/real(itrac-1)
      zxt=1. *(1.-zfrac)     + 400.  *zfrac
      zf0=fth_delta_h_ad_hum(zp_arrivee,zxt,zp_depart,zt_depart,zglace,zlog)
      write(iul,*) zxt,zf0
    enddo
    close(iul)
  endif
  itermax=15
  do jit=1,itermax
    !
    ! -------------------------------------------------
    ! Itération de Newton. On résout fth_delta_h_ad_hum = 0.
    ! -------------------------------------------------
    !
    zf0=fth_delta_h_ad_hum(zp_arrivee,zt,zp_depart,zt_depart,zglace,zlog)
    ztderi=zt+0.1
    zf1=fth_delta_h_ad_hum(zp_arrivee,ztderi,zp_depart,zt_depart,zglace,zlog)
    zderi=(zf1-zf0)/(ztderi-zt)
    !
    ! -------------------------------------------------
    ! La fonction dont on cherche le zéro est convexe.
    ! Elle est donc a priori favorable à une recherche de zéro
    ! par la méthode de Newton.
    ! Elle comporte un seul zéro. Sa dérivée est positive au lieu du zéro.
    ! Cependant sa dérivée peut être négative, pour les valeurs faibles
    ! de T. Donc si l'ébauche est trop froide, l'algorithme ne converge pas.
    ! Donc ci-dessous, si la dérivée est négative on force une ébauche
    ! plus grande.
    ! -------------------------------------------------
    !
    zt_prec=zt
    if(zderi <= 0.) then
      write(*,fmt=*) 'FTH_T_EVOL_AD_HUM: dérivée négative'
      write(*,fmt=*) '  zt=',zt
      write(*,fmt=*) '  ZT_DEPART=',zt_depart
      write(*,fmt=*) '  ZP_DEPART=',zp_depart
      write(*,fmt=*) '  ZP_ARRIVEE=',zp_arrivee
      zt=400.
    else
      zt=zt-zf0/zderi
    endif
    if(zt < 100. .or. zt > 400.) then
      zt=400.
    endif
    !
    ! -------------------------------------------------
    ! On sort de la boucle de Newton
    ! si on a la solution à epsilon près.
    ! -------------------------------------------------
    !
    if(abs(zt-zt_prec) < 0.01) exit
    ! IF(JIT == ITERMAX) THEN
    ! WRITE(*,FMT=*) 'fth_t_evol_ad_hum/ATTENTION: non convergence de l''algorithme de Newton après ',ITERMAX,' itérations'
    ! WRITE(*,FMT=*) '  pt0,PP0,PP1=',PT0,PP0,PP1
    ! WRITE(*,FMT=*) '  fth_t_evol_ad_hum=',ZT
    ! ENDIF
  enddo
enddo
fth_t_evol_ad_hum=zt
end
function fth_tphig(pp,pt,cdmethode)
! --------------------------------------------------------------
! //// *fth_tphig* Calcul de la coordonnée X dans un T-Phi-gramme.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-03, J.M. Piriou.
! Modifications:
!                  2018-06, J.M. Piriou : température de sortie en °C et non plus en Kelvin.
! --------------------------------------------------------------
! En entree:
!  pp: pression de départ (Pa).
!  pt: température de départ (K).
!  cdmethode: méthode utilisée pour bâtir le T-Phi-gramme.
! En sortie:
!  température d'arrivée (°C).
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only: rtt,rpref
implicit none
real(kind=8) :: pp,pt
real(kind=8) :: fth_tphig,fth_theta,fth_thetapw,fth_qs
character*(*) cdmethode
if(trim(cdmethode) == 'AS') then
  !
  ! -------------------------------------------------
  ! Adiabatique sèche.
  ! Le T-Phi-gramme consiste à ramener adiabatiquement au niveau 1000hPa.
  ! Dans un tel T-Phi-gramme une ascendance adiabatique sèche (theta)
  ! est verticale.
  ! -------------------------------------------------
  !
  fth_tphig=fth_theta(pp,pt)
elseif(trim(cdmethode) == 'PAH') then
  !
  ! -------------------------------------------------
  ! Pseudo-adiabatique humide.
  ! Le T-Phi-gramme consiste à ramener au niveau 1000hPa
  ! selon une pseudo-adiabatique humide.
  ! Dans un tel T-Phi-gramme une ascendance pseudo-adiabatique humide (theta'w)
  ! est verticale.
  ! -------------------------------------------------
  !
  fth_tphig=fth_thetapw(pp,pt,fth_qs(pt,pp))
elseif(trim(cdmethode) == 'E761') then
  !
  ! -------------------------------------------------
  ! Le T-Phi-gramme consiste à ramener au niveau 1000hPa
  ! selon la relation entre T et p de l'émagramme
  ! oblique à 45°, dit "Emagramme 761".
  ! Sur cet émagramme particulier on a
  ! y = 764.4 - 254.8 * log10(p), avec y en mm, p en hPa.
  ! x = 2.91 * T + 764.4 - 254.8 * log10(p), avec x en mm, T en °C, p en hPa.
  ! Ici on ne cherche pas à spécifier x et y, mais seulement à ramener
  ! au niveau standard avec la même relation entre T et p que cet émagramme.
  ! Donc, connaissant T et p en entrée, on déduit y.
  ! Connaissant y, on ramène à y=0 à x constant.
  ! On en déduit le nouveau T.
  ! On résout donc en fth_tphig:
  ! 2.91 * (pt-rtt)    + 764.4 - 254.8 * log10(pp/100.)
  ! = 2.91 * (fth_tphig-rtt) + 764.4 - 254.8 * log10(rpref/100.)
  ! dont la solution est
  ! fth_tphig = pt + 254.8 * log10(rpref/pp)/2.91
  ! -------------------------------------------------
  !
  fth_tphig=pt+254.8_8*log(rpref/pp)/log(10._8)/2.91_8
else
  write(*,fmt=*) 'fth_tphig/ERREUR: méthode inconnue!...'
  write(*,fmt=*) trim(cdmethode)
  call exit(1)
endif
fth_tphig=fth_tphig-rtt
end
subroutine fth_cin_cape_simpl(klev,pt,pqv,pp,pentr,pcape,pcin,klevx)
! --------------------------------------------------------------
! //// *fth_cin_cape_simpl* Version simplifiée de calculs de CAPE et CIN.
! --------------------------------------------------------------
! Subject:
! Method:
! Externals:
! Auteur/author:   2004-05, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! In input:
! ----------
!  INTEGER: klev: number of levels.
!  REAL: pt: temperature (K).
!  REAL: pqv: specific humidity (no dim).
!  REAL: pp: pressure (Pa).
!  REAL: pentr: entraînement (m^-1).
!    WARNING: pressure values are supposed to icrease from pp(1) to pp(klev).
!
! In output:
! -----------
!  REAL: pcape (J/kg): CAPE maxi de ce profil.
!  REAL: pcin (J/kg): CIN de la particule ayant la CAPE maxi.
!  INTEGER: klevx: niveau de la particule présentant cette CAPE maxi.
! --------------------------------------------------------------
!
!use parkind1, only : jpim, jprb
implicit none
!
!-------------------------------------------------
! Arguments.
!-------------------------------------------------
!
integer(kind=4) :: klev
real(kind=8) :: pt(klev)
real(kind=8) :: pqv(klev)
real(kind=8) :: pp(klev)
real(kind=8) :: pentr
real(kind=8) :: pcin
real(kind=8) :: pcape
integer(kind=4) :: klevx
logical :: llverbose
!
!-------------------------------------------------
! Variables locales.
!-------------------------------------------------
!
real(kind=8) :: zcape(klev),zcin(klev)

integer(kind=4) :: ilcl(klev)
integer(kind=4) :: ilfc(klev)
integer(kind=4) :: ilnb(klev)

real(kind=8) :: zlc(klev)
real(kind=8) :: zlfc(klev)
real(kind=8) :: zlnb(klev)

integer(kind=4) :: icloud(klev),jlev,ilevsommet
real(kind=8) :: zmc(klev)
real(kind=8) :: zthetae_top(klev)
real(kind=8) :: zprec
!
!-------------------------------------------------
! Calcul complet de CIN et CAPE.
!-------------------------------------------------
!
ilevsommet=1
call fth_cin_cape(klev,ilevsommet,pt,pqv,pp,pt,pqv,pentr,zcape,zcin,ilcl,ilfc,ilnb)
!
!-------------------------------------------------
! Calcul de la CAPE maxi, et CIN de ce niveau de CAPE maxi.
!-------------------------------------------------
!
pcape=0.
pcin=0.
klevx=klev
do jlev=1,klev
  if(zcape(jlev) > pcape) then
    pcape=zcape(jlev)
    pcin=zcin(jlev)
    klevx=jlev
  endif
enddo
!
!-------------------------------------------------
! Diagnostics sur output standard.
!-------------------------------------------------
!
llverbose=.true.
if(llverbose) then
  write(*,fmt=*) 'FTH_CIN_CAPE_SIMPL:'
  write(*,fmt=*) '  Ascendance avec un entraînement de ',pentr,' m^-1.'
  write(*,fmt=*) '  CAPE maxi=',pcape,' CIN=',pcin
  write(*,fmt=*) '  Niveau de départ de la particule de CAPE maxi: ',klevx,', p=',pp(klevx)/100.,' hPa.'
  write(*,fmt=*) '  Niveau de neutralité de cette particule: ',ilnb(klevx),', p=',pp(ilnb(klevx))/100.,' hPa.'
  write(*,fmt=*) '  '
endif
end
subroutine fth_ajustement(pp,pt0,pqv0,pqc0,pt1,pqv1,pqc1)
! --------------------------------------------------------------
! //// *fth_ajustement* mise en cohérence de (p,T,qv,qc).
! --------------------------------------------------------------
! Subject:
!
! Ajustement thermodynamique:
!    - En entrée (T,qv,qc) peut être tel que qv > qsat.
!      On va alors condenser la vapeur d'eau jusqu'à atteindre fth_hr=1.
!      qv se trouve donc diminué et qc augmenté de la même quantité.
!      T se trouve augmenté.
!      En sortie on a qv1=qsat(T1,p).
!    - En entrée (T,qv,qc) peut être tel que qv < qsat.
!      En ce cas si qc > 0, on va évaporer qc.
!      On évapore qc sous deux conditions:
!      1. que qv résultant ne dépasse pas qs(T,p).
!      2. on ne peut évaporer plus que la valeur de qc!...
!      En sortie on a
!        - soit qv1=qsat(T1,p) et alors qc1 peut être > 0.
!        - soit qv1<qsat(T1,p) et alors qc1=0.
! Method:
! Externals:
! Auteur/author:   2004-05, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! In input:
! ----------
!  REAL: pp: pressure (Pa).
!  REAL: pt0: temperature (K).
!  REAL: pqv0: water vapour specific humidity (no dim).
!  REAL: pqc0: condensated water (liquid + solid) specific humidity (no dim).
!
! In output:
! -----------
!  REAL: pt1: temperature (K).
!  REAL: pqv1: water vapour specific humidity (no dim).
!  REAL: pqc1: condensated water (liquid + solid) specific humidity (no dim).
! --------------------------------------------------------------
!
!use parkind1, only : jpim, jprb
implicit none
!
!-------------------------------------------------
! Arguments.
!-------------------------------------------------
!
real(kind=8), intent(in) :: pp,pt0,pqv0,pqc0
real(kind=8), intent(out) :: pt1,pqv1,pqc1
!
!-------------------------------------------------
! Local.
!-------------------------------------------------
!
real(kind=8) :: zqs,fth_qs
!
!-------------------------------------------------
! Tests de cohérence.
!-------------------------------------------------
!
if(pqc0 < 0.) then
  write(*,fmt=*)
  write(*,fmt=*) 'fth_ajustement/ATTENTION: eau condensée < 0.!...'
  write(*,fmt=*) 'pp,pt0,pqv0,pqc0=',pp,pt0,pqv0,pqc0
endif
if(pqv0 < 0.) then
  write(*,fmt=*)
  write(*,fmt=*) 'fth_ajustement/ATTENTION: eau vapeur < 0.!...'
  write(*,fmt=*) 'pp,pt0,pqv0,pqc0=',pp,pt0,pqv0,pqc0
endif
!
!-------------------------------------------------
! Calculs.
!-------------------------------------------------
!
zqs=fth_qs(pt0,pp)
if(pqv0 > zqs) then
  !
  ! -------------------------------------------------
  ! Cas sursaturé.
  ! Condensation isobare.
  ! -------------------------------------------------
  !
  call fth_evol_ad_hum_isobare(pt0,pqv0,pp,pt1,pqv1)
  pqc1=pqc0+pqv0-pqv1
else
  !
  ! -------------------------------------------------
  ! Cas soussaturé.
  ! Evaporation isobare.
  ! -------------------------------------------------
  !
  call fth_evapo_isobare(pt0,pqv0,pqc0,pp,pt1,pqv1,pqc1)
endif
end
function fth_thetae_bolton_inv(pp,pthetae,pqv)
! --------------------------------------------------------------
! //// *fth_thetae_bolton_inv* Inversion en T de la fonction thetae.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2004-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  pp: pression de départ (Pa).
!  pthetae: thetae de départ (K).
!  pqv: humidité spécifique vapeur d'eau.
! En sortie:
!  température associée T (K), telle que thetae(pp,T,pqv)=pthetae.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
use constantes, only : rlvtt
use constantes, only : rcpd
implicit none
!
!-------------------------------------------------
! Arguments.
!-------------------------------------------------
!
real(kind=8), intent(in) :: pp,pthetae,pqv
real(kind=8) :: fth_thetae_bolton_inv
!
!-------------------------------------------------
! Locaux.
!-------------------------------------------------
!
real(kind=8) :: fth_thetae_bolton,zf,zdt,zfprime,zincrement,zt,fth_t
integer(kind=4) :: jit
!
!-------------------------------------------------
! Boucle de Newton pour résoudre en T thetae(pp,T,pqv)=pthetae.
!-------------------------------------------------
!
!
! Ebauche de T: inverse de theta, moins L/cp fois pqv.
!
zt=fth_t(pp,pthetae)-rlvtt/rcpd*pqv
!
! Boucle.
!
do jit=1,10
  !
  ! -------------------------------------------------
  ! Valeur de la fonction.
  ! -------------------------------------------------
  !
  zf=fth_thetae_bolton(pp,zt,pqv)-pthetae
  zdt=0.02
  zfprime=(fth_thetae_bolton(pp,zt+zdt,pqv)-pthetae-zf)/zdt
  zincrement=-zf/zfprime
  zt=zt+zincrement
  !
  ! -------------------------------------------------
  ! On sort de la boucle de Newton
  ! si on a la solution à epsilon près.
  ! -------------------------------------------------
  !
  if(abs(zincrement) < 0.01) exit
enddo
!
!-------------------------------------------------
! On affecte le résultat dans la fonction de sortie.
!-------------------------------------------------
!
fth_thetae_bolton_inv=zt
end
function fth_pinterpole(pycou,pyprec,ppcou,ppprec)
! --------------------------------------------------------------
! //// *fth_pinterpole* Calcul de l'altitude-pression à laquelle une fonction affine s'annule.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2008-12, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  (ppcou, pycou) et (ppprec,pyprec) sont les deux points par lesquels la fonction affine passe.
! En sortie:
!  fth_pinterpole est la valeur de p pour laquelle la fonction affine s'annule.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
!
!-------------------------------------------------
! Arguments.
!-------------------------------------------------
!
real(kind=8), intent(in) :: pycou,pyprec,ppcou,ppprec
real(kind=8) :: fth_pinterpole
!
!-------------------------------------------------
! Variables locales.
!-------------------------------------------------
!
real(kind=8) :: zecart
!
!-------------------------------------------------
! Si le dénominateur (zecart) s'annule, c'est que la routine
! a été appelée dans un cas non soluble.
! En ce cas on ne génère pas de message d'erreur,
! on protège simplement la division
! en divisant par 1.
!-------------------------------------------------
!
zecart=pyprec-pycou
if(zecart == 0.0_8) zecart=1.0_8
fth_pinterpole=(ppcou*pyprec-ppprec*pycou)/zecart
if(fth_pinterpole < min(ppprec,ppcou)) fth_pinterpole=min(ppprec,ppcou)
if(fth_pinterpole > max(ppprec,ppcou)) fth_pinterpole=max(ppprec,ppcou)
end
function fth_td_thr(pt,phr)
! --------------------------------------------------------------
! //// *fth_td_thr* Calcul de Td en fonction de température et humidité relative.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2018-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!     pt température en K.
!     phr humidité relative (sans dimension).
! En sortie: fth_td_thr en K.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8), intent(in) :: pt,phr
real(kind=8) :: ze
real(kind=8) :: fth_td_thr,fth_es,fth_td_et,fth_td
!
!-------------------------------------------------
! Calcul de e en fonction de hr.
!-------------------------------------------------
!
ze=max(0.01_8,phr)*fth_es(pt)
!
!-------------------------------------------------
! Td en fonction de e.
!-------------------------------------------------
!
fth_td_thr=fth_td_et(ze,pt)
end
function fth_entr(ppsi0,ppsie,pdz,pentr)
! --------------------------------------------------------------
! //// *fth_entr* Calcul de la valeur au sommet de la couche, après entraînement.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur/author:   2018-08-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!     ppsi0 : valeur de psi updraft       à la base de la couche.
!     ppsie : valeur de psi environnement à la base de la couche.
!     pdz   : épaisseur de la couche en m.
!     pentr : entraînement en m^-1.
! En sortie: psi au sommet de la couche.
! --------------------------------------------------------------
!use parkind1, only : jpim, jprb
implicit none
real(kind=8), intent(in) :: ppsi0,ppsie,pdz,pentr
real(kind=8) :: fth_entr,zprod
!
!-------------------------------------------------
! Calcul explicite.
!-------------------------------------------------
!
!fth_entr=ppsi0+pentr*pdz*(ppsie-ppsi0)
!
!-------------------------------------------------
! Calcul implicite.
!-------------------------------------------------
!
zprod=pdz*pentr
fth_entr=(ppsi0+zprod*ppsie)/(1.+zprod)
end
module yomcape
!use parkind1, only : jpim, jprb
implicit none
save
real(kind=8), parameter :: GMISCINV=10.
real(kind=8), parameter :: GCAPEMIN=100.
logical :: LADAE=.false.
integer(kind=4), parameter :: NCAPEITER=2
end

!OPTIONS XOPT(NOEVAL)
SUBROUTINE FPCINCAPE(KST,KEND,KPROMA,KLEV,KLEVST,PENTRA,PMLDEP,PT,PRP,PQV,PTE,PQVE,PCAPE,PCIN,KLCL,KFCL,KLNB)

! --------------------------------------------------------------
! **** *FPCINCAPE* COMPUTE CAPE AND CIN.
! --------------------------------------------------------------
! SUBJECT:
!    ROUTINE COMPUTING CAPE AND CIN  

! INTERFACE:
!    *CALL* *FPCINCAPE*

! --------------------------------------------------------------
! -   INPUT ARGUMENTS
!     ---------------

! - DIMENSIONING

! KST      : FIRST INDEX OF LOOPS
! KEND     : LAST INDEX OF LOOPS
! KPROMA   : DEPTH OF THE VECTORIZATION ARRAYS
! KLEV     : END OF VERTICAL LOOP AND VERTICAL DIMENSION

! - VARIABLES
! KLEVST   : LEVEL FROM WHICH PARCEL IS RAISED
! PENTRA   : ENTRAINMENT
! PMLDEP   : Mean Layer DEPth for MLCAPE computation (Pa).
! PT       : TEMPERATURE (K)
! PRP      : PRESSURE (PA)
! PQV      : WATER VAPOUR SPECIFIC HUMIDITY (NO DIM)

! --------------------------------------------------------------
! -   OUTPUT ARGUMENTS
!     ---------------
! - VARIABLES
! PCAPE    : CAPE - CONVECTIVE AVAILABLE POTENTIAL ENERGY (J/KG)
!                   (POTENTIALLY AVAILABLE CONVECTIVE KINETIC ENERGY)
! PCIN     : CIN - CONVECTIVE INHIBITION (J/KG)
! KLCL     : CONDENSATION LEVEL
! KFCL     : FREE CONVECTION LEVEL
! KLNB     : LEVEL OF NEUTRAL BUOYANCY

! --------------------------------------------------------------
! -   IMPLICITE ARGUMENTS
!     -------------------
! YOMCAPE 
! YOMCST
! FCTTRM
! FCTAST
! FCTTIM

! --------------------------------------------------------------
! EXTERNALS:

! METHOD:

!      THE PARCEL IS RAISED FROM LEVEL (LO which is equal KLEVST) 
!      TO THE LEVEL OF CONDENSATION (LC),
!      FURTHER TO ITS LEVEL OF FREE CONVECTION (LFC), WHERE THE 
!      PARCEL BECOMES BUOYANT,
!      THEN FURTHER TO THE LEVEL OF NEUTRAL BUOYANCY (LNB), WHERE THE
!      PARCEL BECOMES UNBUOYANT.

!      CIN IS MASS SPECIFIC ENERGY TO RAISE THE PARCEL FROM 
!          from LO to LFC.
!          ONLY THE SUM OF NEGATIVE TERMS.
!      CAPE IS MASS SPECIFIC ENERGY  PROVIDED BY THE RAISE OF THE PARCEL
!          from LFC to LNB.
!          ONLY THE SUM OF POSITIVE TERMS.

! AUTHOR:   2001-03, J.M. PIRIOU, N. PRISTOV.

! MODIFICATIONS:
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!    2010-02-04  J.M. Piriou. Bug correction: derivative of qs(T,p) from the Newton loops.
!    2010-02-11  J.M. Piriou. Bug correction: JLON dimension of ZFDERQS0 array.
!    2010-02-17  J.M. Piriou. Protections in case of cold temperature, low pressure, etc.
!    2010-02-17  J.M. Piriou. First Newton loop in a single DO loop.
!    2010-02-17  J.M. Piriou. Compute CAPE only below the convective reference level (NTCVIM).
!    2018-10-11  J.M. Piriou. Mean level CAPE if KLEVST < 0. 
!    2020-07-06  J.M. Piriou. Compute entrainment and ascent in the same Newton loop.
! --------------------------------------------------------------

!USE PARKIND1  ,ONLY : JPIM     ,JPRB  , JPRD
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK

USE YOMCAPE  , ONLY :  NCAPEITER, GMISCINV, LADAE, GCAPEMIN
USE constantes   , ONLY :  RTT      ,RDAY     ,RETV     ,&
 & RCW      ,REA      ,RD       ,RV       ,RCPD     ,RCPV     ,&
 & RCS      ,RLVTT    ,RLSTT    ,RBETS    ,RALPW    ,RBETW    ,&
 & RGAMW    ,RALPS    ,RGAMS    ,RALPD    ,RBETD    ,RGAMD    ,&
 & RG       ,RATM
!USE YOMTOPH, ONLY : TTOPH
!USE YOMLSFORC, ONLY : LMUSCLFA,NMUSCLFA

IMPLICIT NONE

!TYPE(TTOPH)       ,INTENT(IN)    :: YDTOPH
INTEGER(KIND=4),INTENT(IN)    :: KPROMA 
INTEGER(KIND=4),INTENT(IN)    :: KLEV 
INTEGER(KIND=4),INTENT(IN)    :: KST 
INTEGER(KIND=4),INTENT(IN)    :: KEND 
INTEGER(KIND=4),INTENT(IN)    :: KLEVST 
REAL(KIND=8)   ,INTENT(IN)    :: PENTRA
REAL(KIND=8)   ,INTENT(IN)    :: PMLDEP
REAL(KIND=8)   ,INTENT(IN)    :: PT(KPROMA,KLEV) 
REAL(KIND=8)   ,INTENT(IN)    :: PTE(KPROMA,KLEV) ! T environnement filtré (pour le LABO).
REAL(KIND=8)   ,INTENT(IN)    :: PRP(KPROMA,KLEV) 
REAL(KIND=8)   ,INTENT(IN)    :: PQV(KPROMA,KLEV) 
REAL(KIND=8)   ,INTENT(IN)    :: PQVE(KPROMA,KLEV) ! qv environnement filtré (pour le LABO).
REAL(KIND=8)   ,INTENT(OUT)   :: PCAPE(KPROMA) 
REAL(KIND=8)   ,INTENT(OUT)   :: PCIN(KPROMA) 
INTEGER(KIND=4)   ,INTENT(OUT)   :: KLCL(KPROMA)
INTEGER(KIND=4)   ,INTENT(OUT)   :: KFCL(KPROMA)
INTEGER(KIND=4)   ,INTENT(OUT)   :: KLNB(KPROMA)
INTEGER(KIND=4)      :: IPREVIOUS_NULL_ZRT(KPROMA)
INTEGER(KIND=4) :: JLEV,JLON,JIT
REAL(KIND=8) :: ZDLOG(KPROMA),ZBUOY,ZTV1,ZTV2,ZRT(KPROMA),ZDELARG,ZL,ZCP, &
 & ZFDERQS,ZDERL(KPROMA),ZRDLOG,ZZQV
REAL(KIND=8) :: ZQS(KPROMA),ZQSENV(KPROMA,KLEV),ZQV(KPROMA), ZQV1(KPROMA), &
 & ZT(KPROMA), ZT1(KPROMA), &
 & ZLOG(KPROMA), &
 & ZZT(KPROMA),ZDELTA(KPROMA)
REAL(KIND=8) :: ZDELARG0(KPROMA),ZL0(KPROMA)
REAL(KIND=8) :: ZDZ(KPROMA,KLEV), ZZFOEW(KPROMA)
REAL(KIND=8) :: ZMAXT,ZMINT,ZMINDERI,ZMINQ,ZMAXQ
REAL(KIND=8) :: ZTIN(KPROMA,KLEV) 
REAL(KIND=8) :: ZQVIN(KPROMA,KLEV) 
REAL(KIND=8) :: ZMEAN_THETA(KPROMA),ZMEAN_QV(KPROMA),ZDENO(KPROMA),ZMEAN_LEV(KPROMA)
REAL(KIND=8) :: ZBIN(KPROMA,KLEV)
INTEGER(KIND=4) :: ILEVST
REAL(KIND=8) :: ZMIX(KPROMA),ZAUGM

!REAL(KIND=8) :: ZHOOK_HANDLE
REAL(KIND=8) :: ZT_ASC(KPROMA,KLEV),ZQV_ASC(KPROMA,KLEV)
REAL(KIND=8) :: ZTRS(KPROMA),ZQVRS(KPROMA)
REAL(KIND=8) :: ZF,ZDERI,ZQVDRY,ZQVMOIST,ZENTRA,ZQSCL(KPROMA)
!---------

!  FUNCTIONS
!#include "wrscmr.intfb.h"
!#include "fctast.func.h"
!#include "fcttrm.func.h"
REAL(KIND=8) :: FOEW,FOQS,fth_folh,FODLEW,FODQS
!#include "fcttim.func.h"

!IF (LHOOK) CALL DR_HOOK('FPCINCAPE',0,ZHOOK_HANDLE)
!ASSOCIATE(NTCVIM=>YDTOPH%NTCVIM)
INTEGER(KIND=4) :: NTCVIM=1
!-------------------------------------------------
! INITIALIZE DEFAULT VALUES.
!-------------------------------------------------

PCIN(KST:KEND)=0.0_8
PCAPE(KST:KEND)=0.0_8
KLCL(KST:KEND)=-1
KFCL(KST:KEND)=-1
KLNB(KST:KEND)=-1

ZMINT=150._8
ZMAXT=400._8
ZMINDERI=1000._8
ZMINQ=1.E-07_8
ZMAXQ=1.0_8-ZMINQ
!
!-------------------------------------------------
! Initialize T and qv.
!-------------------------------------------------
!
DO JLEV=1,KLEV
  DO JLON=KST,KEND
    ZTIN(JLON,JLEV)=MAX(ZMINT,MIN(ZMAXT,PT(JLON,JLEV)))
    ZQVIN(JLON,JLEV)=MAX(ZMINQ,MIN(ZMAXQ,PQV(JLON,JLEV)))
    ZQSENV(JLON,JLEV)=FOQS(FOEW(ZTIN(JLON,JLEV),MAX(0.0_8,SIGN(1.0_8,RTT-ZTIN(JLON,JLEV))))/PRP(JLON,JLEV))  
    ZT_ASC(JLON,JLEV)=ZTIN(JLON,JLEV)
    ZQV_ASC(JLON,JLEV)=ZQVIN(JLON,JLEV)
  ENDDO
ENDDO
DO JLON=KST,KEND
  ! ZQSCL is saturation qv at condensation level (CL). 
  ! Below this CL it is initialized to a low value
  ! so that the cubic entrainment function saturates to 1.
  ZQSCL(JLON)=FOQS(FOEW(ZTIN(JLON,NTCVIM),MAX(0.0_8,SIGN(1.0_8,RTT-ZTIN(JLON,NTCVIM))))/PRP(JLON,NTCVIM))  
ENDDO

!
!-------------------------------------------------
! Starting level and starting parcel.
!-------------------------------------------------
!
ILEVST=ABS(KLEVST)
ZT (KST:KEND)=ZTIN (KST:KEND,ILEVST)
ZQV(KST:KEND)=ZQVIN(KST:KEND,ILEVST)

IPREVIOUS_NULL_ZRT(KST:KEND)=999999

ZBIN(:,:)=0._8 ! 1. inside a PMLDEP pressure-depth layer, 0. outside.
IF(KLEVST < 0) THEN
  ! If KLEVST < 0, a mean layer CAPE is computed: 
  ! ascent starts from a (T,qv) value, valid at level -KLEVST, obtained as the
  ! mean value of theta and qv over a layer, centered on -KLEVST,
  ! whose depth is PMLDEP (in Pa).
  !
  ! Compute mean theta and qv over the layers.
  ZMEAN_THETA(:)=0._8
  ZMEAN_LEV(:)=0._8
  ZMEAN_QV(:)=0._8
  ZDENO(:)=0._8
  DO JLEV=KLEV,NTCVIM+1,-1
    DO JLON=KST,KEND
      ZBIN(JLON,JLEV)=MAX(0._8,SIGN(1._8,PMLDEP+PRP(JLON,JLEV)-PRP(JLON,ILEVST)))&
       & *MAX(0._8,SIGN(1._8,PRP(JLON,ILEVST)-PRP(JLON,JLEV)))
      ZMEAN_THETA(JLON)=ZMEAN_THETA(JLON)+ZBIN(JLON,JLEV)*ZTIN(JLON,JLEV)*(RATM/PRP(JLON,JLEV))**(RD/RCPD)
      ZMEAN_QV(JLON)=ZMEAN_QV(JLON)+ZBIN(JLON,JLEV)*ZQVIN(JLON,JLEV)
      ZMEAN_LEV(JLON)=ZMEAN_LEV(JLON)+ZBIN(JLON,JLEV)*JLEV
      ZDENO(JLON)=ZDENO(JLON)+ZBIN(JLON,JLEV)
    ENDDO
  ENDDO
  DO JLON=KST,KEND
    ZMEAN_THETA(JLON)=ZMEAN_THETA(JLON)/MAX(1._8,ZDENO(JLON))
    ZMEAN_QV(JLON)=ZMEAN_QV(JLON)/MAX(1._8,ZDENO(JLON))
    ILEVST=MAX(1,MIN(KLEV,NINT(ZMEAN_LEV(JLON)/MAX(1._8,ZDENO(JLON)))))
    !
    ! Starting T and qv values.
    ZT(JLON)=ZMEAN_THETA(JLON)*(RATM/PRP(JLON,ILEVST))**(-RD/RCPD)
    ZQV(JLON)=ZMEAN_QV(JLON)
  ENDDO
ENDIF

DO JLEV=ILEVST,NTCVIM+1,-1
  DO JLON=KST,KEND
    !
    !-------------------------------------------------
    ! SATURATION SPECIFIC HUMIDITY ZQS TO DIAGNOSE WHETHER CONDENSATION IS REACHED OR NOT.
    !-------------------------------------------------
    !
    ZT(JLON)=MAX(ZMINT,MIN(ZMAXT,ZT(JLON)))
    ZQV(JLON)=MAX(ZMINQ,MIN(ZMAXQ,ZQV(JLON)))
    ZQS(JLON)=FOQS(FOEW(ZT(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZT(JLON))))/PRP(JLON,JLEV))  
    ZDELTA(JLON)=MAX(0._8,SIGN(1._8,ZQV(JLON)-ZQS(JLON)))
    ZDLOG(JLON)=LOG(PRP(JLON,MIN(ILEVST,JLEV+1))/PRP(JLON,JLEV)) * MAX(0,-SIGN(1,JLEV-ILEVST))  
    !
  ENDDO
  DO JLON=KST,KEND
    !
    !-------------------------------------------------
    ! IF THE PARCEL IS SUPERSATURATED, SUPERSATURATION IS REMOVED.
    ! THIS IS DONE THROUGH AN ISOBARIC TRANSFORMATION FROM (ZT,ZQV)
    ! TO (ZTRS,ZQVRS): RS=Remove Sursaturation.
    ! Solution for T
    ! f(T)=cp*(T-T0)+L*(q-q0)=0
    ! with constraint q=qs(T,p0),
    ! solved by the method of Newton, iteration T --> T-f(T)/f'(T), with starting point ZT.
    !-------------------------------------------------
    !
    ZTRS(JLON)=ZT(JLON)
    !
    !-------------------------------------------------
    ! LATENT HEAT
    !-------------------------------------------------
    !
    ZDELARG0(JLON)=MAX(0.0_8,SIGN(1.0_8,RTT-ZT(JLON)))
    ZL0(JLON)=fth_folh(ZT(JLON),ZDELARG0(JLON))
    ZDERL(JLON)=-RV*(RGAMW+ZDELARG0(JLON)*RGAMD)
  ENDDO ! JLON
  !
  !-------------------------------------------------
  ! Newton's loop to solve the sursaturation.
  !-------------------------------------------------
  !
  DO JIT=1,NCAPEITER
    DO JLON=KST,KEND
      ZZFOEW(JLON)=FOEW(ZTRS(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZTRS(JLON))))
    ENDDO
    DO JLON=KST,KEND
      !
      ! SATURATION SPECIFIC HUMIDITY ZQVRS.
      ZQVRS(JLON)=FOQS(ZZFOEW(JLON)/PRP(JLON,JLEV))  
      ZCP=RCPD*(1.0_8-ZQVRS(JLON))+RCPV*ZQVRS(JLON) 
      ZFDERQS=FODQS(ZQVRS(JLON),ZZFOEW(JLON)/PRP(JLON,JLEV)&
        & ,FODLEW(ZTRS(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZTRS(JLON)))))
      ZF=ZCP*(ZTRS(JLON)-ZT(JLON))+ZL0(JLON)*(ZQVRS(JLON)-ZQV(JLON))
      ZDERI=ZCP+((RCPV-RCPD)*(ZTRS(JLON)-ZT(JLON))+ZL0(JLON))*ZFDERQS+ZDERL(JLON)*(ZQVRS(JLON)-ZQV(JLON))
      ZTRS(JLON)=MAX(ZMINT,MIN(ZMAXT,ZTRS(JLON)-ZF/MAX(ZMINDERI,ZDERI)))
    ENDDO ! JLON
  ENDDO ! JIT
  DO JLON=KST,KEND
    !
    ! Choose RS (Remove Saturation) point or original point, depending on saturation ZDELTA.
    ZT(JLON)=ZDELTA(JLON)*ZTRS(JLON)+(1._8-ZDELTA(JLON))*ZT(JLON)
    ZQV(JLON)=ZDELTA(JLON)*ZQVRS(JLON)+(1._8-ZDELTA(JLON))*ZQV(JLON)
    !
    ! ZT_ASC and ZQV_ASC are stored as profile arrays for MUSC diagnostics only.
    ZT_ASC(JLON,JLEV)=ZT(JLON)
    ZQV_ASC(JLON,JLEV)=ZQV(JLON)
    !
    !-------------------------------------------------
    ! BUOYANCY.
    !-------------------------------------------------
    !
    ZTV1=ZT(JLON)*(1._8+RETV*ZQV(JLON)) ! Tv ascent.
    ZTV2=PTE(JLON,JLEV)*(1._8+RETV*PQVE(JLON,JLEV)) ! Tv environment.
    ZBUOY=RG*(ZTV1/ZTV2-1._8)
    !
    !-------------------------------------------------
    ! CIN AND CAPE INTEGRALS.
    !-------------------------------------------------
    !
    ZRT(JLON)=ZBUOY/RG*(RD+(RV-RD)*ZQVIN(JLON,JLEV))*ZTIN(JLON,JLEV)*ZDLOG(JLON)
    !------------------------------------------
    ! CUMULATE CAPE IF POSITIVE CONTRIBUTION AND SATURATION.
    !------------------------------------------
    PCAPE(JLON)=PCAPE(JLON)+MAX(0.0_8,ZRT(JLON))*MAX(0._8,SIGN(1._8,ZDELTA(JLON)-0.5_8))
    !------------------------------------------
    ! CUMULATE CIN IF NEGATIVE CONTRIBUTION AND BELOW LFC.
    !------------------------------------------
    IF(PCAPE(JLON) < GCAPEMIN) PCIN(JLON)=PCIN(JLON)+MIN(0.0_8,ZRT(JLON))
  ENDDO

  DO JLON=KST,KEND
    IF (ZDELTA(JLON) > 0.5_8 .AND. KLCL(JLON)==-1) THEN
      ! Parcel is saturated. LCL found.
      KLCL(JLON)=JLEV
      ZQSCL(JLON)=FOQS(FOEW(ZTIN(JLON,JLEV),MAX(0.0_8,SIGN(1.0_8,RTT-ZTIN(JLON,JLEV))))/PRP(JLON,JLEV))  
    ENDIF
    IF (PCAPE(JLON) > 0.0_8 .AND. KFCL(JLON)==-1) THEN
      ! Positive CAPE. FCL found.
      KFCL(JLON)=JLEV
    ENDIF
    IF (ZRT(JLON) <= 0.0_8 .AND. JLEV<KFCL(JLON) .AND. JLEV<IPREVIOUS_NULL_ZRT(JLON)-1) THEN
      ! Negative buoyancy. LNB found.
      KLNB(JLON)=JLEV
    ENDIF
    IF (ZRT(JLON) <= 0.0_8) THEN
      IPREVIOUS_NULL_ZRT(JLON)=JLEV
    ENDIF
    !
    !-------------------------------------------------
    ! MOIST OR DRY ASCENT FROM JLEV TO JLEV-1.
    ! TRANSFORMATION FROM (ZT1,ZQV1) TO (ZZT,ZZQV).
    !      Solution for T
    !             f(T)=cp*(T-T0)+delta*L*(q-q0)+phi-phi0=0
    !      either f(T)=cp*(T-T0)+delta*L*(q-q0)-R*T*log(p/p0)=0
    !            with constraint q=qs(T,p), knowing that q0=qs(T0,p0)
    !       it is solved by the method of Newton, iteration
    !       T --> T-f(T)/f'(T), with starting point ZT1.
    !-------------------------------------------------
    !
    ZT1(JLON)=ZT(JLON)
    ZQV1(JLON)=ZQV(JLON)
    ZZT(JLON)=ZT1(JLON)
    ZLOG(JLON)=LOG(PRP(JLON,JLEV-1)/PRP(JLON,JLEV))
  ENDDO
  DO JIT=1,NCAPEITER
    DO JLON=KST,KEND
      ZZFOEW(JLON)=FOEW(ZZT(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZZT(JLON))))
    ENDDO
    DO JLON=KST,KEND
      !
      !-------------------------------------------------
      ! SATURATION SPECIFIC HUMIDITY
      !-------------------------------------------------
      !
      ZZQV=FOQS(ZZFOEW(JLON)/PRP(JLON,JLEV-1))  
      !
      !-------------------------------------------------
      ! LATENT HEAT
      !-------------------------------------------------
      !
      ZDELARG=MAX(0.0_8,SIGN(1.0_8,RTT-ZT1(JLON)))
      ZL=fth_folh(ZZT(JLON),ZDELARG)  
      ZDERL(JLON)=-RV*(RGAMW+ZDELARG*RGAMD)
      IF(LADAE) THEN
        !-------------------------------------------------
        ! Entrainment is larger in low relative humidity and large saturation specific humidity.
        ! This entrainment formula follows the Tiedtke-Bechtold convection scheme.
        !-------------------------------------------------
        ZENTRA=(1._8-ZBIN(JLON,JLEV))*PENTRA*(1.3_8-MIN(1._8,ZQVIN(JLON,JLEV)/ZQSENV(JLON,JLEV)))&
          & *MIN(1._8,ZQSENV(JLON,JLEV)/ZQSCL(JLON))**3
      ELSE
        !-------------------------------------------------
        ! Uniform entrainment.
        !-------------------------------------------------
        ZENTRA=(1._8-ZBIN(JLON,JLEV))*PENTRA
      ENDIF
      !
      !-------------------------------------------------
      ! Newton's loop to solve the moist or dry adiabatic ascent and entrainment.
      !-------------------------------------------------
      !
      ZRDLOG=(RD+(RV-RD)*ZZQV)*ZLOG(JLON)
      ZCP=RCPD*(1.0_8-ZZQV)+RCPV*ZZQV 
      ZFDERQS=FODQS(ZZQV,ZZFOEW(JLON)/PRP(JLON,JLEV-1)&
        & ,FODLEW(ZZT(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZZT(JLON)))))
      ! ZDZ: layer depth in meter.
      ZDZ(JLON,JLEV)=(PRP(JLON,JLEV)-PRP(JLON,JLEV-1))/(0.5*(PRP(JLON,JLEV)+ &
        & PRP(JLON,JLEV-1)))*(RD+(RV-RD)*(0.5*(ZQVIN(JLON,JLEV)+ &
        & (ZQVIN(JLON,JLEV-1)))))*0.5*(ZTIN(JLON,JLEV)+(ZTIN(JLON,JLEV-1)))/RG
      ZMIX(JLON)=MIN(1._8,ZENTRA*ZDZ(JLON,JLEV))
      ZAUGM=1._8+ZMIX(JLON)
      ZF=ZCP*(ZZT(JLON)*ZAUGM-ZT1(JLON)-ZMIX(JLON)*PT(JLON,JLEV))&
       & +ZDELTA(JLON)*ZL*(ZZQV*ZAUGM-ZQV1(JLON)-ZMIX(JLON)*PQV(JLON,JLEV))-ZZT(JLON)*ZRDLOG
      ZDERI=ZCP*ZAUGM+ZDELTA(JLON)*((RCPV-RCPD)*(ZZT(JLON)*ZAUGM-ZT1(JLON)-ZMIX(JLON)*PT(JLON,JLEV))&
       & +ZL*ZAUGM-(RV-RD)*ZZT(JLON)*ZLOG(JLON))*ZFDERQS&
       & +(ZZQV*ZAUGM-ZQV1(JLON)-ZMIX(JLON)*PQV(JLON,JLEV))*ZDELTA(JLON)*ZDERL(JLON)-ZRDLOG
      ZZT(JLON)=MAX(ZMINT,MIN(ZMAXT,ZZT(JLON)-ZF/MAX(ZMINDERI,ZDERI)))
    ENDDO ! JLON
  ENDDO ! JIT
  
  DO JLON=KST,KEND
    !
    !-------------------------------------------------
    ! UPDATE PARCEL STATE. T in moist or dry mode.
    !-------------------------------------------------
    !
    ZT(JLON)=ZZT(JLON)
    !
    !-------------------------------------------------
    ! UPDATE PARCEL STATE. qv in moist and dry modes.
    !-------------------------------------------------
    !
    ZQVMOIST=FOQS(FOEW(ZT(JLON),MAX(0.0_8,SIGN(1.0_8,RTT-ZT(JLON))))/PRP(JLON,JLEV-1))
    ZQVDRY=ZQV1(JLON)+ZMIX(JLON)*(ZQVIN(JLON,JLEV)-ZQV1(JLON))
    !
    !-------------------------------------------------
    !  VALUES FROM DRY OR MOIST ASCENT MODES ARE CHOSEN 
    !-------------------------------------------------
    !
    ZQV(JLON)=ZDELTA(JLON)*ZQVMOIST+(1._8-ZDELTA(JLON))*ZQVDRY
  ENDDO ! JLON
ENDDO ! JLEV

! If no LFC (Level of Free Convection) found, set CIN to a positive value, to plot it as missing data.
DO JLON=KST,KEND
  IF(PCAPE(JLON) < GCAPEMIN) PCIN(JLON)=GMISCINV
ENDDO

!IF(LMUSCLFA) THEN
!  CALL WRSCMR(NMUSCLFA,'T_ASC',ZT_ASC,KPROMA,KLEV)
!  CALL WRSCMR(NMUSCLFA,'QV_ASC',ZQV_ASC,KPROMA,KLEV)
!ENDIF

!END ASSOCIATE
!IF (LHOOK) CALL DR_HOOK('FPCINCAPE',1,ZHOOK_HANDLE)
END SUBROUTINE FPCINCAPE
subroutine fth_cin_cape(klev,klevsommet,pt,pqv,pp,pte,pqve,pentr,pcape,pcin,klcl,klfc,klnb)
! --------------------------------------------------------------
! //// *fth_cin_cape* Compute CIN, CAPE, and vertical location of clouds.
! --------------------------------------------------------------
! Subject:
! Method:
!  Ascents will be computed starting from each level of the input profile.
!  The parcel is raised from its level of origin LO to the level of condensation LC,
!  then to its level of free convection LFC (the parcel becomes buoyant),
!  then to the level of neutral buoyancy LNB, where the the parcel becomes unbuoyant.
! Externals:
! Auteur/author:   2021-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! In input:
! ----------
!  INTEGER: klev: number of levels.
!  INTEGER: klevsommet: les particules de départ sont recherchées de klevsommet à klev.
!  REAL: pt: temperature (K).
!  REAL: pte: température de l'environnement (K). Sauf besoin de filtrage (LABO), on peut donner en entrée pte le même tableau que pour  pt.
!  REAL: pqv: specific humidity (no dim).
!  REAL: pqve: specific humidity (no dim) de l'environnement. Sauf besoin de filtrage (LABO), on peut donner en entrée pqve le même tableau que pour  pqv.
!  REAL: pp: pressure (Pa).
!    WARNING: pressure values are supposed to icrease from pp(1) to pp(klev).
!  REAL pentr: vertical entrainment coefficient
!       (in m**-1, useful range: between 0. (no entrainment) and 3.E-03 m**-1).
!
! In output:
! -----------
!  REAL: pcin (J/kg): CIN, Convection INhibition,
!         massic energy to raise the parcel from
!         LO to LC then to LFC (see above).
!         Only negative terms are cumulated.
!  REAL: pcape (J/kg): CAPE, Convection Available Potential Energy,
!         massic energy  provided by the raise of the parcel
!         from LFC to LNB (see above).
!         Only positive terms are cumulated.
!  INTEGER: klcl(jlev) LCL of the parcel raised from jlev (level number).
!  INTEGER: klfc(jlev) LFC of the parcel raised from jlev (level number).
!  INTEGER: klnb(jlev) LNB of the parcel raised from jlev (level number).
! --------------------------------------------------------------
!
!use parkind1, only : jpim, jprb
implicit none
!
integer(kind=4), intent(in) :: klev
integer(kind=4), intent(in) :: klevsommet
real(kind=8), intent(in) :: pt(klev),pte(klev)
real(kind=8), intent(in) :: pqv(klev),pqve(klev)
real(kind=8), intent(in) :: pp(klev)
real(kind=8), intent(in) :: pentr

real(kind=8), intent(out) :: pcin(klev)
real(kind=8), intent(out) :: pcape(klev)
integer(kind=4), intent(out) :: klcl(klev), klfc(klev), klnb(klev)

integer(kind=4) :: jlev
real(kind=8) :: zmldep
integer(kind=4) :: ist,iend,iproma
!
!-------------------------------------------------
! Default initializations.
!-------------------------------------------------
!
!-------------------------------------------------
! Initialize to zero.
!-------------------------------------------------
!
!
!-------------------------------------------------
! pcin: CIN: <= 0.: vertical integral from surf. to current level
!            of the buoyancy force, where <= 0.
!-------------------------------------------------
!
pcin=0.0_8
!
!-------------------------------------------------
! pcape: CAPE: >= 0.: vertical integral from surf. to current level
!            of the buoyancy force, where >= 0.
!-------------------------------------------------
!
pcape=0.0_8
!
!-------------------------------------------------
! Integer levels.
!-------------------------------------------------
!
klcl=0 ; klfc=0 ; klnb=0
ist=1
iend=1
iproma=1
zmldep=10000._8 ! mean layer depth for MLCAPE computation.
do jlev=klevsommet,klev
  call FPCINCAPE(ist,iend,iproma,KLEV,jlev,PENTR,zmldep,PT,pp,PQV,PTE,PQVE,PCAPE(jlev),PCIN(jlev),klcl(jlev),klfc(jlev),KLNB(jlev))
enddo
end
