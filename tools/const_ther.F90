MODULE const_ther
! --------------------------------------------------------------
! **** const_ther physiques.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Auteur/author:   2001-01, J.M. Piriou d'après ARPEGE.
! Modifications:
! --------------------------------------------------------------
!USE PARKIND1  ,ONLY : JPIM     ,JPRB
implicit none
save
!
!-------------------------------------------------
! Nombre d'itérations de la boucle de Newton.
!-------------------------------------------------
!
INTEGER(KIND=4), parameter :: nbiter=2
!      -----------------------------------------------------------------
!
!*       1.    DEFINE FUNDAMENTAL CONSTANTS.
!              -----------------------------
!
REAL(KIND=8), parameter :: rpi=3.14159265358979 ! pi.
REAL(KIND=8), parameter :: rclum=299792458. ! célérité de la lumière.
REAL(KIND=8), parameter :: rhpla=6.6260755e-34 ! cte de Planck.
REAL(KIND=8), parameter :: rkbol=1.380658e-23 ! cte de Bolzman.
REAL(KIND=8), parameter :: rnavo=6.0221367e+23 ! nombre d'Avogadro.
!
!     ------------------------------------------------------------------
!
!*       2.    DEFINE ASTRONOMICAL CONSTANTS.
!              ------------------------------
!
REAL(KIND=8), parameter :: rday=86400. ! jour solaire.
REAL(KIND=8), parameter :: rea=149597870000. ! demi-grand axe de rév. terrestre.
REAL(KIND=8), parameter :: rsiyea=365.25*rday*2.*rpi/6.283076 ! année sidérale.
REAL(KIND=8), parameter :: rsiday=rday/(1._8+rday/rsiyea) ! jour sidéral.
REAL(KIND=8), parameter :: romega=2.*rpi/rsiday ! vitesse angulaire terrestre.
!
! ------------------------------------------------------------------
!
! *       3.    DEFINE GEOIDE.
! --------------
!
REAL(KIND=8), parameter :: rg=9.80665 ! accélération de la pesanteur.
REAL(KIND=8), parameter :: ra=6371229. ! rayon terrestre.
!
! ------------------------------------------------------------------
!
! *       4.    DEFINE RADIATION CONSTANTS.
! ---------------------------
!
REAL(KIND=8), parameter :: rsigma=2. * rpi**5 * rkbol**4 /(15.* rclum**2 * rhpla**3) ! cte de Stefan-Bolzman.
!real, parameter :: rsigma=5.670509E-08
REAL(KIND=8), parameter :: ri0=1370. ! cte solaire.
!
! ------------------------------------------------------------------
!
! *       5.    DEFINE THERMODYNAMIC CONSTANTS, GAS PHASE.
! ------------------------------------------
!
REAL(KIND=8), parameter :: r=rnavo*rkbol ! cte des gaz parfaits.
REAL(KIND=8), parameter :: rmd=28.9644
REAL(KIND=8), parameter :: rmv=18.0153
REAL(KIND=8), parameter :: rmo3=47.9942
REAL(KIND=8), parameter :: rd=1000.*r/rmd ! cte spécifique de l'air sec.
REAL(KIND=8), parameter :: rv=1000.*r/rmv ! cte spécifique de la vapeur d'eau.
REAL(KIND=8), parameter :: rcpd=3.5*rd ! chaleur massique de l'air sec.
REAL(KIND=8), parameter :: rcvd=rcpd-rd
REAL(KIND=8), parameter :: rcpv=4. *rv ! chaleur massique de la vapeur d'eau.
REAL(KIND=8), parameter :: rcvv=rcpv-rv
REAL(KIND=8), parameter :: rkappa=rd/rcpd
REAL(KIND=8), parameter :: retv=rv/rd-1._8
!
REAL(KIND=8), parameter :: ralpw =  .6022274788e+02
REAL(KIND=8), parameter :: rbetw =  .6822400210e+04
REAL(KIND=8), parameter :: rgamw =  .5139266694e+01
!
REAL(KIND=8), parameter :: ralps =  .3262117981e+02
REAL(KIND=8), parameter :: rbets =  .6295421339e+04
REAL(KIND=8), parameter :: rgams =  .5631331575e+00
!
REAL(KIND=8), parameter :: ralpd = -.2760156808e+02
REAL(KIND=8), parameter :: rbetd = -.5269788712e+03
REAL(KIND=8), parameter :: rgamd = -.4576133537e+01
! ------------------------------------------------------------------
!
! *       6.    DEFINE THERMODYNAMIC CONSTANTS, LIQUID PHASE.
! ---------------------------------------------
!
REAL(KIND=8), parameter :: rcw=4218. ! chaleur massique de l'eau liquide.
!
! ------------------------------------------------------------------
!
! *       7.    DEFINE THERMODYNAMIC CONSTANTS, SOLID PHASE.
! --------------------------------------------
!
REAL(KIND=8), parameter :: rcs=2106. ! chaleur massique de la glace.
!
! ------------------------------------------------------------------
!
! *       8.    DEFINE THERMODYNAMIC CONSTANTS, TRANSITION OF PHASE.
! ----------------------------------------------------
!
REAL(KIND=8), parameter :: rtt=273.16 ! point triple de l'eau.
REAL(KIND=8), parameter :: rdt=11.82
REAL(KIND=8), parameter :: rlvtt=2.5008e+6 ! chaleur latente eau vapeur > eau liquide.
REAL(KIND=8), parameter :: rlstt=2.8345e+6 ! chaleur latente eau vapeur > eau glace.
REAL(KIND=8), parameter :: rlvzer=rlvtt+rtt*(rcw-rcpv) ! chaleur latente de fusion à 0°K!
REAL(KIND=8), parameter :: rlszer=rlstt+rtt*(rcs-rcpv) ! chaleur latente de sublimation à 0°K!
REAL(KIND=8), parameter :: rlmlt=rlstt-rlvtt ! chaleur latente eau liquide > eau glace.
REAL(KIND=8), parameter :: ratm=100000. ! pression standard.
!
! ------------------------------------------------------------------
!
! *       9.    SATURATED VAPOUR PRESSURE.
! --------------------------
!
REAL(KIND=8), parameter :: restt=611.14
!
!-------------------------------------------------
! Constante de Joule.
!-------------------------------------------------
!
REAL(KIND=8), parameter :: rjoule=4.184

end
