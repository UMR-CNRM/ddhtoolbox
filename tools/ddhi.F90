#include"fonctions.F90"
!OPTIONS NODOUBLE
program ddhi
! --------------------------------------------------------------------------
! **** *DDHI*  Interpretation de donnees DDH.
! --------------------------------------------------------------------------
! Sujet:    Conversion d'unites pour les articles de DDH
! selon le contenu du fichier clficc afin de rendre
! l'interpretation plus aisee.
! Calcul des moyenne, ecart-type, etc... des champs convertis.
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! Methode:
! --------
! Externes: /
! ---------
! Auteur:           92-02, J.M. Piriou.
! -------
! Modifications:    94-10, J.M. Piriou: lecture d'un lfa au lieu d'un LFP.
!                 2007-11, A. Deckmyn : make lisc (list of fields) external.
!                 2008-07, J.M. Piriou: translate comments to English.
!                 2010-01, J.M. Piriou: plot vertical profiles of theta, thetaW, etc.
!                 2021-03, J.M. Piriou: set initial pressure and geopotential values to final ones, if the former are equal to 0..
!                 2023-10-08, J.M. Piriou: introduce the "PTS" time coordinate.
! --------------------------------------------------------------------------
use const_ther, only : rcpd, rcpv, rcw, rcs
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)

#include"ddhpar.h"
CHARACTER*200 :: CL1
CHARACTER*200 :: CL3
CHARACTER*200 :: CLCHAL
CHARACTER*200 :: CLCONV
CHARACTER*200 :: CLECHEA
CHARACTER*200 :: CLECT
CHARACTER*200 :: CLFICC
CHARACTER*200 :: CLFICD
CHARACTER*200 :: CLFICM
CHARACTER*200 :: CLFICS
CHARACTER*200 :: CLLEG
CHARACTER*200 :: CLLOC
CHARACTER*200 :: CLMAX
CHARACTER*200 :: CLMIN
CHARACTER*200 :: CLMOY
CHARACTER*200 :: CLNOMA
CHARACTER*200 :: CLNOMAFS
CHARACTER*200 :: CLOPT
CHARACTER*200 :: CLOUTFD
CHARACTER*200 :: CLOUTNO

CHARACTER*200 :: CLOUTTV
CHARACTER*200 :: CLRCM
CHARACTER*200 :: CLSOL
CHARACTER*200 :: CLSOR
CHARACTER*200 :: CLSORD
CHARACTER*200 :: CLSYST
CHARACTER*200 :: CLTYPE
CHARACTER*200 :: CLUNIT
CHARACTER*200 :: CLZE
CHARACTER*200 :: CLZUE
INTEGER(KIND=4) :: IAMQ
INTEGER(KIND=4) :: IB
INTEGER(KIND=4) :: IBARRE
INTEGER(KIND=4) :: ICOMM
INTEGER(KIND=4) :: ICOO
INTEGER(KIND=4) :: IDATEREF
INTEGER(KIND=4) :: idom(1)
INTEGER(KIND=4) :: IDOML
INTEGER(KIND=4) :: IECART
INTEGER(KIND=4) :: IECHLOG
INTEGER(KIND=4) :: IECR
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: IFINL
INTEGER(KIND=4) :: IGOL
INTEGER(KIND=4) :: IINTX
INTEGER(KIND=4) :: ILISS
INTEGER(KIND=4) :: IINTY
INTEGER(KIND=4) :: IL3
INTEGER(KIND=4) :: ILART
INTEGER(KIND=4) :: ILCOO
INTEGER(KIND=4) :: ILCOO1
INTEGER(KIND=4) :: ILCOO2
INTEGER(KIND=4) :: ILECHEA
INTEGER(KIND=4) :: ILECT
INTEGER(KIND=4) :: ilev(1)
INTEGER(KIND=4) :: ILEV2
INTEGER(KIND=4) :: ILEVM
INTEGER(KIND=4) :: ILEVM2
INTEGER(KIND=4) :: ILFICD
INTEGER(KIND=4) :: ILFICS
INTEGER(KIND=4) :: ILISCONV
INTEGER(KIND=4) :: ILISLC
INTEGER(KIND=4) :: ILLEG
INTEGER(KIND=4) :: ILMAX
INTEGER(KIND=4) :: ILMIN
INTEGER(KIND=4) :: ILMOY
INTEGER(KIND=4) :: ILNAMX
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: ILONGF
INTEGER(KIND=4) :: ILONGV
INTEGER(KIND=4) :: ILOPT
INTEGER(KIND=4) :: ILRCM
INTEGER(KIND=4) :: ILSOR
INTEGER(KIND=4) :: ILUNIT
INTEGER(KIND=4) :: ILZE
INTEGER(KIND=4) :: IMAXF
INTEGER(KIND=4) :: IMAXV
INTEGER(KIND=4) :: IMOD
INTEGER(KIND=4) :: IMODV
INTEGER(KIND=4) :: INOMVS
INTEGER(KIND=4) :: IOKDOC
INTEGER(KIND=4) :: IOKLISCONV
INTEGER(KIND=4) :: IOKLISLC
INTEGER(KIND=4) :: IPOS
INTEGER(KIND=4) :: IREP
INTEGER(KIND=4) :: IRESUL
INTEGER(KIND=4) :: ISSSSS
INTEGER(KIND=4) :: ITAILLE
INTEGER(KIND=4) :: itype(1)
INTEGER(KIND=4) :: ITYPMOY
INTEGER(KIND=4) :: IUL1
INTEGER(KIND=4) :: IUL3
INTEGER(KIND=4) :: IULB
INTEGER(KIND=4) :: IULC
INTEGER(KIND=4) :: IULENT
INTEGER(KIND=4) :: IULM
INTEGER(KIND=4) :: IULSOR
INTEGER(KIND=4) :: IULTIT
INTEGER(KIND=4) :: JARG
INTEGER(KIND=4) :: JART
INTEGER(KIND=4) :: JB
INTEGER(KIND=4) :: JCHAMP
INTEGER(KIND=4) :: JCOO
INTEGER(KIND=4) :: JDOC
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: JLEV
INTEGER(KIND=4) :: JLIS
INTEGER(KIND=4) :: JLISCONV
INTEGER(KIND=4) :: JLISLC
INTEGER(KIND=4) :: JPCOO
INTEGER(KIND=4) :: JVAR
LOGICAL :: LL1N
LOGICAL :: LLAQUA
LOGICAL :: LLBIL
LOGICAL :: LLCOODEF
LOGICAL :: LLDEBUG
LOGICAL :: LLELEV
LOGICAL :: LLEX
LOGICAL :: LLLIS
LOGICAL :: LLPS
LOGICAL :: LLSORFDTA
LOGICAL :: LLVCT0
LOGICAL :: LLCP
REAL(KIND=8) :: ZCMU
REAL(KIND=8) :: ZCOEC
REAL(KIND=8) :: ZCOEFDDHI
REAL(KIND=8) :: zadd_ddhi
REAL(KIND=8) :: ZCOEFMU
REAL(KIND=8) :: ZECART(1)
REAL(KIND=8) :: ZECH(1)
REAL(KIND=8) :: ZECHDV(1)
REAL(KIND=8) :: ZECHELLE
REAL(KIND=8) :: ZECHO
REAL(KIND=8) :: ZECT
REAL(KIND=8) :: ZG
REAL(KIND=8) :: ZMAX
REAL(KIND=8) :: ZMIN
REAL(KIND=8) :: ZMOY
REAL(KIND=8) :: ZNORP
REAL(KIND=8) :: ZNORZ
REAL(KIND=8) :: ZPOI
REAL(KIND=8) :: ZRCM
REAL(KIND=8) :: ZRD
REAL(KIND=8) :: ZSOMMAS
REAL(KIND=8) :: ZSOMX
REAL(KIND=8) :: ZSOMX2
REAL(KIND=8) :: ZVALSOM
REAL(KIND=8) :: ZVC
REAL(KIND=8) :: ZX
REAL(KIND=8) :: ZEPSQV 
REAL(KIND=8) :: ZEPSQL
REAL(KIND=8) :: ZEPSQR
REAL(KIND=8) :: ZEPSQS
REAL(KIND=8) :: ZEPSQI
REAL(KIND=8) :: ZEPSQG

parameter(jpcoo=3)
!
character*240 clarg(jparg)
character *03 clconvu
character *03 clconvs
character *200 clnamx
character *200 clint
character *139 clnomat ! 139: nombre de caracteres des articles de la liste de
character *16 cladcou
character *01 clfctct ! champ à tracer ou non.
character *01 clfct2c ! fonctions de deux champs.
character *01 clfcttv ! traitement de la verticale.
character *01 clfctno ! normalisation.
character *13 clmnemo ! mnémonique du champ.
character *13 clnom1 ! champ1 des fcts de deux champs.
character *13 clnom2 ! champ2 des fcts de deux champs.
character *60 cllegfi ! légende finale à afficher.
character *15 clunite ! unité de la grandeur finale.
character *13 cllislc(jpnomct) ! lignes de la liste des champs utilisateur.
character *200 cllislcc(jpnomct) ! titre du champ fourni par l'utilisateur.
character *139 cllisconv(jpnomct)
character *100 cliscfile ! file with all field descriptions.
character *200 clart(jpnomct)
character*1 clbif
character*1 clut ! unité de temps (J pour jours, A pour années).
!
INTEGER(KIND=4) idocfi(17)
INTEGER(KIND=4) idatef(11)
integer*4 system ! pour l'appel vers le systeme UNIX.
REAL(KIND=8) zdocd(jpdoc)
REAL(KIND=8) zdocdf(jpdoc,jpdom) ! articles de documentation des domaines (tableau tota
!
REAL(KIND=8) zminmme(6)
!
REAL(KIND=8) zmoyz1(jpprod)
REAL(KIND=8) zmoyz2(jpprod)
REAL(KIND=8) zmoyz3(jpprod)
!
REAL(KIND=8) zpre0(jpprod) ! champ vpp0 unidim.
REAL(KIND=8) zpre1(jpprod) ! champ vpp1 unidim.
REAL(KIND=8) zprec(jpprod) ! champ ppp unidim.
!
REAL(KIND=8) zvct0u(jpprod) ! champ vct0 unidim.
REAL(KIND=8) zvct1u(jpprod) ! champ vct1 unidim.
REAL(KIND=8) zvct0(jplev,jpdom) ! champ vct0 bidim.
REAL(KIND=8) zvct1(jplev,jpdom) ! champ vct1 bidim.
!
REAL(KIND=8) zcoop0v(jplev,jpdom) ! coordonnee verticale masse initiale niveau variables.
REAL(KIND=8) zcoop1v(jplev,jpdom) ! coordonnee verticale masse finale niveau variables.
REAL(KIND=8) zcoopcv(jplev,jpdom) ! coordonnee verticale masse cumulee niveau variables
REAL(KIND=8) zcoop0f(jplev2,jpdom) ! coordonnee verticale masse initiale niveau flux.
REAL(KIND=8) zcoop1f(jplev2,jpdom) ! coordonnee verticale masse finale niveau flux.
REAL(KIND=8) zcoopcf(jplev2,jpdom) ! coordonnee verticale masse cumulee niveau flux.
!
REAL(KIND=8) zcoop0vu(jpprod) ! id. zcoop0v sauf unidim.
REAL(KIND=8) zcoop1vu(jpprod) ! id. zcoop1v sauf unidim.
REAL(KIND=8) zcoopcvu(jpprod) ! id. zcoopcv sauf unidim.
REAL(KIND=8) zcoop0fu(jpprod) ! id. zcoop0f sauf unidim.
REAL(KIND=8) zcoop1fu(jpprod) ! id. zcoop1f sauf unidim.
REAL(KIND=8) zcoopcfu(jpprod) ! id. zcoopcf sauf unidim.
!
REAL(KIND=8) zz0(jpprod) ! champ z0 unidim.
REAL(KIND=8) zz1(jpprod) ! champ z1 unidim.
!
REAL(KIND=8) zrcp0(jpprod) ! champ rcp0 unidim.
REAL(KIND=8) zrcp1(jpprod) ! champ rcp1 unidim.

REAL(KIND=8) zvqv0u(jpprod) ! champ vqv0 bidim.
REAL(KIND=8) zvqv1u(jpprod) ! champ vqv1 bidim.
REAL(KIND=8) zvql0u(jpprod) ! champ vql0 bidim.
REAL(KIND=8) zvql1u(jpprod) ! champ vql1 bidim.
REAL(KIND=8) zvqr0u(jpprod) ! champ vqr0 bidim.
REAL(KIND=8) zvqr1u(jpprod) ! champ vqr1 bidim.
REAL(KIND=8) zvqs0u(jpprod) ! champ vqs0 bidim.
REAL(KIND=8) zvqs1u(jpprod) ! champ vqs1 bidim.
REAL(KIND=8) zvqi0u(jpprod) ! champ vqi0 bidim.
REAL(KIND=8) zvqi1u(jpprod) ! champ vqi1 bidim.
REAL(KIND=8) zvqg0u(jpprod) ! champ vqg0 bidim.
REAL(KIND=8) zvqg1u(jpprod) ! champ vqg1 bidim.
!
!-------------------------------------------------
! On définit un champ altitude de la surface.
! Ce champ peut être différent au début et à la fin
! de la prévision, non parce que l'orographie
! évolue (!) mais parce que dans le fichier
! DDH n'est pas porté l'orographie de surface
! mais le géopotentiel atmosphérique.
! On est donc obligé d'estimer l'orographie
! de surface à partir du géopotentiel du plus bas niveau,
! et cette estimation dépend de T, donc de l'échéance de prévision.
!-------------------------------------------------
!
REAL(KIND=8) zzsurf0(jpdom) ! altitude initiale de la surface.
REAL(KIND=8) zzsurf1(jpdom) ! altitude finale de la surface.
REAL(KIND=8) zzsurfc(jpdom) ! altitude moyenne de la surface.
!
REAL(KIND=8) zpsurf0(jpdom) ! pression initiale de la surface.
REAL(KIND=8) zpsurf1(jpdom) ! pression finale de la surface.
REAL(KIND=8) zpsurfc(jpdom) ! pression moyenne de la surface.
!
REAL(KIND=8) zcooz0v(jplev,jpdom) ! coordonnee verticale z initiale niveau variables.
REAL(KIND=8) zcooz1v(jplev,jpdom) ! coordonnee verticale z finale niveau variables.
REAL(KIND=8) zcoozcv(jplev,jpdom) ! coordonnee verticale z cumulee niveau variables.
REAL(KIND=8) zcooz0f(jplev2,jpdom) ! coordonnee verticale z initiale niveau flux.
REAL(KIND=8) zcooz1f(jplev2,jpdom) ! coordonnee verticale z finale niveau flux.
REAL(KIND=8) zcoozcf(jplev2,jpdom) ! coordonnee verticale z cumulee niveau flux.
!
REAL(KIND=8) zcooz0vu(jpprod) ! id. zcooz0v sauf unidim.
REAL(KIND=8) zcooz1vu(jpprod) ! id. zcooz1v sauf unidim.
REAL(KIND=8) zcoozcvu(jpprod) ! id. zcoozcv sauf unidim.
REAL(KIND=8) zcooz0fu(jpprod) ! id. zcooz0f sauf unidim.
REAL(KIND=8) zcooz1fu(jpprod) ! id. zcooz1f sauf unidim.
REAL(KIND=8) zcoozcfu(jpprod) ! id. zcoozcf sauf unidim.
!
REAL(KIND=8) zcoon0vu(jpprod) ! id. zcoon0v sauf unidim.
REAL(KIND=8) zcoon1vu(jpprod) ! id. zcoon1v sauf unidim.
REAL(KIND=8) zcooncvu(jpprod) ! id. zcooncv sauf unidim.
REAL(KIND=8) zcoon0fu(jpprod) ! id. zcoon0f sauf unidim.
REAL(KIND=8) zcoon1fu(jpprod) ! id. zcoon1f sauf unidim.
REAL(KIND=8) zcooncfu(jpprod) ! id. zcooncf sauf unidim.
!
REAL(KIND=8) zsols(jpdom) ! tableau de donnees au sol.
REAL(KIND=8) zpros(jpdom) ! tableau de proportion de surface du sol.
!
REAL(KIND=8) zcoet(jpcoo) ! coefficients de conversion temporelle pour les jpcoo coordo
character*200 clcoo(jpcoo) ! type de coordonnee pour les jpcoo coordonnees.
REAL(KIND=8) zcoo(jpcoo+1) ! reels a ecrire sur le fichier de sortie.
!
REAL(KIND=8) zchacf(jpprod) ! champ courant (vers lfaecr).
!
! Initialisation par defaut.
!
clficd='DHFZO.lfa'
clopt='ddhlfa2dd'
clficc='lc'
clconvu='OUI'
clconvs='NON'
clfics=' '
llcoodef=.true. ! vrai si les coordonnées sont celles par défaut.
clcoo(1)='NON'
clcoo(2)='NON'
clcoo(3)='NON'
clficm='DDH.lismoi'
llbil=.false.
lllis=.false. ! vrai si l'utilisateur souhaite la sortie de la seule liste des champs.
lldebug=.false. ! mode debug verbeux!...
idateref=0
zcoefddhi=1. ! coefficient multiplicateur fourni depuis la ligne commande.
zadd_ddhi=0. ! valeur additive fournie depuis la ligne commande.
llaqua=.false. ! vrai si DDH d'aquaplanète.
llelev=.false. ! vrai si la verticale Z doit être donnée en élévation et non altitude.
llemagramme=.false.
cltypeema='E761'
clhmaxi='18.' ! hauteur maximale à écrire sur les fichiers-profils de sortie.
clhmaxi='30.' ! hauteur maximale à écrire sur les fichiers-profils de sortie.
llcp=.true.

CALL GETENV('DDHI_LIST',cliscfile)
!WRITE(*,*) 'default list file:',TRIM(cliscfile)
!
! Obtention des parametres d'entree par ligne de commande.
!
ierr=0
do jarg=1,jparg
  clarg(jarg)=' '
  call getargp(jarg,clarg(jarg))
  if(clarg(jarg)(1:1) /= ' ') then
    !
    ! L'argument numero jarg est non vide.
    !
    if(clarg(jarg)(1:len_trim(clarg(jarg))) == '-elev') then
      llelev=.true.
    elseif(clarg(jarg)(1:len_trim(clarg(jarg))) == '-debug') then
      lldebug=.true.
    elseif(clarg(jarg)(1:2) == '-e') then
      llemagramme=.true.
    elseif(clarg(jarg)(1:2) == '-t') then
      cltypeema=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-L') then
      lllis=.true.
    elseif(clarg(jarg)(1:2) == '-l') then
      clficc=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-F') then
      cliscfile=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-s') then
      clfics=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-m') then
      read(clarg(jarg)(3:),fmt=*) zcoefddhi
    elseif(clarg(jarg)(1:2) == '-d') then
      read(clarg(jarg)(3:),fmt=*) zcoefddhi
      zcoefddhi=1./zcoefddhi
    elseif(clarg(jarg)(1:2) == '-a') then
      read(clarg(jarg)(3:),fmt=*) zadd_ddhi
    elseif(clarg(jarg)(1:5) == '-ymax') then
      clhmaxi=clarg(jarg)(6:)
    elseif(clarg(jarg)(1:2) == '-c') then
      clloc=clarg(jarg)(3:)
      if(clloc(1:index(clloc,' ')) == 'OUI') then
      elseif(clloc(1:index(clloc,' ')) == 'NON') then
      elseif(clloc(1:index(clloc,' ')) == 'SOL') then
      elseif(clloc(1:index(clloc,' ')) == 'SOM') then
      else
        ierr=1
        print*,'ERREUR: argument -c errone!...'
      endif
      clconvu=clloc
    elseif(clarg(jarg)(1:5) == '-aqua') then
      llaqua=.true.
    elseif(trim(clarg(jarg)) == '-n') then
      !
      !-------------------------------------------------
      ! The option "-n" is let for compatibility with previous ddhi versions, so that ddhi works if the option "-n" is given.
      ! However, as llcp=T is now the default, typing "ddhi -n" or "ddhi" produces the same results.
      !-------------------------------------------------
      !
      !llcp=.true.        
    elseif(trim(clarg(jarg)) == '-noccd') then
      llcp=.false.        
    elseif(clarg(jarg)(1:2) == '-S') then
      clloc=clarg(jarg)(3:)
      if(clloc(1:index(clloc,' ')) == 'OUI') then
      elseif(clloc(1:index(clloc,' ')) == 'NON') then
      else
        ierr=1
        print*,'ERREUR: argument -S errone!...'
      endif
      clconvs=clloc
    elseif(clarg(jarg)(1:2) == '-o') then
      clloc=clarg(jarg)(3:)
      if(clloc(1:index(clloc,' ')) == 'ddhlfa2xyv') then
      elseif(clloc(1:index(clloc,' ')) == 'ddhlfa2dd') then
      elseif(clloc(1:index(clloc,' ')) == 'BIL') then
        llbil=.true.
        icoo=2
        clcoo(1)='HDOM'
        clcoo(2)='VN'
        llcoodef=.false.
      else
        ierr=1
        print*,'ERREUR: argument -o errone!...'
      endif
      clopt=clloc
    elseif(clarg(jarg)(1:2) == '-1') then
      clloc=clarg(jarg)(3:)
      icoo=1
      llcoodef=.false.
      if(clloc(1:index(clloc,' ')) == 'VP') then
      elseif(clloc(1:index(clloc,' ')) == 'VZ') then
      elseif(clloc(1:index(clloc,' ')) == 'VN') then
      elseif(clloc(1:index(clloc,' ')) == 'HLAT') then
      elseif(clloc(1:index(clloc,' ')) == 'HLON') then
      elseif(clloc(1:index(clloc,' ')) == 'HDOM') then
      elseif(clloc(1:index(clloc,' ')) == 'HDIS') then
      elseif(clloc(1:index(clloc,' ')) == 'TJ') then
        zcoet(icoo)=86400.
      elseif(clloc(1:index(clloc,' ')) == 'TH') then
        zcoet(icoo)=3600.
      elseif(clloc(1:index(clloc,' ')) == 'TS') then
        zcoet(icoo)=1.
      elseif(clloc(1:index(clloc,' ')) == 'NON') then
      elseif(clloc(1:index(clloc,' ')) == 'BA') then
      elseif(clloc(1:index(clloc,' ')) == 'JD0') then
      elseif(clloc(1:index(clloc,' ')) == 'JD1') then
      elseif(clloc(1:3) == 'PTS') then
      elseif(clloc(1:1) == 'B') then
        read(clloc(2:index(clloc,' ')),fmt='(a1,a1,i8)') clbif,clut,idateref
      else
        ierr=1
        print*,'ERREUR: argument -1 errone!...'
      endif
      clcoo(icoo)=clloc
    elseif(clarg(jarg)(1:2) == '-2') then
      clloc=clarg(jarg)(3:)
      icoo=2
      llcoodef=.false.
      if(clloc(1:index(clloc,' ')) == 'VP') then
      elseif(clloc(1:index(clloc,' ')) == 'VZ') then
      elseif(clloc(1:index(clloc,' ')) == 'VN') then
      elseif(clloc(1:index(clloc,' ')) == 'HLAT') then
      elseif(clloc(1:index(clloc,' ')) == 'HLON') then
      elseif(clloc(1:index(clloc,' ')) == 'HDOM') then
      elseif(clloc(1:index(clloc,' ')) == 'HDIS') then
      elseif(clloc(1:index(clloc,' ')) == 'TJ') then
        zcoet(icoo)=86400.
      elseif(clloc(1:index(clloc,' ')) == 'TH') then
        zcoet(icoo)=3600.
      elseif(clloc(1:index(clloc,' ')) == 'TS') then
        zcoet(icoo)=1.
      elseif(clloc(1:index(clloc,' ')) == 'NON') then
      elseif(clloc(1:index(clloc,' ')) == 'BA') then
      elseif(clloc(1:1) == 'B') then
        read(clloc(2:index(clloc,' ')),fmt='(a1,a1,i8)') clbif,clut,idateref
      else
        ierr=1
        print*,'ERREUR: argument -2 errone!...'
      endif
      clcoo(icoo)=clloc
    elseif(clarg(jarg)(1:2) == '-3') then
      clloc=clarg(jarg)(3:)
      icoo=3
      llcoodef=.false.
      if(clloc(1:index(clloc,' ')) == 'VP') then
      elseif(clloc(1:index(clloc,' ')) == 'VZ') then
      elseif(clloc(1:index(clloc,' ')) == 'VN') then
      elseif(clloc(1:index(clloc,' ')) == 'HLAT') then
      elseif(clloc(1:index(clloc,' ')) == 'HLON') then
      elseif(clloc(1:index(clloc,' ')) == 'HDOM') then
      elseif(clloc(1:index(clloc,' ')) == 'HDIS') then
      elseif(clloc(1:index(clloc,' ')) == 'TJ') then
        zcoet(icoo)=86400.
      elseif(clloc(1:index(clloc,' ')) == 'TH') then
        zcoet(icoo)=3600.
      elseif(clloc(1:index(clloc,' ')) == 'TS') then
        zcoet(icoo)=1.
      elseif(clloc(1:index(clloc,' ')) == 'NON') then
      elseif(clloc(1:index(clloc,' ')) == 'BA') then
      elseif(clloc(1:1) == 'B') then
        read(clloc(2:index(clloc,' ')),fmt='(a1,a1,i8)') clbif,clut,idateref
      else
        ierr=1
        print*,'ERREUR: argument -3 errone!...'
      endif
      clcoo(icoo)=clloc
    elseif(clarg(jarg)(1:2) == '-r') then
      clficm=clarg(jarg)(3:)
    else
      !
      ! L'argument n'a pas été tapé comme "-"
      ! suivi de caractères.
      ! On suppose par défaut qu'il s'agit
      ! du nom du fichier DDH d'entrée.
      !
      clficd=clarg(jarg)
    endif
  endif
enddo
if(lllis) then
  !
  ! Chargement de la liste de conversion.
  !
  call lisc(jpnomct,cllisconv,ilisconv,cliscfile)
  do jlis=1,ilisconv
    clsor=cllisconv(jlis)(5:17)//' # '//cllisconv(jlis)(44:103)
    ilsor=len_trim(clsor)
    write(*,'(a)') clsor(1:ilsor)
  enddo
  stop
endif
ilopt=len_trim(clopt)
!
! Initialisation du nom de fichier de sortie
! si celui-ci n'a pas ete fourni par l'utilisateur.
!
ilficd=len_trim(clficd)
if(clfics == ' '.and.llbil) then
  clfics=clficd(1:index(clficd,' ')-1)//'.bil'
elseif(clfics == ' ') then
  clfics=clficd(1:ilficd)//'.tmp'
endif
if(clarg(1)(1:1) == ' '.or.ierr == 1) then
  !
  ! L'utilisateur n'a tape aucun argument.
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Interpretation of a DDH file.'
  write(*,'(a)') 'Converts extensive units in mass and time,'
  write(*,'(a)') 'which are native units in DDH files,'
  write(*,'(a)') 'into intensive units (K/day, g/kg, etc).'
  write(*,'(a)') ' '
  write(*,'(a)') 'Usage: ddhi [-e] [-t] -o -l -m -d -a -c -S -s -1 -2 -3 -elev -r -L -aqua -debug -ymax ficddh '
  write(*,'(a)') ' '
  write(*,'(a)') 'where'
  write(*,'(a)') '  ficddh DDH file name  '
  write(*,'(2a)') '     Default: ',clficd(1:ilficd)
  write(*,'(a)') '  -e to compute the vertical profile of theta, thetaW, thetaE, thetaES, theta''w, thetaS, thetaL, thetaV, thetaVL.'
  write(*,'(a)') '  -t if "-e" option given, this option defines the kind of T-Phi-gram:'
  write(*,'(9a)') '    AS: adiabatic ascents are vertical'
  write(*,'(9a)') '    PAH: irreversible pseudo-adiabatic ascents are vertical'
  write(*,'(9a)') '    E761: local T corrected as a linear function of p'
  write(*,'(9a)') '    Default: ',trim(cltypeema)
  write(*,'(a)') '  -o conversion option (ddhlfa2xyv, ddhlfa2dd, BIL).'
  write(*,'(a)') '     Default: ddhlfa2dd'
  write(*,'(a)') '  -l list of articles to interpret.'
  write(*,'(a)') '     Default: lc'
  write(*,'(a)') '     In this list:'
  write(*,'(a)') '         VxxD et VxxM: difference between final and initial INTENSIVE variables.'
  write(*,'(a)') '         VxxF        : difference between final and initial EXTENSIVE variables (budget mode).'
  write(*,'(a)') '  -m multiplication coefficient to apply to all fields.'
  write(*,'(a)') '  -d division       coefficient to apply to all fields.'
  write(*,'(a)') '     Default: 1.'
  write(*,'(a)') '  -a addition to apply to all fields.'
  write(*,'(a)') '     Default: 0.'
  write(*,'(a)') '  -L standard output of all fields, than can be managed by ddhi.'
  write(*,'(a)') '  -F alternative liste_conversion.'
  write(*,'(a)') '  -c convert units (OUI, NON, SOL, SOM).'
  write(*,'(a)') '     Default: OUI'
  write(*,'(a)') '  -S interpret soil data (OUI, NON).'
  write(*,'(a)') '     Default: NON'
  write(*,'(a)') '  -s prefix of output files.'
  write(*,'(a)') '     Default: name of input file + .tmp'
  write(*,'(a)') '  -1 coordinate 1: (VP, VZ, VN, HLAT, HLON,'
  write(*,'(a)') '                  HDOM, HDIS, TJ, TH, TS, JD0, JD1, PTS, BA, BxyD, NON).'
  write(*,'(a)') '                  where VP: pressure; VZ: altitude'
  write(*,'(a)') '                       VN: levels; HLAT: latitude'
  write(*,'(a)') '                       HLON: longitude; HDOM: domaine number'
  write(*,'(a)') '                       HDIS: distance to the first domain'
  write(*,'(a)') '                       TJ: prediction range in days'
  write(*,'(a)') '                       TH: prediction range in hours'
  write(*,'(a)') '                       JD0: initial julian date of the DDH file'
  write(*,'(a)') '                       JD1: final julian date of the DDH file (initial date + prediction range)'
  write(*,'(a)') '                       PTSYYYYMMDDHH: Prediction Time in hours Since YYYYMMDDHH (example: PTS2023100612)'
  write(*,'(a)') '                       BA: base in years AAAA + (MM-1)/12. + (QQ-1)/365.2425 + HH/8765.82 + MM/525949.2.'
  write(*,'(a)') '                       BxyD: prediction range (in days if y=J or years if y=A)'
  write(*,'(a)') '                             between base and date D (au format AAAAMMQQ),'
  write(*,'(a)') '                             the base being that of DDH file  (if x=I)'
  write(*,'(a)') '                             or that of DDH file + prediction range (if x=F)'
  write(*,'(a)') '     Default: HLAT.'
  write(*,'(a)') '  -2 coordinate 2: (VP, VZ, VN, HLAT, HLON,'
  write(*,'(a)') '                  HDOM, HDIS, TJ, TH, TS, JD0, JD1, PTS, BxyD, NON).'
  write(*,'(a)') '     Default: VP.'
  write(*,'(a)') '  -3 coordinate 3: (VP, VZ, VN, HLAT, HLON,'
  write(*,'(a)') '                  HDOM, HDIS, TJ, TH, TS, JD0, JD1, PTS, BxyD, NON).'
  write(*,'(a)') '     Default: NON.'
  write(*,'(a)') '  -elev if ones choose the vertical coordinate to be an elevation (above surface).'
  write(*,'(a)') '        By default this coordinate is an altitude (above sea level).'
  write(*,'(a)') '  -r file name for text outputs.'
  write(*,'(a)') '     Default: DDH.lismoi'
  write(*,'(a)') '  -aqua mode "aquaplanete": no contouring of continental borders.'
  write(*,'(a)') '  -debug verbose mode.'
  write(*,'(a)') '  -ymax: do not produce the profile over this vertical height (in km).'
  write(*,'(a)') '  -noccd (NO Calorific Capacity Dependency): do not compute calorific capacity as a function of qv, ql, qr, qi, qs, qg'
  write(*,'(a)') 'Examples:'
  write(*,'(a)') '  ddhi Fic1 -llc -ymax8.5'
  write(*,'(a)') '  ddhi Fic1 -llc -SOUI -cNON '
  write(*,'(a)') '  ddhi Fic1 -llc -SOUI -cNON -m0.86'
  write(*,'(a)') '  ddhi Fic1 -llc -1BIJ19960801 '
  write(*,'(a)') '  ddhi -L'
  write(*,'(a)') '------------------------------------------------------'
  stop
endif
print*,'----------------------'
print*,'---DDHI-CHAMPS--------'
print*,'----------------------'
!
! Unites logiques.
!
iulm=2
iulb=15
iulc=3
iul3=8
iul1=14
!
! Fichier lfa  de DDH d'entree
!
ilficd=len_trim(clficd)
print*,'Input file: ',trim(clficd)
call lfaouv(iul1,clficd,'R')
!if(lldebug) call lfames(iul1,2)
!
! Echeances et types de domaines.
!
call lfalecc(iul1,'INDICE EXPERIENCE',1,clnamx,ilong,irep)
ilnamx=len_trim(clnamx)
call lfaleci(iul1,'DATE',11,idatef,ilong,irep)
isssss=idatef(4)*3600+idatef(5)*60
iamq=10000*idatef(1)+100*idatef(2)+idatef(3)
!
! --------------------------------------------------------------------------
!
! Determination de la duree du run en secondes.
!
! On lit la duree du run sur le fichier de DDH.
idim1=1
call lfalecr(iul1,'ECHEANCE',idim1,zech,ilong,irep)
!
! Echéance caractéristique des "delta de variables".
!
call lfacas(iul1,'ECHEANCEDV',cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
if(irep == 0) then
  !
  ! L'article est dans le fichier.
  ! On y lit l'échéance.
  !
  call lfalecr(iul1,'ECHEANCEDV',1,zechdv,ilong,irep)
else
  !
  ! L'article n'est pas dans le fichier.
  ! L'échéance est prise égale à l'écheance nominale 'ECHEANCE'.
  !
  zechdv(1)=zech(1)
endif
if(idateref /= 0) then
  !
  ! L'utilisateur a fourni une date
  ! de référence par rapport à laquelle
  ! sont à calculer des écarts.
  !
  if(clbif == 'I') then
    !
    ! L'écart est à calculer par rapport
    ! à la base du fichier DDH.
    ! Ecart entre la base DDH et idateref à 0h.
    !
    call ecartd(idateref,iamq,1,iecart)
    zecart(1)=real(iecart)+real(isssss)/86400.
    if(clut == 'A') then
      !
      !-------------------------------------------------
      ! On veut l'écart en années et non en jours.
      !-------------------------------------------------
      !
      zecart(1)=zecart(1)/365.2425
    elseif(clut == 'J') then
    else
      print*,'ddhi/ERREUR: unité de temps non reconnue 1: ',clut,'!...'
                        call exit(1)
    endif
  elseif(clbif == 'F') then
    !
    ! L'écart est à calculer par rapport
    ! à la base + échéance du fichier DDH.
    ! Ecart entre la base DDH et idateref.
    !
    call ecartd(idateref,iamq,1,iecart)
    zecart(1)=real(iecart)+real(isssss)/86400.
    !
    ! A cet ecart on ajoute l'échéance du fichier DDH.
    !
    zecart(1)=zecart(1)+zech(1)/86400.
    if(clut == 'A') then
      !
      !-------------------------------------------------
      ! On veut l'écart en années et non en jours.
      !-------------------------------------------------
      !
      zecart(1)=zecart(1)/365.2425
    elseif(clut == 'J') then
    else
      print*,'ddhi/ERREUR: unité de temps non reconnue 2: ',clut,'!...'
                        call exit(1)
    endif
  else
    print*,'DDHI/ERREUR: la coordonnée B',clbif,' n''existe pas!...'
    print*,'Attendu: BI ou BF!...'
                call exit(1)
  endif
else
     zecart(1)=0.
endif
idocfi=0
call lfaleci(iul1,'DOCFICHIER',17,idocfi,ilong,irep)
itype(1)=idocfi(1)
ilev(1)=idocfi(6)
ilev2=ilev(1)+1
idom(1)=idocfi(15)
!
! Nombre de points pour l'éventuelle interpolation par un outil de tracé.
! En X (bandes zonales): on interpole de 3 fois le nombre de bandes zonales si 30 bandes zonales à 1.2 fois le nombre de bandes zonales si 250 bandes zonales. La transition est continue.
zfrac=max(0.,min(1.,(real(idom(1))-30.)/(250.-30.))) ; zintx=3.+(1.2-3.)*zfrac
iintx=nint(zintx*real(idom(1))) ! interpolation en X.
iinty=nint(real(ilev(1))*2.) ! interpolation en Y.
iliss=2 ! lissage.
!
! Contrôle des dimensions statiques.
!
if(ilev(1) > jplev) then
  print*,'ERREUR: recompiler avec une valeur plus grande de jplev!...'
  print*,ilev(1),jplev
  call exit(1)
endif
if(idom(1) > jpdom) then
  print*,'ERREUR: recompiler avec une valeur plus grande de jpdom!...'
  print*,idom(1),jpdom
  call exit(1)
endif
if(llemagramme) then
  if(idom(1) > 1) then
    write(*,fmt=*) 
    write(*,fmt=*) 'ddhi/ERROR: the "-e" option is to be used only with DDH files containing a single domain!...'
    write(*,fmt=*) 
    call exit(1)
  endif
  call emagramme(iul1,clficd,cltypeema,ilev)
endif
!
!-------------------------------------------------
! Détermination des coordonnées de sortie.
!-------------------------------------------------
!
if(llcoodef) then
  !
  ! Les coordonnées sont par défaut (l'utilisateur
  ! n'a pas imposé les siennes).
  ! On choisit un type de coordonnées adapté
  ! au fichier courant.
  !
  ll1n=ilev(1) == 1.or.clconvu == 'SOL'.or.clconvu == 'SOM' ! vrai si un seul niveau de sortie.
  if(ll1n.and.idom(1) == 1) then
    !
    ! Un niveau, un domaine.
    !
    clcoo(1)='NON'
    clcoo(2)='NON'
    clcoo(3)='NON'
  elseif(ll1n) then
    !
    ! Un niveau, plusieurs domaines.
    !
    if(itype(1) == 6) then
      !
      ! Fichier zonal.
      !
      clcoo(1)='HLAT'
      clcoo(2)='NON'
      clcoo(3)='NON'
    elseif(itype(1) == 1) then
      !
      ! Fichier domaines limités.
      !
      clcoo(1)='HLON'
      clcoo(2)='HLAT'
      clcoo(3)='NON'
    else
      print*,'DDHI/ERREUR: nombre de domaines > 1 dans un fichier global!...'
                        call exit(1)
    endif
  elseif(idom(1) == 1) then
    !
    ! Plusieurs niveaux, un domaine.
    !
    clcoo(1)='VP'
    clcoo(2)='NON'
    clcoo(3)='NON'
  else
    !
    ! Plusieurs niveaux, plusieurs domaines.
    !
    clcoo(1)='HLAT'
    clcoo(2)='VP'
    clcoo(3)='NON'
  endif
endif
ilcoo1=len_trim(clcoo(1))
ilcoo2=len_trim(clcoo(2))
!
! Ouverture du lfa de sortie.
!
call lfaouv(iul3,'.ddhi.tmp','W')
call lfapreci(iul3,jpprecint)
call lfaprecr(iul3,jpprecree)
call lfaecrc(iul3,'CONVERSION EFFECTUEE',clconvu,1)
call lfaecrc(iul3,'INDICE EXPERIENCE',clnamx,1)
call lfaecri(iul3,'DATE',idatef,11)
call lfaecri(iul3,'TYPE DE FICHIER',itype,1)
call lfaecri(iul3,'NOMBRE DE NIVEAUX VERTICAUX',ilev,1)
call lfaecri(iul3,'NOMBRE DE DOMAINES',idom,1)
!
! Fichier formatte clficm qui va recevoir                     .
! les resultats en clair: moyenne, ecart-type, etc...
!
if(lldebug) open(iulm,file=clficm,form='formatted',status='unknown')
!
! Fichier formatte qui va recevoir                     .
! les resultats de bilan.
!
if(llbil) open(iulb,file=clfics,form='formatted',status='unknown')
if(lldebug) then
write(iulm,*) '-------------------------------------------------'
if(itype(1) == 1) then
  write(iulm,fmt=*) 'DIAGNOSTICS EN DOMAINES LIMITES'
elseif(itype(1) == 5) then
  write(iulm,fmt=*) 'DIAGNOSTICS DOMAINE GLOBAL'
elseif(itype(1) == 6) then
  write(iulm,fmt=*) 'DIAGNOSTICS ZONAUX'
else
  call stoperr('Type de fichier de DDH non reconnu')
endif
write(iulm,fmt=*) 'Nombre de domaines: ',idom(1)
write(iulm,fmt=*) 'Nombre de niveaux : ',idocfi(6)
write(iulm,fmt=*) 'Run: ',clnamx(1:ilnamx)
write(iulm,fmt=*) idatef(1), &
& idatef(2), &
& idatef(3), &
& idatef(4), &
& idatef(5)
endif
call lfaecrr(iul3,'ECHEANCE',zech,1)
if(lldebug) then
write(iulm,fmt=*) 'Duree du run en secondes:',zech(1)
write(iulm,fmt=*) '                heures  :',zech(1)/3600.
write(iulm,fmt=*) '                jours   :',zech(1)/86400.
endif
zg=9.80665
znorp=zg/100. ! normalisation de la pression: resultat en hPa.
znorz=1./zg/1000. ! normalisation de la coordonnee z: resultat en km.
!
! Articles de documentation sur les domaines.
!
zdocd(:)=0.
do jdom=1,idom(1)
  call lited(iul1,jdom,'R',zdocd,irep)
  call lited(iul3,jdom,'W',zdocd,irep)
enddo
call lfacas(iul1,'VCZ0',cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
if(irep == 0) then
  !
  !-------------------------------------------------
  ! Le fichier courant est un fichier de différence. La coordonnée
  ! verticale est lue sur les articles VCP et VCZ.
  !-------------------------------------------------
  !
  !
  ! Masse.
  !
  cladcou='VCP0'
  call lfalecr(iul1,cladcou,jpprod,zpre0,ilong,irep)
  call tespv0(zpre0,ilong,llpre0)
  cladcou='VCP1'
  call lfalecr(iul1,cladcou,jpprod,zpre1,ilong,irep)
  if(llpre0) then
    write(*,fmt=*) 'ddhi/WARNING: initial pressure = 0. !...'
    zpre0=zpre1
  endif
  !
  ! Z.
  !
  cladcou='VCZ0'
  call lfalecr(iul1,cladcou,jpprod,zz0,ilong,irep)
  call tespv0(zz0,ilong,llz0)
  cladcou='VCZ1'
  call lfalecr(iul1,cladcou,jpprod,zz1,ilong,irep)
  if(llz0) then
    write(*,fmt=*) 'ddhi/WARNING: initial z = 0. !...'
    zz0=zz1
  endif
else
  !
  !-------------------------------------------------
  ! Le fichier courant n'est pas un fichier de différence. La coordonnée
  ! verticale est lue sur les articles VPP et VEP.
  !-------------------------------------------------
  !
  !
  ! Masse.
  !
  cladcou='VPP0'
  call lfalecr(iul1,cladcou,jpprod,zpre0,ilong,irep)
  call tespv0(zpre0,ilong,llpre0)
  cladcou='VPP1'
  call lfalecr(iul1,cladcou,jpprod,zpre1,ilong,irep)
  if(llpre0) then
    write(*,fmt=*) 'ddhi/WARNING: initial pressure = 0. !...'
    zpre0=zpre1
  endif
  !
  ! Z.
  !
  cladcou='VEP0'
  call lfalecr(iul1,cladcou,jpprod,zz0,ilong,irep)
  call tespv0(zz0,ilong,llz0)
  cladcou='VEP1'
  call lfalecr(iul1,cladcou,jpprod,zz1,ilong,irep)
  if(llz0) then
    write(*,fmt=*) 'ddhi/WARNING: initial z = 0. !...'
    zz0=zz1
  endif
endif
cladcou='PPP'
call lfalecr(iul1,cladcou,jpprod,zprec,ilong,irep)
!
! Température.
!
cladcou='VCT0'
call lfacas(iul1,cladcou,cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
if(irep == 0) then
  call lfalecr(iul1,cladcou,jpprod,zvct0u,ilong,irep)
  cladcou='VCT1'
  call lfalecr(iul1,cladcou,jpprod,zvct1u,ilong,irep)
  llvct0=.true.
else
  llvct0=.false.
endif


zrcp0=rcpd
zrcp1=rcpd
zepsqv=0.
zepsql=0.
zepsqr=0.
zepsqi=0.
zepsqs=0.
zepsqg=0.

if (llcp) then  
  cladcou='VQV0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvqv0u,ilong,irep)
    cladcou='VQV1'
    call lfalecr(iul1,cladcou,jpprod,zvqv1u,ilong,irep)
    zepsqv=1.
  endif
  cladcou='VQL0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvql0u,ilong,irep)
    cladcou='VQL1'
    call lfalecr(iul1,cladcou,jpprod,zvql1u,ilong,irep)
    zepsql=1.
  endif
  cladcou='VQR0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvqr0u,ilong,irep)
    cladcou='VQR1'
    call lfalecr(iul1,cladcou,jpprod,zvqr1u,ilong,irep)
    zepsqr=1.
  endif
  cladcou='VQI0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvqi0u,ilong,irep)
    cladcou='VQI1'
    call lfalecr(iul1,cladcou,jpprod,zvqi1u,ilong,irep)
    zepsqi=1.
  endif
  cladcou='VQS0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvqs0u,ilong,irep)
    cladcou='VQS1'
    call lfalecr(iul1,cladcou,jpprod,zvqs1u,ilong,irep)
    zepsqs=1.
  endif
  cladcou='VQG0'
  call lfacas(iul1,cladcou,cltype,ilong,irep)
  if(irep == 0) then
    call  lfalecr(iul1,cladcou,jpprod,zvqg0u,ilong,irep)
    cladcou='VQG1'
    call lfalecr(iul1,cladcou,jpprod,zvqg1u,ilong,irep)
    zepsqg=1.
  endif
endif
!
! Passage 1D > 2D.
!
ib=0
do jdom=1,idom(1)
  do jlev=1,ilev(1)
    ib=ib+1
    !
    ! Cp
    !
    if (llcp) then
      zrcp0(ib)=zrcp0(ib)+zepsqv*(rcpv-rcpd)*zvqv0u(ib)/zpre0(ib)+&
        & zepsql*(rcw-rcpd)*zvql0u(ib)/zpre0(ib)+ & 
        & zepsqr*(rcw-rcpd)*zvqr0u(ib)/zpre0(ib)+ &
        & zepsqi*(rcs-rcpd)*zvqi0u(ib)/zpre0(ib)+ &
        & zepsqs*(rcs-rcpd)*zvqs0u(ib)/zpre0(ib)+ &
        & zepsqg*(rcs-rcpd)*zvqg0u(ib)/zpre0(ib)

      zrcp1(ib)=zrcp1(ib)+zepsqv*(rcpv-rcpd)*zvqv1u(ib)/zpre1(ib)+&
        & zepsql*(rcw-rcpd)*zvql1u(ib)/zpre1(ib)+ & 
        & zepsqr*(rcw-rcpd)*zvqr1u(ib)/zpre1(ib)+ &
        & zepsqi*(rcs-rcpd)*zvqi1u(ib)/zpre1(ib)+ &
        & zepsqs*(rcs-rcpd)*zvqs1u(ib)/zpre1(ib)+ &
        & zepsqg*(rcs-rcpd)*zvqg1u(ib)/zpre1(ib)
    endif             
    !
    !
    ! Pression.
    !
    zcoop0v(jlev,jdom)=zpre0(ib)*znorp
    zcoop1v(jlev,jdom)=zpre1(ib)*znorp
    zcoopcv(jlev,jdom)=zprec(ib)*znorp/zech(1)
    !
    ! Z.
    !
    zcooz0v(jlev,jdom)=zz0(ib)*znorz/zpre0(ib)
    zcooz1v(jlev,jdom)=zz1(ib)*znorz/zpre1(ib)
    zcoozcv(jlev,jdom)=zz1(ib)*znorz/zpre1(ib)
    !
    ! Température.
    !
    if(llvct0) then
      zvct0(jlev,jdom)=zvct0u(ib)/zpre0(ib)/zrcp0(ib)
      zvct1(jlev,jdom)=zvct1u(ib)/zpre1(ib)/zrcp1(ib)
    endif
  enddo
  ierr=0
  if(zcoop0v(ilev(1),jdom) == 0.) then
    print*,'DDHI/WARNING: initial pressure domain ',jdom,' = 0. !...'
  endif
  if(zcoop1v(ilev(1),jdom) == 0.) then
    print*,'DDHI/WARNING: final pressure domain ',jdom,' = 0. !...'
  endif
  if(zcoopcv(ilev(1),jdom) == 0.) then
    print*,'DDHI/WARNING: time-cumulated pressure domain ',jdom,' = 0. !...'
  endif
enddo
!
!-------------------------------------------------
! Pour info.
!-------------------------------------------------
!
write(*,fmt=*) 'Max value of initial cp = ',maxval(zrcp0)
if(ierr == 1) call exit(1)
!
! Integrale verticale de la coordonnee pression, niveaux de flux.
!
do jdom=1,idom(1)
  zcoop0f(1,jdom)=0.
  zcoop1f(1,jdom)=0.
  zcoopcf(1,jdom)=0.
  do jlev=2,ilev(1)+1
    zcoop0f(jlev,jdom)=zcoop0f(jlev-1,jdom)+zcoop0v(jlev-1,jdom)
    zcoop1f(jlev,jdom)=zcoop1f(jlev-1,jdom)+zcoop1v(jlev-1,jdom)
    zcoopcf(jlev,jdom)=zcoopcf(jlev-1,jdom)+zcoopcv(jlev-1,jdom)
  enddo
  !
  !-------------------------------------------------
  ! Pression de surface, en Pa.
  !-------------------------------------------------
  !
  zpsurf0(jdom)=zcoop0f(ilev(1)+1,jdom)*100.
  zpsurf1(jdom)=zcoop1f(ilev(1)+1,jdom)*100.
  zpsurfc(jdom)=zcoopcf(ilev(1)+1,jdom)*100.
  if(lldebug) then
    write(*,fmt=*) 'Domaine ',jdom
    write(*,fmt=*) '  Pression de surface initiale=',zpsurf0(jdom),' Pa'
    write(*,fmt=*) '  Pression de surface moyenne =',zpsurfc(jdom),' Pa'
    write(*,fmt=*) '  Pression de surface finale  =',zpsurf1(jdom),' Pa'
  endif
enddo
!
! Coordonnee pression, niveaux de variables.
!
do jdom=1,idom(1)
  do jlev=1,ilev(1)
    zcoop0v(jlev,jdom)=0.5*(zcoop0f(jlev,jdom)+zcoop0f(jlev+1,jdom))
    zcoop1v(jlev,jdom)=0.5*(zcoop1f(jlev,jdom)+zcoop1f(jlev+1,jdom))
    zcoopcv(jlev,jdom)=0.5*(zcoopcf(jlev,jdom)+zcoopcf(jlev+1,jdom))
  enddo
enddo
!
! Interpolation des coordonnees niveaux variables
! sur les niveaux de flux.
!
do jdom=1,idom(1)
  ! Sommet Z.
  if(ilev(1) > 1) then
    zcooz0f(1,jdom)=1.5*zcooz0v(1,jdom)-0.5*zcooz0v(2,jdom)
    zcooz1f(1,jdom)=1.5*zcooz1v(1,jdom)-0.5*zcooz1v(2,jdom)
    zcoozcf(1,jdom)=zcooz1f(1,jdom)
  else
    zcooz0f(1,jdom)=2.*zcooz0v(1,jdom)
    zcooz1f(1,jdom)=2.*zcooz1v(1,jdom)
    zcoozcf(1,jdom)=zcooz1f(1,jdom)
  endif
  do jlev=2,ilev2-1
    ! Courant Z.
    zcooz0f(jlev,jdom)=0.5*(zcooz0v(jlev-1,jdom)+zcooz0v(jlev,jdom))
    zcooz1f(jlev,jdom)=0.5*(zcooz1v(jlev-1,jdom)+zcooz1v(jlev,jdom))
    zcoozcf(jlev,jdom)=zcooz1f(jlev,jdom)
  enddo
  !
  !-------------------------------------------------
  ! Estimation de l'altitude de la surface, à partir
  ! de celle du niveau le plus bas et de la pression,
  ! en utilisant l'hydrostatique.
  !-------------------------------------------------
  !
  if(ilev(1) > 1 .and. llvct0) then
    zrd=287.0597
    zzsurf0(jdom)=zcooz0v(ilev(1),jdom)*1000.-0.5*zrd*zvct0(ilev(1),jdom)/zg &
    & *log(zcoop0f(ilev(1)+1,jdom)/zcoop0f(ilev(1),jdom))
    zzsurf1(jdom)=zcooz1v(ilev(1),jdom)*1000.-0.5*zrd*zvct1(ilev(1),jdom)/zg &
    & *log(zcoop1f(ilev(1)+1,jdom)/zcoop1f(ilev(1),jdom))
    zzsurfc(jdom)=zzsurf0(jdom)
  else
    zzsurf0(jdom)=zcooz0v(ilev(1),jdom)*1000.
    zzsurf1(jdom)=zcooz1v(ilev(1),jdom)*1000.
    zzsurfc(jdom)=zzsurf0(jdom)
  endif
  if(lldebug) then
    write(*,fmt=*) 'Domaine ',jdom
    write(*,fmt=*) '  Altitude initiale de la surface=',zzsurf0(jdom),' m'
    write(*,fmt=*) '  Altitude finale   de la surface=',zzsurf1(jdom),' m'
  endif
  ! Base Z.
  zcooz0f(ilev2,jdom)=zzsurf0(jdom)/1000.
  zcooz1f(ilev2,jdom)=zzsurf1(jdom)/1000.
  zcoozcf(ilev2,jdom)=zcooz1f(ilev2,jdom)
enddo
if(llelev) then
  !
  !-------------------------------------------------
  ! L'utilisateur veut l'élévation et non l'altitude.
  ! On soustrait donc l'altitude la surface à la coordonnée verticale z.
  !-------------------------------------------------
  !
  do jdom=1,idom(1)
    do jlev=1,ilev(1)
      zcooz0v(jlev,jdom)=zcooz0v(jlev,jdom)-zzsurf0(jdom)/1000.
      zcooz1v(jlev,jdom)=zcooz1v(jlev,jdom)-zzsurf1(jdom)/1000.
      zcoozcv(jlev,jdom)=zcoozcv(jlev,jdom)-zzsurfc(jdom)/1000.
    enddo
    do jlev=1,ilev2
      zcooz0f(jlev,jdom)=zcooz0f(jlev,jdom)-zzsurf0(jdom)/1000.
      zcooz1f(jlev,jdom)=zcooz1f(jlev,jdom)-zzsurf1(jdom)/1000.
      zcoozcf(jlev,jdom)=zcoozcf(jlev,jdom)-zzsurfc(jdom)/1000.
    enddo
  enddo
endif
!
! On recrit les tableaux unidimensionnels
! maintenant que l'integrale verticale est effectuee.
!
ib=0
do jdom=1,idom(1)
  do jlev=1,ilev(1)
    ib=ib+1
    ! Pression.
    zcoop0vu(ib)=zcoop0v(jlev,jdom)
    zcoop1vu(ib)=zcoop1v(jlev,jdom)
    zcoopcvu(ib)=zcoopcv(jlev,jdom)
    ! Z.
    zcooz0vu(ib)=zcooz0v(jlev,jdom)
    zcooz1vu(ib)=zcooz1v(jlev,jdom)
    zcoozcvu(ib)=zcoozcv(jlev,jdom)
    ! Niveaux.
    zcoon0vu(ib)=jlev
    zcoon1vu(ib)=jlev
    zcooncvu(ib)=jlev
  enddo
enddo
ib=0
do jdom=1,idom(1)
  do jlev=1,ilev2
    ib=ib+1
    ! Pression.
    zcoop0fu(ib)=zcoop0f(jlev,jdom)
    zcoop1fu(ib)=zcoop1f(jlev,jdom)
    zcoopcfu(ib)=zcoopcf(jlev,jdom)
    ! Z.
    zcooz0fu(ib)=zcooz0f(jlev,jdom)
    zcooz1fu(ib)=zcooz1f(jlev,jdom)
    zcoozcfu(ib)=zcoozcf(jlev,jdom)
    ! Niveaux.
    zcoon0fu(ib)=jlev-1
    zcoon1fu(ib)=jlev-1
    zcooncfu(ib)=jlev-1
  enddo
enddo
!
! Ecriture de la coordonnee pression sur le lfa.
!
imaxv=ilev(1)*idom(1)
imaxf=ilev2*idom(1)
! Pression.
call lfaecrr(iul3,'COOR-P0V',zcoop0vu,imaxv)
call lfaecrr(iul3,'COOR-P1V',zcoop1vu,imaxv)
call lfaecrr(iul3,'COOR-PCV',zcoopcvu,imaxv)
call lfaecrr(iul3,'COOR-P0F',zcoop0fu,imaxf)
call lfaecrr(iul3,'COOR-P1F',zcoop1fu,imaxf)
call lfaecrr(iul3,'COOR-PCF',zcoopcfu,imaxf)
! Z.
call lfaecrr(iul3,'COOR-Z0V',zcooz0vu,imaxv)
call lfaecrr(iul3,'COOR-Z1V',zcooz1vu,imaxv)
call lfaecrr(iul3,'COOR-ZCV',zcoozcvu,imaxv)
call lfaecrr(iul3,'COOR-Z0F',zcooz0fu,imaxf)
call lfaecrr(iul3,'COOR-Z1F',zcooz1fu,imaxf)
call lfaecrr(iul3,'COOR-ZCF',zcoozcfu,imaxf)
! Niveaux.
call lfaecrr(iul3,'COOR-N0V',zcoon0vu,imaxv)
call lfaecrr(iul3,'COOR-N1V',zcoon1vu,imaxv)
call lfaecrr(iul3,'COOR-NCV',zcooncvu,imaxv)
call lfaecrr(iul3,'COOR-N0F',zcoon0fu,imaxf)
call lfaecrr(iul3,'COOR-N1F',zcoon1fu,imaxf)
call lfaecrr(iul3,'COOR-NCF',zcooncfu,imaxf)
!
! On rend non fatale toute erreur de lecture du fichier.
!
call lfaerf(iul1,.false.)
!
! Fichier-liste clficc des champs a estimer et(ou) tracer
! sur le tableau cllislc
!
inquire(file=clficc,exist=llex)
ilislc=0
if(llex) then
  open(iulc,file=clficc,form='formatted',status='old')
  635   read(iulc,fmt='(a)',end=130) clint
  !
  ! On cherche la position d'un éventuel caractère
  ! marquant le début d'une zone commentaire
  ! sur la ligne.
  !
  if(clint(1:1) == ' '.or.clint(1:1) == '  '.or.clint(1:1) == '#') then
    !
    ! Ligne de commentaire.
    !
  else
    !
    ! Ligne non de commentaire.
    !
    icomm=index(clint,'#')
    if(icomm == 0) then
      !
      ! Pas de commentaire sur la ligne exécutable.
      ! La fin de ligne est celle physique
      ! de la chaîne.
      !
      ifinl=len(clint)
    else
      !
      ! On considère que la fin de ligne
      ! est à gauche du commentaire.
      !
      ifinl=icomm-1
    endif
    ilislc=ilislc+1
    if(ilislc > jpnomct) then
      print*,'DDHI/ERREUR: recompiler avec une valeur plus grande de jpnomct!...'
      print*,ilislc,jpnomct
                        call exit(1)
    endif
    ibarre=index(clint,'/')
    if(ibarre == 0) then
      !
      ! Pas de "/" sur la ligne exécutable.
      ! La fin de ligne est celle physique
      ! de la chaîne.
      !
      cllislc(ilislc)=clint(1:ifinl)
      cllislcc(ilislc)=' '
    else
      !
      ! "/" sur la ligne exécutable.
      !
      cllislc(ilislc)=clint(1:ibarre-1)
      cllislcc(ilislc)=clint(ibarre+1:ifinl)
    endif
  endif
  goto 635
  130   continue
  close(iulc)
endif
!
! Chargement de la liste de conversion.
!
call lisc(jpnomct,cllisconv,ilisconv,cliscfile)
!
! A titre informatif pour l'utilisateur,
! on teste si tous les champs a tracer
! sont bien tracables.
!
do jlislc=1,ilislc
  ioklisconv=0
  do jlisconv=1,ilisconv
    if(cllisconv(jlisconv)(05:17) == cllislc(jlislc) &
&     .and.cllisconv(jlisconv)(1:1) /= '''' &
&     .and.cllisconv(jlisconv)(1:1) /= 'X') then
      
      !-------------------------------------------------
      ! Le champ courant cllislc(jlislc) de la liste utilisateur est présent dans la liste de conversion : cllisconv(jlisconv).
      !-------------------------------------------------
      ioklisconv=jlisconv
    endif
  enddo ! jlisconv
  if(ioklisconv == 0) then
    print*,trim(cllislc(jlislc)),': DDHI/WARNING: this article is not in the above conversion list lisc'
    !
    !-------------------------------------------------
    ! On ajoute cet article dans la liste de conversion locale.
    !-------------------------------------------------
    !
    llreco=.false.
    if(cllislc(jlislc)(1:2) == 'FQ') then
      llreco=.true.
      !
      !-------------------------------------------------
      ! Cas des champs de type flux de QV, QL, QI, etc.
      !-------------------------------------------------
      !
      clfctct='P' ! inutile à ddhi.
      clfct2c='I' ! opérations de 2 champs.
      clfcttv='T' ! traitement de la verticale.
      clfctno='P' ! normalisation (aucune, par VPP0, par VPP1, etc).
      clmnemo=trim(cllislc(jlislc)) ! mnémonique du champ (son nom dans la liste d'entrée à ddhi).
      clnom1=trim(cllislc(jlislc)) ! nom du champ1 dans le fichier de DDH.
      clnom2=' ' ! nom du champ2 dans le fichier de DDH. 
      cllegfi=cllislc(jlislc)(2:3)//' FLUX, PROCESS: '//cllislc(jlislc)(4:13) ! nom en clair du champ dans le tracé de sortie.
      clunite='g/kg/day'
      zcoefmu=86400000.000
      iechlog=0
    elseif(cllislc(jlislc)(1:2) == 'FC') then
      llreco=.true.
      !
      !-------------------------------------------------
      ! Cas des champs de type flux d'enthalpie.
      !-------------------------------------------------
      !
      clfctct='P' ! inutile à ddhi.
      clfct2c='I' ! opérations de 2 champs.
      clfcttv='T' ! traitement de la verticale.
      clfctno='P' ! normalisation (aucune, par VPP0, par VPP1, etc).
      clmnemo=trim(cllislc(jlislc)) ! mnémonique du champ (son nom dans la liste d'entrée à ddhi).
      clnom1=trim(cllislc(jlislc)) ! nom du champ1 dans le fichier de DDH.
      clnom2=' ' ! nom du champ2 dans le fichier de DDH. 
      cllegfi=cllislc(jlislc)(2:3)//' FLUX, PROCESS: '//cllislc(jlislc)(4:13) ! nom en clair du champ dans le tracé de sortie.
      clunite='K/day'
      zcoefmu=85.99506148113663
      iechlog=0
    elseif(cllislc(jlislc)(1:1) == 'V') then
      llreco=.true.
      !
      !-------------------------------------------------
      ! Variable non reconnue. Traitement standard.
      !-------------------------------------------------
      !
      clfctct='V' ! inutile à ddhi.
      clfct2c='I' ! opérations de 2 champs.
      clfcttv='I' ! traitement de la verticale.
      clfctno='I' ! normalisation: aucune.
      clmnemo=trim(cllislc(jlislc)) ! mnémonique du champ (son nom dans la liste d'entrée à ddhi).
      clnom1=trim(cllislc(jlislc)) ! nom du champ1 dans le fichier de DDH.
      clnom2=' ' ! nom du champ2 dans le fichier de DDH. 
      if(cllislc(jlislc)(4:4) == '0') then
        cllegfi=cllislc(jlislc)(2:3)//' : INITIAL VALUE' ! nom en clair du champ dans le tracé de sortie.
      else
        cllegfi=cllislc(jlislc)(2:3)//' : FINAL VALUE' ! nom en clair du champ dans le tracé de sortie.
      endif
      clunite='unknown'
      zcoefmu=1.
      iechlog=0
    elseif(cllislc(jlislc)(1:1) == 'F') then
      llreco=.true.
      !
      !-------------------------------------------------
      ! Flux non reconnu. Traitement standard.
      !-------------------------------------------------
      !
      clfctct='P' ! inutile à ddhi.
      clfct2c='I' ! opérations de 2 champs.
      clfcttv='T' ! traitement de la verticale.
      clfctno='P' ! normalisation: par PPP.
      clmnemo=trim(cllislc(jlislc)) ! mnémonique du champ (son nom dans la liste d'entrée à ddhi).
      clnom1=trim(cllislc(jlislc)) ! nom du champ1 dans le fichier de DDH.
      clnom2=' ' ! nom du champ2 dans le fichier de DDH. 
      cllegfi=cllislc(jlislc)(2:3)//' FLUX, PROCESS: '//cllislc(jlislc)(4:13) ! nom en clair du champ dans le tracé de sortie.
      clunite='unknown/day'
      zcoefmu=86400.
      iechlog=0
    elseif(cllislc(jlislc)(1:1) == 'T') then
      llreco=.true.
      !
      !-------------------------------------------------
      ! Tendance non reconnue.
      !-------------------------------------------------
      !
      clfctct='P' ! inutile à ddhi.
      clfct2c='I' ! opérations de 2 champs.
      clfcttv='I' ! traitement de la verticale.
      clfctno='P' ! normalisation: par PPP.
      clmnemo=trim(cllislc(jlislc)) ! mnémonique du champ (son nom dans la liste d'entrée à ddhi).
      clnom1=trim(cllislc(jlislc)) ! nom du champ1 dans le fichier de DDH.
      clnom2=' ' ! nom du champ2 dans le fichier de DDH. 
      cllegfi=cllislc(jlislc)(2:3)//' TEND, PROCESS: '//cllislc(jlislc)(4:13) ! nom en clair du champ dans le tracé de sortie.
      if(cllislc(jlislc)(2:3) == 'CT') then
        clunite='K/day'
        zcoefmu=86400./1004.709
        iechlog=0
      elseif(cllislc(jlislc)(2:2) == 'Q') then
        clunite='g/kg/day'
        zcoefmu=86400.*1000.
        iechlog=0
      else
        clunite='unknown'
        zcoefmu=1.
        iechlog=0
      endif
    endif
    if(llreco) then
      ilisconv=ilisconv+1
      write(cllisconv(ilisconv),fmt=1400) clfctct,clfct2c,clfcttv, &
      &     clfctno,clmnemo,clnom1,clnom2,cllegfi,clunite, &
      &     zcoefmu,iechlog
    endif ! llreco
  endif ! ioklisconv
enddo ! jlislc
!
! Boucle sur les champs a tracer.
!
do jlisconv=1,ilisconv
  !
  ! On teste si le nom du champ est bien l'un de ceux
  ! demandes par l'utilisateur (tableau cllislc)
  !
  ioklisconv=0
  ioklislc=0
  do jlislc=1,ilislc
    if(cllisconv(jlisconv)(05:17) == cllislc(jlislc) &
&     .and.cllisconv(jlisconv)(1:1) /= '''' &
&     .and.cllisconv(jlisconv)(1:1) /= 'X') then
      ioklisconv=jlisconv
      ioklislc=jlislc
    endif
  enddo
  if(ioklisconv > 0) then
    !
    !-------------------------------------------------
    ! Champ à tracer: type VCT0, FCTTUR, etc...
    !-------------------------------------------------
    !
    read(cllisconv(ioklisconv),fmt=1400) clfctct,clfct2c,clfcttv, &
    &     clfctno,clmnemo,clnom1,clnom2,cllegfi,clunite, &
    &     zcoefmu,iechlog
 1400     format(4a1,3a13,a60,a15,f18.6,i3)
    !
    ! Legende des opérateurs de travail sur les champs:
    ! 1 clfctct: inutile pour ddhi.
    ! 2 clfct2c: opérations de deux champs.
    ! .  I: aucune
    ! .  S: c1+c2
    ! .  D: c2-c1
    ! .  F: (c2-c1)/(t tendances (article ECHEANCE))
    ! .  G: (c2-c1)/(t variables (article ECHEANCEDV))
    ! .  N: c2/vpp1-c1/vpp0
    ! .  T: (c2/vpp1-c1/vpp0)/(t tendances (article ECHEANCE))
    ! .  U: (c2/vpp1-c1/vpp0)/(t variables (article ECHEANCEDV))
    ! .  R: c2/c1
    ! 3 clfcttv: traitement de la verticale.
    ! .  I: aucun
    ! .  C: cumul selon la verticale
    ! .  T: calcul de l'ecart (niveau)-(niveau-1)
    ! .  F: écart niveau JLEV - niveau 0
    ! 4 clfctno: normalisation.
    ! .  I: aucune
    ! .   0: VPP0
    ! .   1: VPP1
    ! .   P: PPP
    ! .   M: PPP/(t tendances (article ECHEANCE))
    ! .   D: t tendances (article ECHEANCE)
    ! .   V: t variables (article ECHEANCEDV)
    ! .   X: (PPP/t_tendances)/(-0.5*(Vxx0/VPP0+Vxx1/VPP1))
    ! 5 clmnemo: mnémonique du champ (son nom dans la liste d'entrée à ddhi).
    ! clnom1: nom du champ1 dans le fichier de DDH.
    ! clnom2: nom du champ2 dans le fichier de DDH.
    ! cllegfi: nom en clair du champ dans le tracé de sortie.
    ! clunite: unité du champ de sortie.
    ! zcoefmu: coefficient multiplicatif à appliquer pour obtenir l'unité ci-dessus.
    ! iechlog: puissance de 10 à appliquer.
    !
    if((clmnemo(2:3) == 'A1'.and.idocfi(3) == 0) &
&     .or.(clmnemo(2:3) == 'A2'.and.idocfi(3) == 0) &
&     .or.(clmnemo(2:3) == 'A3'.and.idocfi(3) == 0) &
&     .or.(clmnemo(2:3) == 'SS'.and.idocfi(4) == 0)) then
      !
      ! L'utilisateur a demandé de traiter
      ! des champs qui, d'après l'autodocumentation
      ! du fichier, ne peuvent y être.
      ! Afin de ne pas lancer un "lfalecr" inutile,
      ! on passe directement au champ suivant.
      !
      print*,'DDHI/WARNING: field ',clmnemo(1:13),' ignored.'
      goto 338
    endif
    if(llbil) then
      !
      ! Mode bilan.
      !
      ! zcoefmu=1.
      ! iechlog=0
      ! clunite='SI'
      ! if(lldebug) write(78,*) 'Coefs bilan'
    elseif(clconvu == 'NON') then
      !
      ! L'utilisateur veut les valeurs brutes du fichier
      !
      clfcttv='I'
      clfctno='I'
      zcoefmu=1.
      iechlog=0
      clunite='SI'
    elseif(clconvu == 'SOL'.or.clconvu == 'SOM') then
      !
      ! L'utilisateur veut les valeurs brutes du fichier
      !
      if(clmnemo(1:1) == 'F') then
        clfcttv='I'
        clfctno='D'
        zcoefmu=1.
        iechlog=0
        clunite='SI'
      elseif(clmnemo(1:1) == 'V') then
      elseif(clmnemo(1:1) == 'T') then
      elseif(clmnemo(1:1) == 'P') then
      else
        print*,'DDHI/ERREUR: type de mnémonique inconnu!...'
        print*,clmnemo
                                call exit(1)
      endif
    elseif(clconvu == 'OUI') then
    else
      print*,'DDHI/ERREUR: type de conversion inconnu!...'
      print*,clconvu
                        call exit(1)
    endif
    print*,trim(clmnemo),': processing done:'
    if(trim(clfcttv) == 'I') then
      print*,'  vertical processing: none'
    elseif(trim(clfcttv) == 'C') then
      print*,'  vertical processing: cumulate'
    elseif(trim(clfcttv) == 'T') then
      print*,'  vertical processing: differenciate jlev - (jlev-1)'
    elseif(trim(clfcttv) == 'T') then
      print*,'  vertical processing: differenciate jlev - (level 0)'
    endif
    if(trim(clfctno) == 'I') then
      print*,'  normalization: none'
    elseif(trim(clfctno) == '0') then
      print*,'  normalization: VPP0'
    elseif(trim(clfctno) == '1') then
      print*,'  normalization: VPP1'
    elseif(trim(clfctno) == 'P') then
      print*,'  normalization: PPP'
    elseif(trim(clfctno) == 'M') then
      print*,'  normalization: PPP/(t article ECHEANCE)'
    elseif(trim(clfctno) == 'D') then
      print*,'  normalization: t article ECHEANCE'
    elseif(trim(clfctno) == 'V') then
      print*,'  normalization: t variables, article ECHEANCEDV'
    elseif(trim(clfctno) == 'X') then
      print*,'  normalization: (PPP/t_tendencies)/(-0.5*(Vxx0/VPP0+Vxx1/VPP1))'
    endif
    print*,'  multiplicative factor applied: ',zcoefmu
    print*,'  unit=',trim(clunite)
    if(iechlog /= 0) print*,'  log scale applied: ',iechlog
    !
    !-------------------------------------------------
    ! Lecture de l'article sur fichier LFA.
    !-------------------------------------------------
    !
    call lfalecr(iul1,clnom1,jpprod,zmoyz1,ilong,irep)
    ilevm=ilong
    if(irep /= 0) then
      !
      ! Article inexistant.
      !
      goto 338
    endif
    !
    ! --------------------------------------------------------------------------
    !
    ! Conversion d'unites, cumul vertical, etc...
    !
    ! Fonctions de deux champs
    !
    if(clfct2c /= 'I') then
      ! Il faut faire la combinee de deux champs
      ! Lecture du deuxieme champ
      cladcou(1:16)=clnom2
      call lfalecr(iul1,cladcou,jpprod,zmoyz2,ilong,irep)
      if(irep /= 0) then
        !
        ! Article inexistant.
        !
        goto 338
      endif
      ilevm=ilong
      if(clfct2c == 'S') then
        !
        ! Il faut faire la somme de deux champs
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=zmoyz1(jlev)+zmoyz2(jlev)
          if(lldebug) write(78,*) 'zone somme jlev=',jlev,' zmoyz1=',zmoyz1(jlev),&
                                        &' zmoyz2=',zmoyz2(jlev),' zmoyz3=',zmoyz3(jlev)
        enddo
        cloutfd=' +'
      elseif(clfct2c == 'D') then
        !
        ! Il faut faire la difference de deux champs
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=zmoyz1(jlev)-zmoyz2(jlev)
          if(lldebug) write(78,*) 'zone diff jlev=',jlev,' zmoyz1=',zmoyz1(jlev),&
                                        &' zmoyz2=',zmoyz2(jlev),' zmoyz3=',zmoyz3(jlev)
        enddo
        cloutfd=' -D'
      elseif(clfct2c == 'F') then
        !
        ! Il faut faire la difference de deux champs
        ! puis diviser par la durée du run.
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=(zmoyz1(jlev)-zmoyz2(jlev))/zech(1)
          if(lldebug) write(78,*) 'zone diff jlev=',jlev,' zmoyz1=',zmoyz1(jlev),&
                                        &' zmoyz2=',zmoyz2(jlev),' zmoyz3=',zmoyz3(jlev)
        enddo
        cloutfd=' -F'
      elseif(clfct2c == 'G') then
        !
        ! Il faut faire la difference de deux champs
        ! puis diviser par la durée du run (écarts de variables).
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=(zmoyz1(jlev)-zmoyz2(jlev))/zechdv(1)
          if(lldebug) write(78,*) 'zone diff jlev=',jlev,' zmoyz1=',zmoyz1(jlev),&
                                        &' zmoyz2=',zmoyz2(jlev),' zmoyz3=',zmoyz3(jlev)
        enddo
        cloutfd=' -G'
      elseif(clfct2c == 'N') then
        !
        ! Il faut faire la difference des deux champs,
        ! normalisés respectivement par VPP1 et VPP0.
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=zmoyz1(jlev)/zpre1(jlev)-zmoyz2(jlev)/zpre0(jlev)
        enddo
        cloutfd=' -N'
      elseif(clfct2c == 'T') then
        !
        ! Il faut faire la difference des deux champs,
        ! normalisés respectivement par VPP1 et VPP0,
        ! puis par la durée du run.
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=(zmoyz1(jlev)/zpre1(jlev)-zmoyz2(jlev)/zpre0(jlev))/zech(1)
        enddo
        cloutfd=' -T'
      elseif(clfct2c == 'U') then
        !
        ! Il faut faire la difference des deux champs,
        ! normalisés respectivement par VPP1 et VPP0,
        ! puis par la durée du run (durée des écarts de variables).
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=(zmoyz1(jlev)/zpre1(jlev)-zmoyz2(jlev)/zpre0(jlev))/zechdv(1)
        enddo
        cloutfd=' -U'
      elseif(clfct2c == 'R') then
        !
        ! Il faut faire le rapport de deux champs
        !
        do jlev=1,ilevm
          zmoyz3(jlev)=zmoyz1(jlev)/zmoyz2(jlev)
          if(lldebug) write(78,*) 'zone rapp jlev=',jlev,' zmoyz1=',zmoyz1(jlev),&
                                        &' zmoyz2=',zmoyz2(jlev),' zmoyz3=',zmoyz3(jlev)
        enddo
        cloutfd=' /'
      else
        print*,trim(cllisconv(ioklisconv))
        call stoperr('Combinee de champs inconnue!...')
      endif
    else
      !
      ! Pas de fct de deux champs a executer
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz1(jlev)
      enddo
      cloutfd='ABSENCE'
    endif
    !
    ! Traitement de la verticale
    !
    if(ilong == ilev(1)*idom(1)) then
      imodv=ilev(1)
    else
      imodv=ilev(1)+1
    endif
    if(clfcttv == 'C') then
      !
      ! Cumul selon la verticale.
      !
      do jlev=1,ilevm
        imod=mod(jlev-1,imodv)
        if(imod /= 0) then
          zmoyz3(jlev)=zmoyz3(jlev)+zmoyz3(jlev-1)
          if(lldebug) write(78,*) 'zone vert C jlev=',jlev,' zmoyz3=',zmoyz3(jlev)
        endif
      enddo
      clouttv=' (cumul sommet>niveau)*C'
      ilevm2=ilevm
    elseif(clfcttv == 'T') then
      !
      ! Calcul de l'ecart (niveau)-(niveau-1)
      !
      iecr=0
      do jlev=1,ilevm
        imod=mod(jlev-1,imodv)
        if(imod /= 0) then
          iecr=iecr+1
          zmoyz3(iecr)=-zmoyz3(jlev)+zmoyz3(jlev-1)
          if(lldebug) write(78,*) 'zone vert T jlev=',jlev,' zmoyz3=',zmoyz3(jlev)
        endif
      enddo
      clouttv=' (ecart NivJ - NivJ-1)*C'
      ilevm2=iecr
    elseif(clfcttv == 'F') then
      !
      ! Ecart niveau JLEV - niveau 0
      !
      iecr=0
      do jlev=1,ilevm
        ipos=jlev-1-mod(jlev-1,imodv)+1
        ! zvalsom: valeur du flux au sommet du domaine courant.
        if(ipos == jlev) zvalsom=zmoyz3(ipos)
        iecr=iecr+1
        zmoyz3(iecr)=zmoyz3(jlev)-zvalsom
        if(lldebug) write(78,*) 'zone vert F jlev=',jlev,' zmoyz3=',zmoyz3(jlev)
      enddo
      clouttv=' (ecart NivJ - Niv0)*C'
      ilevm2=iecr
    else
      !
      ! Rien a faire
      !
      clouttv='ABSENCE'
      ilevm2=ilevm
    endif
    ilevm=ilevm2
    !
    ! Multiplication par coef et echelle
    !
    zechelle=10.**real(iechlog)*zcoefddhi
    zcmu=zcoefmu*zechelle
                if ((clmnemo(1:4) == 'VCT0').or.(clnom1(1:4) == 'VCT0')) then
                        do jlev=1,ilevm
                                zmoyz3(jlev)=zmoyz3(jlev)*(rcpd/zrcp0(jlev))*zcmu
                       enddo 
                elseif ((clmnemo(1:4) == 'VCT1').or.(clnom1(1:4) == 'VCT1')) then
                       do jlev=1,ilevm
                                zmoyz3(jlev)=zmoyz3(jlev)*(rcpd/zrcp1(jlev))*zcmu
                       enddo 
                else
            do jlev=1,ilevm
              zmoyz3(jlev)=zmoyz3(jlev)*zcmu
              if(lldebug) write(78,*) 'zone mult coef jlev=',jlev,' zmoyz3=',zmoyz3(jlev),&
                                &' zcmu=',zcmu
            enddo
                endif
    !
    ! Normalisation par VPP0 ou VPP1
    !
    if(clfctno == '0') then
      !
      ! Normalisation par VPP0 (champ de masse instant initial).
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zpre0(jlev)
        if(lldebug) write(78,*) 'zone norm VPP0 jlev=',jlev,' zmoyz3=',zmoyz3(jlev),&
                                &' zpre0=',zpre0(jlev)
      enddo
      cloutno='MASSE INITIALE'
    elseif(clfctno == '1') then
      !
      ! Normalisation par VPP1 (champ de masse instant final).
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zpre1(jlev)
        if(lldebug) write(78,*) 'zone norm VPP1 jlev=',jlev,' zmoyz3=',zmoyz3(jlev),&
                                &' zpre1=',zpre1(jlev)
      enddo
      cloutno='MASSE FINALE'
    elseif(clfctno == 'P') then
      !
      ! Normalisation par PPP (champ de masse cumulee dans le temps).
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zprec(jlev)
        if(lldebug) write(78,*) 'zone norm PPP jlev=',jlev,' zmoyz3=',zmoyz3(jlev),&
                                &' zprec=',zprec(jlev)
      enddo
      cloutno='MASSE CUMULEE'
    elseif(clfctno == 'M') then
      !
      ! Normalisation par PPP/ECHEANCE  (champ de masse moyenne dans le temps)
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zprec(jlev)*zech(1)
        if(lldebug) write(78,*) 'zone norm PPPech jlev=',jlev,' zmoyz3=',zmoyz3(jlev),&
                                &' zprec=',zprec(jlev),' zech(1)=',zech(1)
      enddo
      cloutno='MASSE MOYENNE'
    elseif(clfctno == 'D') then
      !
      ! On normalise par la duree du run.
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zech(1)
        if(lldebug) write(78,*) 'zone norm ech jlev=',jlev,' zmoyz3=',zmoyz3(jlev),' zech(1)=',zech(1)
      enddo
      cloutno='DUREE RUN'
    elseif(clfctno == 'V') then
      !
      ! On normalise par la duree du run (écarts de variables).
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)/zechdv(1)
        if(lldebug) write(78,*) 'zone norm ech jlev=',jlev,' zmoyz3=',zmoyz3(jlev),' zechdv(1)=',zechdv(1)
      enddo
      cloutno='DUREE RUN variables'
    elseif(clfctno == 'X') then
      !
      ! On normalise par le champ (PPP/t_tendances)/(-0.5*(Vxx0/VPP0+Vxx1/VPP1))
      !
      ! Lecture de Vxx0.
      !
      write(cladcou,fmt='(3a)') 'V',clmnemo(2:3),'0'
      call lfalecr(iul1,cladcou,jpprod,zmoyz1,ilong,irep)
      !
      ! Lecture de Vxx1.
      !
      write(cladcou,fmt='(3a)') 'V',clmnemo(2:3),'1'
      call lfalecr(iul1,cladcou,jpprod,zmoyz2,ilong,irep)
      !
      ! Normalisation.
      !
      do jlev=1,ilevm
        zmoyz3(jlev)=zmoyz3(jlev)*(-0.5)*(zmoyz1(jlev)/zpre0(jlev)+zmoyz2(jlev)/zpre1(jlev))/(zprec(jlev)/zech(1))
      enddo
      if(lldebug) write(78,*) 'zone norm X'
      cloutno='-0.5*(Vxx0/VPP0+Vxx1/VPP1)'
    else
      !
      ! Pas de normalisation.
      !
      cloutno='ABSENCE'
    endif
    !
    ! Valeur additive demandée par l'utilisateur.
    !
    do jlev=1,ilevm
      zmoyz3(jlev)=zmoyz3(jlev)+zadd_ddhi
    enddo
    !
    ! --------------------------------------------------------------------------
    !
    ! Cumul statistique.
    ! On initialise a 0. les valeurs de cumul spatial
    !
    zsomx=0.
    zsomx2=0.
    zmax=-1.e20
    zmin=-zmax
    zsommas=0.
    if(ilevm == ilev(1)*idom(1)) then
      !
      ! Le champ qu'on somme est sur les niveaux modele.
      ! On va le sommer en 2D (vertical, horizontal)
      ! en le ponderant par la masse.
      !
      zminmme(5)=0.
      itypmoy=1
      do jlev=1,ilevm
        zx=zmoyz3(jlev)
        if(clmnemo(4:4) == '0') then
          !
          ! Le poids est la masse initiale.
          !
          zpoi=zpre0(jlev)
        elseif(clmnemo(4:4) == '1') then
          !
          ! Le poids est la masse finale.
          !
          zpoi=zpre1(jlev)
        else
          !
          ! Le poids est la masse cumulee.
          !
          zpoi=zprec(jlev)
        endif
        zsomx=zsomx+zx*zpoi
        zsomx2=zsomx2+zx*zx*zpoi
        zsommas=zsommas+zpoi
        zmax=max(zmax,zx)
        zmin=min(zmin,zx)
      enddo
    else
      !
      ! Le champ qu'on somme est sur les inter-niveaux modele.
      ! On va sommer en 1D (horizontal) sa valeur au sol.
      ! Les sommations se font avec le meme poids tant
      ! pour les bandes zonales (equisurfaces) que pour
      ! les domaines limites.
      !
      zminmme(5)=1.
      itypmoy=2
      imodv=ilev(1)+1
      do jlev=1,ilevm
        imod=mod(jlev,imodv)
        zx=zmoyz3(jlev)
        if(imod == 0) then
          zsomx=zsomx+zx
          zsomx2=zsomx2+zx*zx
          zsommas=zsommas+1.
        endif
        zmax=max(zmax,zx)
        zmin=min(zmin,zx)
      enddo
    endif
    !
    ! --------------------------------------------------------------------------
    !
    ! Ecriture des resultats et cumuls.
    !
    ! Calcul des valeurs de moyenne, etc...
    !
    zmoy=zsomx/zsommas
    zect=sqrt(max(0.,zsomx2/zsommas &
&     -(zsomx/zsommas)*(zsomx/zsommas)))
    !
    ! Impression des resultats
    !
    if(lldebug) then
    write(iulm,*) '-------------------------------------------------'
    write(iulm,fmt=*   ) 'Fichier :    ',clficd(1:index(clficd,' '))
    write(iulm,fmt='(20a)') clfctct,clfct2c,clfcttv, &
&     clfctno,clmnemo,clnom1,clnom2,cllegfi,clunite, &
&     zcoefmu,iechlog
    write(iulm,fmt=*   ) 'Champ   :    ',cllegfi
    write(iulm,fmt=*   ) 'Unite   :    ',clunite
    write(iulm,fmt=*   ) 'Article1:    ',clnom1
    write(iulm,fmt=*   ) 'Article2:    ',clnom2
    write(iulm,fmt=*   ) 'Fct 2 champs : '//cloutfd
    write(iulm,fmt=*   ) 'Verticale    : '//clouttv
    write(iulm,fmt=*   ) 'Normalisation: '//cloutno
    write(iulm,fmt=*   ) 'Coef mul base: ',zcoefmu
    write(iulm,fmt=*   ) 'Coef mul util: ',zcoefddhi
    write(iulm,fmt=*   ) 'Valeur add   : ',zadd_ddhi
    write(iulm,fmt=*   ) 'Echelle      : ',zechelle
    if(itypmoy == 1) then
      !
      ! Moyenne ponderee par la masse.
      !
      write(iulm,fmt=*) 'Minimum       :    ',zmin
      write(iulm,fmt=*) 'Maximum       :    ',zmax
      write(iulm,fmt=*) 'Moyenne       :    ',zmoy
      write(iulm,fmt=*) 'Ecart-type    :    ',zect
    else
      !
      ! Moyenne de surface.
      !
      write(iulm,fmt=*) 'Min           :    ',zmin
      write(iulm,fmt=*) 'Max           :    ',zmax
      write(iulm,fmt=*) 'Moy en surface:    ',zmoy
      write(iulm,fmt=*) 'EcT en surface:    ',zect
    endif
    endif
    !
    ! Ecriture des donnees converties sur le lfa de sortie.
    !
    clnomafs='LISC-'//clmnemo
    if(cllislcc(ioklislc) /= ' ') then
      !
      ! L'utilisateur a tapé dans le fichier liste
      ! des champs à tracer un champ
      ! de type "VUU0/mon vent zonal";
      ! le nom en clair du champ
      ! de la liste de conversion doit donc être
      ! substitué par celui proposé par l'utilisateur.
      !
      cllisconv(ioklisconv)(44:103)=cllislcc(ioklislc)(1:60)
    endif
    call lfaecrc(iul3,clnomafs,cllisconv(ioklisconv),1)
    zminmme(1)=zmin
    zminmme(2)=zmax
    zminmme(3)=zmoy
    zminmme(4)=zect
    zminmme(6)=zechelle
    clnomafs='MINM-'//clmnemo
    call lfaecrr(iul3,clnomafs,zminmme,6)
    clnomafs='TRAC-'//clmnemo
    call lfaecrr(iul3,clnomafs,zmoyz3,ilevm)
  else
    !
    ! On ne fait rien car on ne traite pas ici
    ! les eventuels commentaires de la liste.
    !
  endif
  338   continue
enddo
if(clconvs == 'OUI') then
  !
  !-------------------------------------------------
  ! Traitement des champs au sol.
  !-------------------------------------------------
  !
  print*,'CHAMPS SOL'
  ! On rend fatale toute erreur de lecture du fichier.
  !
  call lfaerf(iul1,.true.)
  !
  ! Traitement des variables.
  !
  inomvs=idocfi(12) ! nombre de variables initiales ou finales.
  do jvar=0,1
    !
    ! Lecture du champ de proportion de surface.
    !
    write(clsol,fmt='(a,i2.2,a,i1.1)') 'S',1,'_',jvar
    idom_scal=idom(1)
    call lfalecr(iul1,clsol,idom_scal,zpros,ilong,irep)
    !
    ! Ecriture des min/max/moy et unite du champ.
    !
    call ecrm(iul3,iulm,lldebug,clsol,zpros,ilong,'SANS')
    do jchamp=2,inomvs
      write(clsol,fmt='(a,i2.2,a,i1.1)') 'S',jchamp,'_',jvar
      idom_scal=idom(1)
      call lfalecr(iul1,clsol,idom_scal,zsols,ilong,irep)
      if(ilong /= idom(1)) then
        print*,'DDHI/ERREUR interne: ilong!...'
        call exit(1)
      endif
      if(clconvu == 'OUI') then
        !
        ! Conversion vers unites en clair.
        !
        if(jchamp == 1) then
          zcoec=1.
          clunite='SANS'
        elseif(jchamp == 2) then
          !
          ! Conversion vers degres K.
          !
          zcoec=1.e-5 ! valeur du hsol de la physique.
          clunite='K'
        elseif(jchamp == 3) then
          !
          ! Conversion vers degres K.
          !
          zcoec=1.e-5 /5. ! valeur de hsol/rtiner de la physique.
          clunite='K'
        elseif(jchamp == 4.or.jchamp == 5 &
&         .or.jchamp == 6) then
          zcoec=1.
          clunite='KG/M**2'
        else
          print*,'DDHI/ERREUR interne: jchamp!...'
          call exit(1)
        endif
        do jdom=1,idom(1)
          if(zpros(jdom) /= 0) then
            zvc=zsols(jdom)*zcoec/zpros(jdom)
            if(llbil) write(iulb,'(a,1x,i5,a,i1,i5,3F20.10)') 'SOL',jdom,'  VARI' &
&             ,jvar,jchamp, zsols(jdom),zpros(jdom),zvc
            zsols(jdom)=zsols(jdom)*zcoec/zpros(jdom)
          else
            print*,'Domaine numero ',jdom,' de surface terrestre nulle!...'
          endif
        enddo
      else
        !
        ! Pas de conversion.
        !
        clunite='SI'
      endif
      !
      ! Ecriture des min/max/moy et unite du champ.
      !
      idom_scal=idom(1)
      call ecrm(iul3,iulm,lldebug,clsol,zsols,idom_scal,clunite)
    enddo
  enddo
  !
  ! Traitement des flux.
  !
  inomvs=idocfi(14) ! nombre de variables initiales ou finales.
  do jchamp=1,inomvs
    write(clsol,fmt='(a,i2.2)') 'G',jchamp
    idom_scal=idom(1)
    call lfalecr(iul1,clsol,idom_scal,zsols,ilong,irep)
    if(ilong /= idom(1)) then
      print*,'DDHI/ERREUR interne: ilong!...'
      call exit(1)
    endif
    if(clconvu == 'OUI') then
      !
      ! Conversion vers unites en clair.
      !
      do jdom=1,idom(1)
        if(zpros(jdom) /= 0) then
          zvc=zsols(jdom)/zech(1)/zpros(jdom)
          if(llbil) write(iulb,'(a,1x,i5,a,i5,3F20.10)') 'SOL ',jdom,'  FLUX' &
&           ,jchamp, zsols(jdom),zpros(jdom),zvc
          zsols(jdom)=zsols(jdom)/zech(1)/zpros(jdom)
        else
          print*,'Domaine numero ',jdom,' de surface terrestre nulle!...'
        endif
      enddo
      if(jchamp <= 7) then
        clunite='W/M**2'
      elseif(jchamp <= 17) then
        clunite='KG/M**2/S'
      else
        print*,'DDHI/ERREUR interne: champf!...'
        call exit(1)
      endif
    else
      clunite='SI'
    endif
    idom_scal=idom(1)
    call ecrm(iul3,iulm,lldebug,clsol,zsols,idom_scal,clunite)
  enddo
  !
  !-------------------------------------------------
  ! L'utilisateur a demandé d'interpréter les champs libres (vent à 10m, etc...).
  !-------------------------------------------------
  !
  print*,'CHAMPS LIBRES'
  !
  ! Traitement des variables.
  !
  do jvar=1,idocfi(16) ! idocfi(16): nombre de variables type "champs libres".
    !
    ! Lecture du champ libre.
    !
    write(clchal,fmt='(a,i2.2)') 'SVGFS',jvar
    idom_scal=idom(1)
    call lfalecr(iul1,clchal,idom_scal,zpros,ilong,irep)
    !
    ! Ecriture des min/max/moy et unite du champ.
    !
    call ecrm(iul3,iulm,lldebug,clchal,zpros,idom_scal,'???')
  enddo
  !
  ! Traitement des flux.
  !
  do jvar=1,idocfi(17) ! idocfi(17): nombre de variables type "flux libres".
    !
    ! Lecture du champ libre.
    !
    write(clchal,fmt='(a,i2.2)') 'SFGFS',jvar
    idom_scal=idom(1)
    call lfalecr(iul1,clchal,idom_scal,zpros,ilong,irep)
    !
    ! Ecriture des min/max/moy et unite du champ.
    !
    call ecrm(iul3,iulm,lldebug,clchal,zpros,idom_scal,'???')
  enddo
endif
if(lldebug) close(iulm)
!
! Fermeture du fichier DDH d'entree
!
call lfafer(iul1)
!
! Appel a l'ecriture finale.
!
!
! Initialisation.
!
print*,'----------------------'
print*,'---DDHI-COORDONNEES---'
print*,'----------------------'
if(llbil) print*,'Mode bilan en cours...'
!
! Unites logiques.
!
iulsor=5
!
! Fermeture du lfa intermediaire.
!
call lfafer(iul3)
iulent=iul3
call lfaouv(iulent,'.ddhi.tmp','R')
iultit=7
!
! Etablissement du niveau "bavard" de messagerie (cas de debug).
!
!if(lldebug) call lfames(iulent,2)
!
! Articles de documentation.
!
call lfalecc(iulent,'CONVERSION EFFECTUEE',1,clconv,ilong,irep)
call lfalecc(iulent,'INDICE EXPERIENCE',1,clnamx,ilong,irep)
ilnamx=len_trim(clnamx)
call lfaleci(iulent,'DATE',11,idatef,ilong,irep)
call lfaleci(iulent,'TYPE DE FICHIER',1,itype,igol,irep)
call lfaleci(iulent,'NOMBRE DE NIVEAUX VERTICAUX',1,ilev,igol,irep)
call lfaleci(iulent,'NOMBRE DE DOMAINES',1,idom,igol,irep)
imaxv=ilev(1)*idom(1)
imaxf=(ilev(1)+1)*idom(1)
call lfalecr(iulent,'ECHEANCE',1,zech,igol,irep)
!
! Documentation des domaines.
!
do jdom=1,idom(1)
  call lited(iulent,jdom,'R',zdocd,irep)
  do jdoc=1,jpdoc
    zdocdf(jdoc,jdom)=zdocd(jdoc)
  enddo
enddo
!
! Traitement d'un problème historique:
! le code de ddht opérationnel n'écrivant
! que des 0 dans l'article DOCD des fichiers de sortie,
! on recrée ici l'information de façon approchée
! pour les fichiers zonaux. Ces fichiers "buggés"
! restent ainsi lisibles par ddhi.
!
iokdoc=0
if(itype(1) == 6) then
  !
  ! Fichier zonal.
  !
  do jdom=1,idom(1)
    if(zdocdf(4,jdom) /= 0.) iokdoc=1
  enddo
  if(iokdoc == 0) then
    !
    ! On est en présence d'un fichier de DDH zonaux
    ! dont tous les sinus de latitudes sont nuls.
    ! Ce fichier de DDH est "buggé"!...
    !
    print*,'DDHI/WARNING: zonal file has DOCD article equal to zero !...'
    !
    ! On initialise les sinus des latitudes.
    !
    do jdom=1,idom(1)
      zdocdf(4,jdom)=1.-(real(jdom)-0.5)*2./real(idom(1))
    enddo
  endif
endif
!
!-------------------------------------------------
! Impression de ps et de Z500.
!-------------------------------------------------
!
llps=.false.
if(llps) then
  call ps_et_z500(zcoop1f,zcooz1v,zcoop1v &
  & ,zcooz0v,zcoop0v &
  & ,ilev,idom,zdocdf,clfics)
endif
!
! Lecture des coordonnees verticales.
!
! On veut une coordonnee pression.
!
call lfalecr(iulent,'COOR-P0V',imaxv,zcoop0vu,ilongv,irep)
call lfalecr(iulent,'COOR-P1V',imaxv,zcoop1vu,ilongv,irep)
call lfalecr(iulent,'COOR-PCV',imaxv,zcoopcvu,ilongv,irep)
call lfalecr(iulent,'COOR-P0F',imaxf,zcoop0fu,ilongf,irep)
call lfalecr(iulent,'COOR-P1F',imaxf,zcoop1fu,ilongf,irep)
call lfalecr(iulent,'COOR-PCF',imaxf,zcoopcfu,ilongf,irep)
! Pour le tracé on veut p croissant vers le haut!...
do jb=1,ilongv
  zcoop0vu(jb)=-zcoop0vu(jb)
  zcoop1vu(jb)=-zcoop1vu(jb)
  zcoopcvu(jb)=-zcoopcvu(jb)
enddo
do jb=1,ilongf
  zcoop0fu(jb)=-zcoop0fu(jb)
  zcoop1fu(jb)=-zcoop1fu(jb)
  zcoopcfu(jb)=-zcoopcfu(jb)
enddo
!
! On veut une coordonnee Z.
!
call lfalecr(iulent,'COOR-Z0V',imaxv,zcooz0vu,ilongv,irep)
call lfalecr(iulent,'COOR-Z1V',imaxv,zcooz1vu,ilongv,irep)
call lfalecr(iulent,'COOR-ZCV',imaxv,zcoozcvu,ilongv,irep)
call lfalecr(iulent,'COOR-Z0F',imaxf,zcooz0fu,ilongf,irep)
call lfalecr(iulent,'COOR-Z1F',imaxf,zcooz1fu,ilongf,irep)
call lfalecr(iulent,'COOR-ZCF',imaxf,zcoozcfu,ilongf,irep)
!
! On veut une coordonnee niveaux.
!
call lfalecr(iulent,'COOR-N0V',imaxv,zcoon0vu,ilongv,irep)
call lfalecr(iulent,'COOR-N1V',imaxv,zcoon1vu,ilongv,irep)
call lfalecr(iulent,'COOR-NCV',imaxv,zcooncvu,ilongv,irep)
call lfalecr(iulent,'COOR-N0F',imaxf,zcoon0fu,ilongf,irep)
call lfalecr(iulent,'COOR-N1F',imaxf,zcoon1fu,ilongf,irep)
call lfalecr(iulent,'COOR-NCF',imaxf,zcooncfu,ilongf,irep)
! Pour le tracé on veut n croissant vers le haut!...
do jb=1,ilongv
  zcoon0vu(jb)=-zcoon0vu(jb)
  zcoon1vu(jb)=-zcoon1vu(jb)
  zcooncvu(jb)=-zcooncvu(jb)
enddo
do jb=1,ilongf
  zcoon0fu(jb)=-zcoon0fu(jb)
  zcoon1fu(jb)=-zcoon1fu(jb)
  zcooncfu(jb)=-zcooncfu(jb)
enddo
!
! Liste des articles du fichier.
!
call lfalaft(iulent,clart,jpnomct,ilart)
!
! Boucle sur les articles du fichier.
!
do jart=1,ilart
  clnoma=clart(jart) ! nom d'article rencontre.
  if(clnoma(1:5) == 'LISC-'.and.clopt(1:ilopt) == 'ddhlfa2dd') then
    !
    ! Il faut generer le fichier d'autodocumentation.
    ! Lecture de la ligne de liste_conversion relative au champ.
    !
    call lfalecc(iulent,clnoma,1,clnomat,ilong,irep)
    cl3=clnomat(44:103) ! nom en clair du champ.
    il3=len_trim(cl3)
    if(clconv(1:3) == 'OUI') then
      clunit=clnomat(104:118)
      read(clnomat(137:139),fmt='(i3)') iechlog
    else
      clunit='SI'
      iechlog=0
    endif
  elseif(clnoma(1:5) == 'MINM-'.and.clopt(1:ilopt) == 'ddhlfa2dd') then
    !
    ! Il faut generer le fichier d'autodocumentation.
    ! Ecriture des extremes du champ.
    !
    call lfalecr(iulent,clnoma,6,zminmme,ilong,irep)
    zechelle=zminmme(6)
    call unech(clunit,zechelle,clunit,ilunit)
    zrcm=sqrt(max(0.,zminmme(4)*zminmme(4)+zminmme(3)*zminmme(3)))
    write(clmin,fmt='(g16.7)') zminmme(1) ; ilmin=len_trim(clmin)
    write(clmax,fmt='(g16.7)') zminmme(2) ; ilmax=len_trim(clmax)
    write(clmoy,fmt='(g16.7)') zminmme(3) ; ilmoy=len_trim(clmoy)
    write(clect,fmt='(g16.7)') zminmme(4) ; ilect=len_trim(clect)
    write(clrcm,fmt='(g16.7)') zrcm ; ilrcm=len_trim(clrcm)
  elseif(clnoma(1:5) == 'TRAC-') then
    !
    ! Lecture du champ sur le lfa.
    !
    call lfalecr(iulent,clnoma,jpprod,zchacf,itaille,irep)
    !
    ! Fichier XYZ de sortie.
    !
    if(.not.llbil) then
      write(clsord,fmt='(4a)') clfics(1:index(clfics,' ')-1),'.',clnoma(6:index(clnoma,' ')-1),'.dta'
      print*,trim(clsord)
      open(iulsor,file=clsord)
    endif
    if(itaille == ilev(1)*idom(1)) then
      !
      ! Champ de taille verticale ilev(1).
      !
      do jb=1,itaille
        !
        ! Recherche des coordonnees eventuelles.
        !
        icoo=0
        do jcoo=1,jpcoo
          ilcoo=len_trim(clcoo(jcoo))
          if(clcoo(jcoo)(1:ilcoo) == 'VP') then
            !
            ! Coordonnee verticale pression.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P'.or.clnoma(6:6) == 'Z') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoopcvu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoopcvu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcoop0vu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcoop1vu(jb)
            elseif(clnoma(9:9) == 'D'.or.clnoma(9:9) == 'M'.or.clnoma(9:9) == 'F') then
              !
              ! Coordonnee vert.: masse moyenne.
              !
              zcoo(icoo)=zcoopcvu(jb)
            else
              print*,'ERREUR: type non prevu 1!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:ilcoo) == 'VZ') then
            !
            ! Coordonnee verticale z.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoozcvu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoozcvu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcooz0vu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcooz1vu(jb)
            elseif(clnoma(9:9) == 'D'.or.clnoma(9:9) == 'M'.or.clnoma(9:9) == 'F') then
              !
              ! Coordonnee vert.: masse moyenne.
              !
              zcoo(icoo)=zcoozcvu(jb)
            else
              print*,'ERREUR: type non prevu 2!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:ilcoo) == 'VN') then
            !
            ! Coordonnee verticale niveaux.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcooncvu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcooncvu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcoon0vu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcoon1vu(jb)
            elseif(clnoma(9:9) == 'D'.or.clnoma(9:9) == 'M'.or.clnoma(9:9) == 'F') then
              !
              ! Coordonnee vert.: masse moyenne.
              !
              zcoo(icoo)=zcooncvu(jb)
            else
              print*,'ERREUR: type non prevu 3!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:1) == 'H') then
            !
            ! Coordonnees horizontales.
            !
            icoo=icoo+1
            idoml=(jb-1)/ilev(1)+1
            call cooxd(clcoo(jcoo)(2:),zdocdf,idoml,zcoo(icoo))
          elseif(clcoo(jcoo)(1:1) == 'T') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zech(1)/zcoet(jcoo)
          elseif(clcoo(jcoo)(1:2) == 'BA') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=real(idatef(1))+real(idatef(2)-1)/12.+real(idatef(3)-1)/365.2425 &
            & +real(idatef(4))/8765.82+real(idatef(5))/525949.2
          elseif(trim(clcoo(jcoo)) == 'JD0') then
            !
            ! Coordonnée temporelle date julienne initiale.
            !
            icoo=icoo+1
            zseconde=0.
            call AMQHMS_VERS_DJ(idatef(1),idatef(2),idatef(3),idatef(4),idatef(5),zseconde,zcoo(icoo))
          elseif(trim(clcoo(jcoo)) == 'JD1') then
            !
            ! Coordonnée temporelle date julienne finale.
            !
            icoo=icoo+1
            zseconde=zech(1)
            call AMQHMS_VERS_DJ(idatef(1),idatef(2),idatef(3),idatef(4),idatef(5),zseconde,zcoo(icoo))
          elseif(clcoo(jcoo)(1:3) == 'PTS') then
            !
            ! Example: PTS2023100812: the user asks for the time in hours between 20231008 at 12 UTC and final DDH prediction time.
            !
            icoo=icoo+1
            zseconde=zech(1)
            call AMQHMS_VERS_DJ(idatef(1),idatef(2),idatef(3),idatef(4),idatef(5),zseconde,zdjfin)
            ! zdjfin est la date julienne de l'instant final de prévision de ce fichier DDH.
            cldateuser=clcoo(jcoo)(4:) ! example : 2023100812
            if(cldateuser(10:10) == ' ' .or. cldateuser(11:11) /= ' ') then
              write(*,fmt=*) 
              write(*,fmt=*) 'ddhi/ERROR: the "PTS" option implies a date in the form YYYYMMDDHH'
              write(*,fmt=*) 'YYYYMMDDHH is got here as ',trim(cldateuser)
              call exit(1)
            endif
            read(cldateuser(1:4),fmt=*) iyyyy
            read(cldateuser(5:6),fmt=*) imm
            read(cldateuser(7:8),fmt=*) iqq
            read(cldateuser(9:10),fmt=*) ihh
            iss=0
            zseconde2=0.
            call AMQHMS_VERS_DJ(iyyyy,imm,iqq,ihh,iss,zseconde2,zdjuser)
            zcoo(icoo)=(zdjfin-zdjuser)*24.
          elseif(clcoo(jcoo)(1:1) == 'B') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zecart(1)
          endif
        enddo
        if(llbil) then
          call sorfb(iulb,zcoo(1),zcoo(2),clnoma(6:),zchacf(jb))
        else
          if(clconvu /= 'SOL'.and.clconvu /= 'SOM') then
            llsorfdta=.true.
          elseif(mod(jb-1,ilev(1)) == 0.and.clconvu == 'SOM') then
            llsorfdta=.true.
          elseif(mod(jb,ilev(1)) == 0.and.clconvu == 'SOL') then
            llsorfdta=.true.
          else
            llsorfdta=.false.
          endif
          if(llsorfdta) call sorfdta(zcoo,icoo,zchacf(jb),iulsor)
        endif
      enddo
    else
      !
      ! Champ de taille verticale ilev(1)+1.
      !
      do jb=1,itaille
        !
        ! Recherche des coordonnees eventuelles.
        !
        icoo=0
        do jcoo=1,jpcoo
          ilcoo=len_trim(clcoo(jcoo))
          if(clcoo(jcoo)(1:ilcoo) == 'VP') then
            !
            ! Coordonnee verticale pression.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoopcfu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoopcfu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcoop0fu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcoop1fu(jb)
            else
              print*,'ERREUR: type non prevu 4!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:ilcoo) == 'VZ') then
            !
            ! Coordonnee verticale z.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoozcfu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcoozcfu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcooz0fu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcooz1fu(jb)
            else
              print*,'ERREUR: type non prevu 5!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:ilcoo) == 'VN') then
            !
            ! Coordonnee verticale niveaux.
            !
            icoo=icoo+1
            if(clnoma(6:6) == 'T'.or.clnoma(6:6) == 'P') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcooncfu(jb)
            elseif(clnoma(6:6) == 'F') then
              !
              ! Coordonnee vert.: masse cumulee.
              !
              zcoo(icoo)=zcooncfu(jb)
            elseif(clnoma(9:9) == '0') then
              !
              ! Coordonnee vert.: masse initiale.
              !
              zcoo(icoo)=zcoon0fu(jb)
            elseif(clnoma(9:9) == '1') then
              !
              ! Coordonnee vert.: masse finale.
              !
              zcoo(icoo)=zcoon1fu(jb)
            else
              print*,'ERREUR: type non prevu 6!...'
              print*,clnoma
              call exit(1)
            endif
          elseif(clcoo(jcoo)(1:1) == 'H') then
            !
            ! Coordonnees horizontales.
            !
            icoo=icoo+1
            idoml=(jb-1)/(ilev(1)+1)+1
            call cooxd(clcoo(jcoo)(2:),zdocdf,idoml,zcoo(icoo))
          elseif(clcoo(jcoo)(1:1) == 'T') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zech(1)/zcoet(jcoo)
          elseif(clcoo(jcoo)(1:2) == 'BA') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=real(idatef(1))+real(idatef(2)-1)/12.+real(idatef(3)-1)/365.2425
          elseif(clcoo(jcoo)(1:1) == 'B') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zecart(1)
          endif
        enddo
        if(llbil) then
          call sorfb(iulb,zcoo(1),zcoo(2),clnoma(6:),zchacf(jb))
        else
          if(clconvu /= 'SOL'.and.clconvu /= 'SOM') then
            llsorfdta=.true.
          elseif(mod(jb-1,ilev(1)+1) == 0.and.clconvu == 'SOM') then
            llsorfdta=.true.
          elseif(mod(jb,ilev(1)+1) == 0.and.clconvu == 'SOL') then
            llsorfdta=.true.
          else
            llsorfdta=.false.
          endif
          if(llsorfdta) call sorfdta(zcoo,icoo,zchacf(jb),iulsor)
        endif
      enddo
    endif
    if(.not.llbil) close(iulsor)
    if(clopt(1:ilopt) == 'ddhlfa2dd'.and..not.llbil) then
      !
      ! Preparation des titres globaux des figures.
      !
      write(clsor,fmt='(4a)') clfics(1:index(clfics,' ')-1),'.',clnoma(6:index(clnoma,' ')-1),'.doc'
      print*,trim(clsor)
      open(iultit,file=clsor)
      !
      ! cl2: C323 - BASE 1994  9 12  0 - ECH 10 J - 24 niv. - 20 b. zon.
      !
      if(icoo == 2) then
        !
        ! La valeur à tracer est associée à une coordonnée 2D.
        !
        if(clcoo(1)(1:ilcoo1) == 'HLON'.and.clcoo(2)(1:ilcoo2) == 'HLAT') then
          !
          ! Coordonnée lon-lat.
          !
          if(.not.llaqua) then
            !
            ! Mode normal. Tracé avec contours de continents.
            !
            write(iultit,fmt='(2a)') '#FORMAT=LLV'
          else
            !
            ! Mode aquaplanète. Tracé sans contours de continents.
            !
            write(iultit,fmt='(2a)') '#FORMAT=XYV'
          endif
          iintx=nint(1.5*sqrt(real(idom(1))))
          iintx=max(iintx,100)
          iinty=iintx
          write(iultit,fmt='(a,2i5)') '#INTERPOLE=',iintx,iinty
          write(iultit,fmt='(a,i3)') '#LISSAGE=',iliss
          write(iultit,fmt='(100a)') '#ISOL'
          write(iultit,fmt='(100a)') '#LANGAGE=ENG'
          write(iultit,fmt='(9a)') '#METHODE_EXTRAPOLATION=Y_PUIS_X'
        else
          !
          ! Coordonnée autre que lon-lat.
          !
          write(iultit,fmt='(2a)') '#FORMAT=XYV'
          write(iultit,fmt='(a,2i5)') '#INTERPOLE=',iintx,iinty
          write(iultit,fmt='(a,i3)') '#LISSAGE=',iliss
          write(iultit,fmt='(100a)') '#ISOL'
          write(iultit,fmt='(100a)') '#LANGAGE=ENG'
          write(iultit,fmt='(9a)') '#METHODE_EXTRAPOLATION=Y_PUIS_X'
        endif
        !
        ! On met des légendes en clair.
        !
        call legc(clcoo(1),clleg,illeg)
        write(iultit,fmt='(2a)') '#LEGENDE_X=',clleg(1:illeg)
        if(clcoo(1) == 'VZ') write(iultit,fmt='(9a)') '#X_MAX_LIMIT=',trim(clhmaxi)
        call legc(clcoo(2),clleg,illeg)
        write(iultit,fmt='(2a)') '#LEGENDE_Y=',clleg(1:illeg)
        if(clcoo(2) == 'VZ') write(iultit,fmt='(9a)') '#Y_MAX_LIMIT=',trim(clhmaxi)
      elseif(icoo == 1) then
        !
        ! La valeur à tracer est associée à une coordonnée 1D.
        !
        !
        !-------------------------------------------------
        ! Tracés X-valeur ou Y-valeur?
        !-------------------------------------------------
        !
        if(clcoo(1)(1:1) == 'V') then
          write(iultit,fmt='(2a)') '#FORMAT=YV'
          !
          ! On met des légendes en clair.
          !
          call legc(clcoo(2),clleg,illeg)
          write(iultit,fmt='(2a)') '#LEGENDE_X=',clleg(1:illeg)
          if(clcoo(2) == 'VZ') write(iultit,fmt='(9a)') '#X_MAX_LIMIT=',trim(clhmaxi)
          call legc(clcoo(1),clleg,illeg)
          write(iultit,fmt='(2a)') '#LEGENDE_Y=',clleg(1:illeg)
          if(clcoo(1) == 'VZ') write(iultit,fmt='(9a)') '#Y_MAX_LIMIT=',trim(clhmaxi)
        else
          write(iultit,fmt='(2a)') '#FORMAT=XV'
          !
          ! On met des légendes en clair.
          !
          call legc(clcoo(1),clleg,illeg)
          write(iultit,fmt='(2a)') '#LEGENDE_X=',clleg(1:illeg)
          if(clcoo(1) == 'VZ') write(iultit,fmt='(9a)') '#X_MAX_LIMIT=',trim(clhmaxi)
          call legc(clcoo(2),clleg,illeg)
          write(iultit,fmt='(2a)') '#LEGENDE_Y=',clleg(1:illeg)
          if(clcoo(2) == 'VZ') write(iultit,fmt='(9a)') '#Y_MAX_LIMIT=',trim(clhmaxi)
        endif
        !
        !-------------------------------------------------
        ! Pour les monocourbes on met 0 pour axe de référence.
        ! Ceci est utile par exemple pour VxxM, VxxF, etc...
        ! C'est par contre peu adapté à des champs
        ! tel VCT0 (température absolue).
        ! On pourra un jour raffiner en testant la mnémonique 
        ! "M ou F" versus "0 ou 1" par exemple.
        !-------------------------------------------------
        !
        write(iultit,fmt='(2a)') '#VREF=0.'
      else
        !
        ! La valeur à tracer est associée à une coordonnée ni 1D ni 2D.
        ! On met une en-tête au fichier ".doc", histoire de mettre
        ! quelque chose, car ce cas n'est pas prévu dans la norme "dd".
        !
        write(iultit,fmt='(2a)') '#FORMAT=XYV'
        write(iultit,fmt='(a,2i5)') '#INTERPOLE=',iintx,iinty
        write(iultit,fmt='(a,i3)') '#LISSAGE=',iliss
        write(iultit,fmt='(100a)') '#ISOL'
        write(iultit,fmt='(100a)') '#LANGAGE=ENG'
        write(iultit,fmt='(9a)') '#METHODE_EXTRAPOLATION=Y_PUIS_X'
        !
        ! On met des légendes en clair.
        !
        call legc(clcoo(1),clleg,illeg)
        write(iultit,fmt='(2a)') '#LEGENDE_X=',clleg(1:illeg)
        if(clcoo(1) == 'VZ') write(iultit,fmt='(9a)') '#X_MAX_LIMIT=',trim(clhmaxi)
        call legc(clcoo(2),clleg,illeg)
        write(iultit,fmt='(2a)') '#LEGENDE_Y=',clleg(1:illeg)
        if(clcoo(2) == 'VZ') write(iultit,fmt='(9a)') '#Y_MAX_LIMIT=',trim(clhmaxi)
      endif
      write(iultit,fmt='(9a)') '#FICHIER=',clsord(1:len_trim(clsord))
      !
      ! Echéance.
      !
      if(zech(1) >= 259200.) then
        ! Cas de runs de plus de 3 jours >> commentaire en jours
        zecho=zech(1)/86400.
        clzue=' days'
      else
        ! Cas de runs de moins de 3 jours >> commentaire en heures
        zecho=zech(1)/3600.
        clzue=' hours'
      endif
      !
      ! Affichage de l'echeance avec deux chiffres apres la virgule.
      ! Si l'echeance est voisine d'un entier a mieux que 10**-5 pres,
      ! on l'affiche au format entier.
      !
      call reecar(zecho,2,2,clze,ilze)
      clechea=clze(1:ilze)//trim(clzue)
      ilechea=len_trim(clechea)
      call autodoc(iulent,clficd,cldate,clorigine)
      !write(iultit,fmt='(a,i4,4(a,i2.2),2a)') '#DATE=BASE ',idatef(1),'-',idatef(2),'-',idatef(3),' ',idatef(4),':',idatef(5),' + ',clechea(1:ilechea)
      write(iultit,fmt='(9a)') trim(cldate)
      write(iultit,fmt='(2a)') '#TITRE=',cl3(1:il3)
      ilunit=len_trim(clunit)
      if(zcoefddhi == 1.) then
        write(iultit,fmt='(2a)') '#UNITE=',clunit(1:ilunit)
      else
        write(iultit,fmt='(2a)') '#UNITE='
      endif
      ilficd=len_trim(clficd)
      write(iultit,fmt='(4a)') trim(clorigine)
      if(itype(1) == 1) then
        write(cl1,fmt='(i3,a,i3,a)')ilev(1),'Niv, ',idom(1),'Dom.'
      elseif(itype(1) == 5) then
        write(cl1,fmt='(i3,a)')ilev(1),'Niv, Global.'
      elseif(itype(1) == 6) then
        write(cl1,fmt='(i3,a,i3,a)')ilev(1),'Niv, ',idom(1),' B. Zon.'
      else
        print*,'DDHU/ERREUR interne itype(1)!...'
        call exit(1)
      endif
      !write(iultit,fmt='(2a)') '#GRILLE=',trim(cl1)
    endif
  elseif(clnoma(1:9) == 'SOL-TRAC-') then
    !
    ! Lecture du champ sur le lfa.
    !
    call lfalecr(iulent,clnoma,jpprod,zchacf,itaille,irep)
    if(itaille /= idom(1)) then
      print*,'DDHI/ERREUR interne: idom(1)!...'
      call exit(1)
    endif
    !
    ! Fichier XYZ de sortie.
    !
    write(clsord,fmt='(4a)') clfics(1:index(clfics,' ')-1),'.',clnoma(10:index(clnoma,' ')-1),'.dta'
    if(.not.llbil) then
      print*,trim(clsord)
      open(iulsor,file=clsord)
      do jdom=1,idom(1)
        !
        ! Recherche des coordonnees eventuelles.
        !
        icoo=0
        do jcoo=1,jpcoo
          ilcoo=len_trim(clcoo(jcoo))
          if(clcoo(jcoo)(1:1) == 'H') then
            !
            ! Coordonnees horizontales.
            !
            icoo=icoo+1
            call cooxd(clcoo(jcoo)(2:),zdocdf,jdom,zcoo(icoo))
          elseif(clcoo(jcoo)(1:1) == 'T') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zech(1)/zcoet(jcoo)
          elseif(clcoo(jcoo)(1:2) == 'BA') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=real(idatef(1))+real(idatef(2)-1)/12.+real(idatef(3)-1)/365.2425
          elseif(clcoo(jcoo)(1:1) == 'B') then
            !
            ! Coordonnees temporelles.
            !
            icoo=icoo+1
            zcoo(icoo)=zecart(1)
          endif
        enddo
        call sorfdta(zcoo,icoo,zchacf(jdom),iulsor)
      enddo
      close(iulsor)
    endif
  endif
enddo
  200 continue
!
! Fermeture du fichier DDH d'entree
!
call lfafer(iulent)
!
! Fermeture du fichier-bilan.
!
if(llbil) then
  close(iulb)
  ilfics=len_trim(clfics)
  !
  ! On effectue un appel systeme a seule fin de trier
  ! le fichier clfics par domaines et niveaux
  ! (il est actuellement trie par champs).
  !
  write(clsyst,*) 'sort ',clfics(1:ilfics),' > tmp ; mv tmp ',clfics(1:ilfics)
  iresul=system(clsyst)
  if(iresul /= 0) write(*,fmt=*) clsyst(1:len_trim(clsyst)),': system error number ',iresul
  print*,'Bilan porte sur le fichier ',clfics(1:ilfics)
endif
end
#include"lited.F90"
