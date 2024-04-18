program ddht
! --------------------------------------------------------------------------
! **** *ddht   * DDH-Transformations: utilitaires de somme, difference, etc.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 92-03-01, J.M. Piriou.
!
! Modifications:
! --------------
! 92-12-04, J.M. Piriou: adaptation au calcul sur CRAY.
! 94-10-27, J.M. Piriou: adaptation au calcul sur lfa.
! 96-02-14, J.M. Piriou: implémentation de la séparation
! en termes principal et complémentaire.
! 97-02-10, J.M. Piriou: implémentation de la fonctionnalité calculatrice
! dirigée par le fichier-liste.
! 2006-06-12, J.M. Piriou: implémentation de la fonctionnalité interpolation d'un fichier DDH sur la grille d'un autre.
! 2008-07-17, J.M. Piriou: translate somme comments to English.
! 2017-03-21, J.M. Piriou: traitement des champs de surface AROME.
! 2017-04-14, J.M. Piriou: correction de bogue: en mode calculatrice, ddht utilisait imaxv1 au lieu de jpprod pour monter la pile ou opérer des additions, multiplications, etc. C'est OK si le champ est une variable, mais si le champ est un flux on ne bouclait pas jusqu'au niveau de flux le plus bas. La conséquence en était que ddh_prec, qui utilise "ddht -cCALC", indiquait sur le dernier domaine les RR du niveau KLEV-1 au lieu de KLEV.
! 2019-10-23, J.M. Piriou: correction de bogue, lors de l'extraction de niveaux verticaux.
! 2019-11-26, J.M. Piriou: suite correction de bogue, lors de l'extraction de niveaux verticaux.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)

#include"ddhpar.h"
#include"ddht_yom_ent.h"
CHARACTER*200 :: CGNFE1
CHARACTER*200 :: CGNFE2
CHARACTER*200 :: CGNFLC,clficc
CHARACTER*200 :: CLLOC,cli,cllis,cllisde,cgnfso,clc

LOGICAL :: LGFILAF,llex
LOGICAL :: LGRENS
CHARACTER*240 clarg(jparg)
CHARACTER *80 cllisct(jpnomct)
CHARACTER *80 clmot(180)
CHARACTER *03 clconvs
INTEGER(KIND=4) :: idimpile ! dimension de X dans la pile de la calculatrice polonaise inverse. C'est le nb de réels écrits sur le tableau zcalcx.
INTEGER(KIND=4) :: iindef
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: ILARG
INTEGER(KIND=4) :: JARG
LOGICAL :: LLPREMR
LOGICAL :: LLTOUS
INTEGER(KIND=4) :: ILISCT
INTEGER(KIND=4) :: IMES
INTEGER(KIND=4) :: IUL1
INTEGER(KIND=4) :: IUL2
INTEGER(KIND=4) :: IUL3
INTEGER(KIND=4) :: IULFICC
INTEGER(KIND=4) :: JPROD
CHARACTER*200 :: CLNAMX1
INTEGER(KIND=4) :: IFINL
INTEGER(KIND=4) :: ILISCT0
INTEGER(KIND=4) :: ILLIS
INTEGER(KIND=4) :: IPLUS
INTEGER(KIND=4) :: IREP
INTEGER(KIND=4) :: JLISCT
INTEGER(KIND=4) :: IDOM1
INTEGER(KIND=4) :: ILEV1
INTEGER(KIND=4) :: ILEV1flux
INTEGER(KIND=4) :: ILONG
CHARACTER*200 :: CLTYPE
INTEGER(KIND=4) :: IMAXV1
INTEGER(KIND=4) :: IMAXV1flux
INTEGER(KIND=4) :: INDOMA
REAL(KIND=8) :: zech1(1)
CHARACTER*200 :: CLADCOU
CHARACTER*200 :: CLNAMX2
INTEGER(KIND=4) :: JDOC
REAL(KIND=8) :: zechdv1(1)
CHARACTER*200 :: CLNAMX3
CHARACTER*200 :: CLTITA
INTEGER(KIND=4) :: IBASE3
INTEGER(KIND=4) :: IDOM2
INTEGER(KIND=4) :: IDOM3
INTEGER(KIND=4) :: IECARTM
INTEGER(KIND=4) :: IERRD
INTEGER(KIND=4) :: ILC
INTEGER(KIND=4) :: ILEV2
INTEGER(KIND=4) :: ILEV2COU
INTEGER(KIND=4) :: ILEV3
INTEGER(KIND=4) :: ILMOT
INTEGER(KIND=4) :: JDATE
INTEGER(KIND=4) :: JINDOMA
INTEGER(KIND=4) :: IB
INTEGER(KIND=4) :: IDOMCOU
INTEGER(KIND=4) :: IERR1
INTEGER(KIND=4) :: IERR2
INTEGER(KIND=4) :: ILART
INTEGER(KIND=4) :: ILEV
INTEGER(KIND=4) :: ILONES
INTEGER(KIND=4) :: IMOD
INTEGER(KIND=4) :: IMODV
INTEGER(KIND=4) :: INIVV
INTEGER(KIND=4) :: IPROD1
INTEGER(KIND=4) :: IPROD2
INTEGER(KIND=4) :: IPROD3
INTEGER(KIND=4) :: J1
INTEGER(KIND=4) :: J2
INTEGER(KIND=4) :: JB
INTEGER(KIND=4) :: JLEV
REAL(KIND=8) :: ZNORM
REAL(KIND=8) :: ZSOM
REAL(KIND=8) :: ZTMP
REAL(KIND=8) :: ZVAL
INTEGER(KIND=4) :: JMOT
REAL(KIND=8) :: zech2(1)
REAL(KIND=8) :: zech3(1)
REAL(KIND=8) :: zechdv2(1)
REAL(KIND=8) :: zechdv3(1)

!
INTEGER(KIND=4) idatef1(11) ! documentation sur les dates.
INTEGER(KIND=4) idatef2(11)
INTEGER(KIND=4) idatef3(11),jdom
!
INTEGER(KIND=4) idocfi1(17) ! documentation sur les dimensions des champs diag.
INTEGER(KIND=4) idocfi2(17)
INTEGER(KIND=4) idocfi3(17)
!
INTEGER(KIND=4) idoma(jpdom) ! domaine a extraire (option -cEXTRAIT_DOMAIN).
INTEGER(KIND=4) imaxval_idoma
!
REAL(KIND=8) zdocd1(jpdoc,jpdom) ! documentation sur les domaines.
REAL(KIND=8) zdocd3(jpdoc,jpdom) ! documentation sur les domaines.
REAL(KIND=8) zdocd(jpdoc) ! documentation sur les domaines: pour lfalec.
!
REAL(KIND=8) zprod1(jpprod) ! donnees du fichier d'entree 1.
REAL(KIND=8) zprod2(jpprod) ! donnees du fichier d'entree 2.
REAL(KIND=8) zprod3(jpprod) ! donnees du fichier de sortie 3.
REAL(KIND=8) zcumt(jpprod) ! tendance cumulee (cas separ_pr_compl).
REAL(KIND=8) zcalcx(jpprod) ! calculatrice: pile X.
REAL(KIND=8) zcalcy(jpprod) ! calculatrice: pile Y.
REAL(KIND=8) zcalcz(jpprod) ! calculatrice: pile Z.
REAL(KIND=8) zcalct(jpprod) ! calculatrice: pile T.
REAL(KIND=8) zcalcu(jpprod) ! calculatrice: pile U.
REAL(KIND=8) zcalcv(jpprod) ! calculatrice: pile V.
REAL(KIND=8) zpre_ini_1(jpprod) ! VPP0 fichier 1.
REAL(KIND=8) zpre_fin_1(jpprod) ! VPP1 fichier 1.
REAL(KIND=8) zpre_cum_1(jpprod) ! PPP fichier 1.
REAL(KIND=8) zpre_ini_2(jpprod) ! VPP0 fichier 2.
REAL(KIND=8) zpre_fin_2(jpprod) ! VPP1 fichier 2.
REAL(KIND=8) zpre_cum_2(jpprod) ! PPP fichier 2.
REAL(KIND=8) zep_ini_1(jpprod) ! VEP0 fichier 1.
REAL(KIND=8) zep_ini_2(jpprod) ! VEP0 fichier 2.
!
! Initialisation.
!
idoma=0
!
! **   Initialisation par defaut des variables d'entree.
!
idocfi1(16)=0 ; idocfi1(17)=0
idocfi2(16)=0 ; idocfi2(17)=0
idocfi3(16)=0 ; idocfi3(17)=0
cgconf='DIFFE_EXP_REFE'
cgnfe1='ddh1'
cgnfe2='ddh2'
cgnflc='lc'
cgnfso='ddht.lfa'
lgrens=.true.
lgdebu=.false.
clconvs='NON'
lltous=.true. ! vrai si tous les champs du fichier d'entrée à traiter.
llpremr=.true. ! vrai si premier mot de la liste des opérations "calculatrice" rencontré.
cllisde='1,2,4' ! liste des domaines (resp niveaux) a extraire (option -cEXTRAIT_DOMAIN (resp -cEXTRAIT_NIVEAUX)).
iindef=-124574154
idimpile=iindef
!
! Obtention des parametres d'entree par ligne de commande.
!
ierr=0
do jarg=1,jparg
  clarg(jarg)=' '
  call getargp(jarg,clarg(jarg))
  ilarg=len_trim(clarg(jarg))
  if(clarg(jarg)(1:1) /= ' ') then
    ! L'argument numero jarg est non vide.
    if(clarg(jarg)(1:2) == '-c') then
      clloc=clarg(jarg)(3:)
      if(trim(clloc) == 'SOMME_PONDEREE') then
      elseif(trim(clloc) == 'SOMME_CONTIGUE') then
      elseif(trim(clloc) == 'DIFFE_EXP_REFE') then
      elseif(trim(clloc) == 'DIFFE_EC2_EC1') then
      elseif(trim(clloc) == 'DIFFE_PONDEREE') then
      elseif(trim(clloc) == 'SEPAR_PR_COMPL') then
      elseif(trim(clloc) == 'MOY_VERTIC') then
      elseif(trim(clloc) == 'INTERPOL') then
      elseif(trim(clloc) == 'MOY_HORIZ') then
      elseif(trim(clloc) == 'EXTRAIT_DOMAIN') then
      elseif(trim(clloc) == 'EXTRAIT_NIVEAUX') then
      elseif(trim(clloc) == 'CALC') then
      else
        ierr=1
        print*,'ddht/ERREUR: argument -c errone!...'
      endif
      cgconf=clloc
    elseif(clarg(jarg)(1:2) == '-1') then
      cgnfe1=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-2') then
      cgnfe2=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-l') then
      lltous=.false.
      cgnflc=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-L') then
      clloc=clarg(jarg)(3:)
      if(trim(clloc) == 'TOUS') then
        lltous=.true.
      elseif(trim(clloc) == 'PAR_FICHIER') then
        lltous=.false.
      else
        ierr=1
        print*,'ddht/ERREUR: argument -L errone!...'
      endif
    elseif(clarg(jarg)(1:2) == '-E') then
      cllisde=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-s') then
      cgnfso=clarg(jarg)(3:)
    elseif(clarg(jarg)(1:2) == '-r') then
      clloc=clarg(jarg)(3:)
      if(trim(clloc) == 'OUI') then
        lgrens=.true.
      elseif(trim(clloc) == 'NON') then
        lgrens=.false.
      else
        ierr=1
        print*,'ddht/ERREUR: argument -r errone!...'
      endif
    elseif(clarg(jarg)(1:2) == '-S') then
      clloc=clarg(jarg)(3:)
      if(trim(clloc) == 'OUI') then
      elseif(trim(clloc) == 'NON') then
      else
        ierr=1
        print*,'ddht/ERREUR: argument -S errone!...'
      endif
      clconvs=clloc
    elseif(clarg(jarg)(1:2) == '-d') then
      clloc=clarg(jarg)(3:)
      if(trim(clloc) == 'OUI') then
        lgdebu=.true.
      elseif(trim(clloc) == 'NON') then
        lgdebu=.false.
      else
        ierr=1
        print*,'ddht/ERREUR: argument -d errone!...'
      endif
    else
      ierr=1
      print*,'ddht/ERREUR: argument d''entree ' &
&         ,clarg(jarg)(1:ilarg),' errone!...'
    endif
  endif
enddo
if(clarg(1)(1:1) == ' '.or.ierr == 1) then
  ! L'utilisateur n'a tape aucun argument.
  print*,' '
  print*,'Transforms on DDH file:'
  print* &
&     ,'ddht cumulates or differentiate 2 DDH files,'
  print*,'operates vertical or horizontal means,'
  print*,'extracts domains or levels,'
  print*,'performs simple operations on budget terms.'
  print* &
&     ,'Input and output files are DDH files.'
  print*,' '
  print*,'Usage: ddht -c -1 -2 -l -S -s -r -d'
  print*,' '
  print*,'where'
  print*,'  -c configuration (SOMME_PONDEREE, SOMME_CONTIGUE,'
  print*,'                    DIFFE_EXP_REFE, DIFFE_EC2_EC1,'
  print*,'                    DIFFE_PONDEREE, SEPAR_PR_COMPL,'
  print*,'                    MOY_VERTIC,     MOY_HORIZ, INTERPOL,'
  print*,'                    EXTRAIT_DOMAIN, EXTRAIT_NIVEAUX, CALC).'
  print*,'     Default: DIFFE_EXP_REFE.'
  print* &
&     ,'     Differences are file 2 minus file 1.'
  print*,'  -1 input file 1.'
  print*,'  -2 input file 2.'
  print*,'  -L liste of DDH fields to be processed (TOUS, PAR_FICHIER).'
  print*,'     TOUS: all fields from DDH file will be processed.'
  print*,'     PAR_FICHIER: the fields from a given list will be processed.'
  print*,'       This list is given in a file, which name is given by the argument -l below.'
  print*,'     Default: TOUS.'
  print*,'  -l file-list if fields to be processed, in the case -LPAR_FICHIER,'
  print*,'     ou liste des operations a effectuer dans le cas -cCALC.'
  print*,'     Default: lc.'
  print*,'  -S process soil fields (OUI, NON).'
  print*,'     Default: NON.'
  print*,'  -E in case of options -cEXTRAIT_DOMAIN'
  print* &
&     ,'     or -cEXTRAIT_NIVEAUX, list of domains or levels to be extracted. '
  print* &
&     ,'     Levels are given in the form 1,2,4'
  print*,'     to extract domains 1, 2 and 4,'
  print*,'     or 7-11 to extract domains 7 to 11.'
  print*,'     or -0.692_+44.832_Bordeaux to get the domain, closest to the place longitude=-0.692°, latitude=44.832°.'
  print*,'       Remark1: in the notation above the format used to write the real data does not matter.'
  print*,'       Remark2: in the notation above the text right to the second "_" is not read by ddht.'
  print*,'         It can be omitted: -0.692_+44.832 leads to the same as -0.692_+44.832_Bordeaux.'
  print*,'  -s output file.'
  print*,'  -r information prints on standard output (OUI, NON).'
  print*,'     Default: OUI.'
  print*,'  -d debug mode (OUI, NON).'
  print*,'     Default: NON.'
  print*,'Examples:'
  print*,'  ddht -cMOY_HORIZ -1monfic -sficsor -llc2 -rOUI.'
  print*,'  ddht -cEXTRAIT_DOMAIN -1f1 -sresul -E5,6,7.'
  print*,'  ddht -cEXTRAIT_DOMAIN -1f1 -sresul -E+001.374_+43.575_Toulouse-Meteopole.'
  print*,'  ddht -cCALC -1f1 -sresul -lliste_des_oper.'
  print*,'  ddht -cINTERPOL -1DZ_41_niv.lfa -2DZ_46_niv.lfa -sDZ_41_niv_interpole_sur_46.lfa'
  print*,'------------------------------------------------------'
  stop
endif
lgrens=lgrens.or.lgdebu
!
! **   Impression des valeurs de namelist.
!
if(lgrens) then
  print*, '----------------------'
  print*, '------ddht------------'
  print*, '----------------------'
  print*, 'CGCONF   =',cgconf(1:len_trim(cgconf))
  print*, 'LGRENS   =',lgrens
  print*, 'LGDEBU   =',lgdebu
  print*, 'CGNFE1   =',cgnfe1(1:index(cgnfe1,' ')-1)
  print*, 'CGNFE2   =',cgnfe2(1:index(cgnfe2,' ')-1)
  print*, 'CGNFLC   =',cgnflc(1:index(cgnflc,' ')-1)
  print*, 'CGNFSO   =',cgnfso(1:index(cgnfso,' ')-1)
endif
if(trim(clloc) == 'CALC') then
  !
  ! Calculatrice.
  ! Mise à zéro de la pile de calcul.
  !
  idimpile=0
  do jprod=1,jpprod
    zcalcx(jprod)=0.
    zcalcy(jprod)=0.
    zcalcz(jprod)=0.
    zcalct(jprod)=0.
    zcalcu(jprod)=0.
    zcalcv(jprod)=0.
  enddo
endif
!
! **   Initialisation.
!
! Unites logiques.
!
iulficc=3 ! unite logique du fichier-liste des champs.
iul1=13 ! fichier d'entree 1.
iul2=14 ! fichier d'entree 2.
iul3=15 ! fichier de sortie.
!
! **   Ouverture du fichier d'entree 1.
!
imes=1 ! niveau de messagerie du logiciel lfa.
call lfaouv(iul1,cgnfe1,'R')  
call lfames(iul1,imes)
!
! Fichier-liste des champs a estimer et(ou) tracer
! sur le tableau cllisct
!
ilisct=0
if(.not.lltous) then
  !
  ! L'utilisateur fournit la liste des champs à traiter
  ! dans un fichier.
  !
  clficc=cgnflc
  inquire(file=clficc,exist=llex)
  if(llex) then
    open(iulficc,file=clficc,form='formatted')
  635     continue
    read(iulficc,fmt='(a)',end=130) cli
    !
    ! On cherche la position d'un éventuel caractère
    ! marquant le début d'une zone de commentaire
    ! sur la ligne.
    !
    if(cli(1:1) /= '#'.and.cli(1:1) /= '  ') then
      !
      ! Le premier caractère n'est ni un blanc, ni une tabulation
      ! ni un '#'. La ligne n'est donc pas commentée.
      !
      ! Y a-t-il un "#" sur cette ligne?
      !
      irep=index(cli,'#')
      if(irep == 0) then
        !
        ! Pas de "#" sur la ligne.
        !
        ifinl=len_trim(cli)
      else
        !
        ! "#" sur la ligne.
        !
        ifinl=irep-1
      endif
      !
      ! Une ligne de plus sur la liste des champs à traiter.
      !
      ilisct=ilisct+1
      if(ilisct > jpnomct) then
        print* &
&           ,'ddht/ERREUR: recompiler avec une valeur plus grande de jpnomct!...'
        print*,ilisct,jpnomct
                                call exit(1)
      endif
      !
      ! On saisit la ligne jusqu'à son éventuel commentaire.
      !
      cllisct(ilisct)=cli(1:ifinl)
    endif
    goto 635
  130     continue
  else
    print*,'ddht/ERREUR: fichier-liste des champs inexistant!...'
    print*,clficc
    call exit(1)
  endif
else
  !
  ! L'utilisateur souhaite traiter tous les champs du fichier d'entrée.
  ! Etablissement de la liste de ces champs.
  !
  call lfalaft(iul1,cllisct,jpnomct,ilisct0)
  !
  ! On va ne garder de cette liste que les noms différents
  ! au sens DDH du terme, i.e.
  ! ceux de nom tel que 'DATE' ne seront pas conservés.
  !
  ilisct=0
  do jlisct=1,ilisct0
    cllis=cllisct(jlisct)
    illis=len_trim(cllis)
!    if(cllis(1:illis) /= 'DATE' &
!&       .and.cllis(1:illis) /= 'DOCFICHIER' &
!&       .and.cllis(1:illis) /= 'INDICE EXPERIENCE' &
!&       .and.cllis(1:illis) /= 'SVGFS' &
!&       .and.cllis(1:illis) /= 'SFGFS' &
!&       .and.cllis(1:4) /= 'DOCD' &
!&       .and.(cllis(1:1) /= 'S'.or.cllis(4:4) /= '_'.or.(cllis(5:5) &
!&        /= '0'.and.cllis(5:5) /= '1')) &
!&       .and.(cllis(1:1) /= 'G'.or.illis /= 3) &
!&       .and.cllis(1:illis) /= 'ECHEANCE' &
!&       .and.cllis(1:illis) /= 'ECHEANCEDV') then
    if(cllis(1:1) == 'V' &
      &  .or. cllis(1:1) == 'T' &
      &  .or. cllis(1:1) == 'F' &
      &  .or. cllis(1:1) == 'S' &
      &  .or. cllis(1:1) == 'G' &
      &  .or. trim(cllis) == 'PPP' &
      &  ) then
      !
      ! Il s'agit d'un article de champ réel DDH.
      ! On le porte sur la liste de sortie.
      !
      ilisct=ilisct+1
      cllisct(ilisct)=cllisct(jlisct)
    else
    endif
  enddo
endif
if(lgrens) then
  print*,ilisct,' articles lus sur le fichier-liste.'
endif
if (cgconf(1:len_trim(cgconf)) /= 'SEPAR_PR_COMPL'.and.cgconf(1:len_trim(cgconf)) /= 'CALC'.and..not.lltous) then
  ! On ajoute systematiquement a la liste les articles de coordonnees
  ! car ils sont necessaires aux eventuels traces ulterieurs.
  iplus=5
  if(ilisct+iplus > jpnomct) then
    print* &
&       ,'ddht/ERROR "iplus": recompile with a greater value of jpnomct!...'
    print*,ilisct+iplus,jpnomct
                call exit(1)
  endif
  do jlisct=ilisct,1,-1
    cllisct(jlisct+iplus)=cllisct(jlisct)
  enddo
  ilisct=ilisct+iplus
  cllisct(1)='VPP0         '
  cllisct(2)='VEP0         '
  cllisct(3)='VPP1         '
  cllisct(4)='VEP1         '
  cllisct(5)='PPP          '
endif
!
! Lecture des articles de documentation.
!
! Lecture du premier article: indice en 4 lettres de l'experience.
!
call lfalecc(iul1,'INDICE EXPERIENCE',1,clnamx1,ilong,irep)
!
! Lecture du 2eme    article: DATE.
!
call lfaleci(iul1,'DATE',11,idatef1,ilong,irep)
!
!-------------------------------------------------
! Date à la seconde près.
!-------------------------------------------------
!
write(clbase1,fmt='(i4,5i2.2)') idatef1(1),idatef1(2),idatef1(3),idatef1(4),idatef1(5),0
!
! Lecture du 3eme    article: DOCFICHIER.
!
call lfaleci(iul1,'DOCFICHIER',17,idocfi1,ilong,irep)
ilev1=idocfi1(6)
ilev_ref=idocfi1(6)
idom1=idocfi1(15)
imaxv1=idom1*ilev1
imaxv1flux=idom1*(ilev1+1)
if(ilev1 > jplev) then
  print* &
&     ,'ddht/ERROR: recompile with a greater jplev!...'
  print*,ilev1,jplev
        call exit(1)
endif
if(idom1 > jpdom) then
  print* &
&     ,'ddht/ERROR: recompile with a greater jpdom!...'
  print*,idom1,jpdom
        call exit(1)
endif
!
! Initialisation des domaines ou niveaux a extraire.
!
call inide(iul1,cllisde,idoma,jpdom,indoma)
!
!-------------------------------------------------
! Contrôle que la demande d'extraction
! de l'utilisateur est compatible avec les domaines/niveaux
! effectivement présents dans le fichier.
!-------------------------------------------------
!
if(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_NIVEAUX') then
  imaxval_idoma=0
  do jdom=1,indoma
    if(idoma(jdom) > imaxval_idoma) imaxval_idoma=idoma(jdom)
  enddo
  if(imaxval_idoma > ilev1) then
    print*,'ddht/ERROR: extract levels impossible:'
    print*,'  level ',maxval(idoma),' required by te user,  ',ilev1,' levels in the file!...'
                call exit(1)
  endif
elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_DOMAIN') then
  imaxval_idoma=0
  do jdom=1,indoma
    if(idoma(jdom) > imaxval_idoma) imaxval_idoma=idoma(jdom)
  enddo
  if(imaxval_idoma > idom1) then
    print*,'ddht/ERROR: extract domains impossible:'
    print*,'  domain ',maxval(idoma),' required by the user, ',idom1,' domains in the file!...'
                call exit(1)
  endif
endif
!
! Lecture du 4eme    article: ECHEANCE.
!
call lfalecr(iul1,'ECHEANCE',1,zech1,ilong,irep)
!
! Echéance caractéristique des "delta de variables".
!
call lfacas(iul1,'ECHEANCEDV',cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
if(irep == 0) then
  !
  ! L'article est dans le fichier.
  ! On y lit l'échéance.
  !
  call lfalecr(iul1,'ECHEANCEDV',1,zechdv1,ilong,irep)
else
  !
  ! L'article n'est pas dans le fichier.
  ! L'échéance est prise égale à l'écheance nominale 'ECHEANCE'.
  !
  zechdv1(1)=zech1(1)
endif
!
! Lecture des documentations sur les domaines.
!
do jdom=1,idom1
  call lited(iul1,jdom,'R',zdocd,irep)
  do jdoc=1,jpdoc
    zdocd1(jdoc,jdom)=zdocd(jdoc)
    zdocd3(jdoc,jdom)=zdocd(jdoc)
  enddo
enddo
!
! Lecture des champs de masse.
!
call lfacas(iul1,'VCP0',cltype,ilong,irep)
if(irep == 0) then
  call lfalecr(iul1,'VCP0',jpprod,zpre_ini_1,ilong,irep)
  call lfalecr(iul1,'VCP1',jpprod,zpre_fin_1,ilong,irep)
  call lfalecr(iul1,'VCZ0',jpprod,zep_ini_1,ilong,irep)
else
  call lfalecr(iul1,'VPP0',jpprod,zpre_ini_1,ilong,irep)
  call lfalecr(iul1,'VPP1',jpprod,zpre_fin_1,ilong,irep)
  call lfalecr(iul1,'VEP0',jpprod,zep_ini_1,ilong,irep)
endif
call lfalecr(iul1,'PPP',jpprod,zpre_cum_1,ilong,irep)
!
! On rend non fatale toute erreur de lecture du fichier.
!
call lfaerf(iul1,.false.)
!
! ** Ouverture du fichier d'entree 2.
!
if (cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE' .or. trim(cgconf) == 'INTERPOL') then
  !
  ! Deux fichiers en entree.
  ! On ouvre le deuxieme.
  !
  call lfaouv(iul2,cgnfe2,'R')  
  !
  ! Lecture des articles de documentation.
  !
  ! Lecture du premier article: indice en 4 lettres de l'experience.
  !
  call lfalecc(iul2,'INDICE EXPERIENCE',1,clnamx2,ilong,irep)
  !
  ! Lecture du 2eme    article: DATE.
  !
  call lfaleci(iul2,'DATE',11,idatef2,ilong,irep)
  !
  ! Lecture du 3eme    article: DOCFICHIER.
  !
  call lfaleci(iul2,'DOCFICHIER',17,idocfi2,ilong,irep)
  ilev2=idocfi2(6)
  idom2=idocfi2(15)
  !
  ! Test de cohérence de dimensions entre les deux fichiers
  ! d'entrée.
  !
  ierrd=0
  if(ilev1 /= ilev2 .and. (cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE')) then
    write(*,'(a)') &
&       ' ddht/ERROR: the two DDH files have a different number of levels!...'
    print*,ilev1,ilev2
    ierrd=1
  endif
  if(idom1 /= idom2 .and. (cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE')) then
    write(*,'(a)') &
&       ' ddht/ERROR: the two DDH files have a different number of domains!...'
    print*,idom1,idom2
    ierrd=1
  endif
  if(ierrd == 1) then
                call exit(1)
        endif
  !
  ! Lecture du 4eme    article: ECHEANCE.
  !
  call lfalecr(iul2,'ECHEANCE',1,zech2,ilong,irep)
  !
  ! Echéance caractéristique des "delta de variables".
  !
  call lfacas(iul2,'ECHEANCEDV',cltype,ilong,irep) ! on regarde si l'article est dans le fichier.
  if(irep == 0) then
    !
    ! L'article est dans le fichier.
    ! On y lit l'échéance.
    !
    call lfalecr(iul2,'ECHEANCEDV',1,zechdv2,ilong,irep)
  else
    !
    ! L'article n'est pas dans le fichier.
    ! L'échéance est prise égale à l'écheance nominale 'ECHEANCE'.
    !
    zechdv2(1)=zech2(1)
  endif
  !
  ! Lecture des champs de masse.
  !
  call lfacas(iul2,'VCP0',cltype,ilong,irep)
  if(irep == 0) then
    call lfalecr(iul2,'VCP0',jpprod,zpre_ini_2,ilong,irep)
    call lfalecr(iul2,'VCP1',jpprod,zpre_fin_2,ilong,irep)
    call lfalecr(iul2,'VCZ0',jpprod,zep_ini_2,ilong,irep)
  else
    call lfalecr(iul2,'VPP0',jpprod,zpre_ini_2,ilong,irep)
    call lfalecr(iul2,'VPP1',jpprod,zpre_fin_2,ilong,irep)
    call lfalecr(iul2,'VEP0',jpprod,zep_ini_2,ilong,irep)
  endif
  call lfalecr(iul2,'PPP',jpprod,zpre_cum_2,ilong,irep)
  !
  ! On rend non fatale toute erreur de lecture du fichier.
  !
  call lfaerf(iul2,.false.)
  !
  ! Nouvel indice experience.
  !
  if(clnamx1 == clnamx2) then
    clnamx3=clnamx1
  elseif(trim(cgconf) == 'DIFFE_EXP_REFE') then
    clnamx3=trim(clnamx2)//'-'//trim(clnamx1)
  elseif(trim(cgconf) == 'INTERPOL') then
    clnamx3=clnamx1
  else
    clnamx3=cgconf
  endif
else
  !
  ! Un seul fichier en entree.
  !
  ! Nouvel indice experience.
  !
  clnamx3=clnamx1
endif
!
! ** Combinaison des articles de documentation.
!
! * Determination de la base du fichier resultant.
!
if(cgconf(1:len_trim(cgconf)) == 'DIFFE_EC2_EC1') then
  !
  ! Nouvelle base: celle du fichier 1 + echeance 1.
  !
  !iecartm=nint(zech1(1)/3600.) ! ecart en heures.
  !call daplus(ibase1,2,iecartm,ibase3)
  iecartm=nint(zech1(1)) ! ecart en secondes.
  !
  !-------------------------------------------------
  ! Date à la seconde près, au format AAAAMMQQHHNNSS.
  !-------------------------------------------------
  !
  call dapluss(clbase1,iecartm,clbase3)
else
  !
  ! Nouvelle base: celle du fichier 1.
  !
  clbase3=clbase1
endif
!
! * Determination des articles idatef et idocfi.
!
do jdate=1,11
  idatef3(jdate)=idatef1(jdate)
enddo
do jdoc=1,17
  idocfi3(jdoc)=idocfi1(jdoc)
enddo
idom3=idom1
ilev3=ilev1
if(cgconf(1:len_trim(cgconf)) == 'DIFFE_PONDEREE' &
&   .or.cgconf(1:len_trim(cgconf)) == 'DIFFE_EC2_EC1') then
  ! Difference des articles extensifs dans le temps.
  idatef3(7)=idatef2(7)-idatef1(7)
  idatef3(10)=idatef2(10)-idatef1(10)
  idocfi3(5)=idocfi2(5)-idocfi1(5)
elseif(cgconf(1:5) == 'SOMME') then
  ! Somme des articles extensifs dans le temps.
  idatef3(7)=idatef2(7)+idatef1(7)
  idatef3(10)=idatef2(10)+idatef1(10)
  idocfi3(5)=idocfi2(5)+idocfi1(5)
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_VERTIC') then
  ilev3=1 ! on porte a 1 le nombre de niveaux verticaux.
  idocfi3(6)=ilev3
elseif(cgconf(1:len_trim(cgconf)) == 'INTERPOL') then
  ilev3=ilev2 ! on porte le nombre de niveaux verticaux à celui du fichier 2.
  idocfi3(6)=ilev3
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_HORIZ') then
  if(idocfi1(1) == 5) then
    !
    ! L'utilisateur veut faire un profil vertical
    ! d'un fichier deja global, operation
    ! denuee de sens...
    !
    idom3=1
    idocfi3(1)=5
    idocfi3(15)=idom3
    do jdoc=1,jpdoc
      zdocd3(jdoc,1)=0.
    enddo
  elseif(idocfi1(1) == 6) then
    !
    ! Profil vertical de fichier zonal.
    ! Le resultat est un fichier global.
    !
    idom3=1
    idocfi3(1)=5
    idocfi3(15)=idom3
    do jdoc=1,jpdoc
      zdocd3(jdoc,1)=0.
    enddo
  elseif(idocfi1(1) == 1) then
    !
    ! Profil vertical de fichier domaine limite.
    ! Le resultat est un fichier limite a 1 domaine.
    !
    idom3=1
    idocfi3(1)=1
    idocfi3(15)=idom3
    do jdoc=1,jpdoc
      zdocd3(jdoc,1)=0.
    enddo
    do jdoc=1,jpdoc
      do jdom=1,idom1
        zdocd3(jdoc,1)=zdocd3(jdoc,1) &
&           +zdocd1(jdoc,jdom)/idom1
      enddo
    enddo
  endif
elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_DOMAIN') then
  idom3=indoma
  idocfi3(1)=1 ! le fichier de sortie devient un fichier type "domaines limites".
  idocfi3(15)=indoma ! nombre de domaines presents dans le fichier de sortie.
elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_NIVEAUX') then
  idocfi3(6)=indoma ! le fichier de sortie devient un fichier a indoma niveaux.
endif
read(clbase3(01:04),fmt=*) idatef3(1)
read(clbase3(05:06),fmt=*) idatef3(2)
read(clbase3(07:08),fmt=*) idatef3(3)
read(clbase3(09:10),fmt=*) idatef3(4)
read(clbase3(11:12),fmt=*) idatef3(5)
!
! * Determination de la nouvelle echeance.
!
if(cgconf(1:len_trim(cgconf)) == 'SOMME_PONDEREE') then
  zech3(1)=zech1(1)+zech2(1) ! flux/tendances ajoutés.
  zechdv3(1)=(zechdv1(1)*zech1(1)+zechdv2(1)*zech2(1))/(zech1(1)+zech2(1)) ! variables moyennées.
elseif(cgconf(1:len_trim(cgconf)) == 'SOMME_CONTIGUE') then
  zech3(1)=zech1(1)+zech2(1) ! flux/tendances ajoutés.
  zechdv3(1)=zechdv1(1)+zechdv2(1) ! variables recopiées.
elseif(cgconf(1:len_trim(cgconf)) == 'DIFFE_EXP_REFE') then
  !
  ! Difference de fichiers de meme echeance.
  !
  zech3(1)=zech1(1) ! flux/tendances.
  zechdv3(1)=zechdv1(1) ! variables différenciées.
  if(cgconf(1:len_trim(cgconf)) == 'DIFFE_EXP_REFE') then
    !
    !-------------------------------------------------
    ! On effectue le test d'égalité des écheances.
    !-------------------------------------------------
    !
    if(abs((zech2(1)-zech1(1))/zech1(1)) > 1.e-5) then
      print*,'ddht/ERROR: DIFFE_EXP_REFE of two DDH files from different prediction ranges!...'
      print*,zech1(1),zech2(1)
      call exit(1)
    endif
  endif
elseif(cgconf(1:len_trim(cgconf)) == 'DIFFE_PONDEREE') then
  zech3(1)=zech2(1)-zech1(1) ! flux/tendances retranchés.
  zechdv3(1)=zechdv2(1)-zechdv1(1) ! variables retranchées.
elseif(cgconf(1:len_trim(cgconf)) == 'DIFFE_EC2_EC1') then
  zech3(1)=zech2(1)-zech1(1) ! flux/tendances retranchés.
  zechdv3(1)=zechdv2(1)-zechdv1(1) ! variables retranchées.
else
  zech3(1)=zech1(1)
  zechdv3(1)=zechdv1(1)
endif
!
! * Determination des articles de documentation des domaines.
!
if(cgconf(1:len_trim(cgconf)) == 'MOY_VERTIC') then
  !
  ! Les articles de documentation des domaines
  ! sont a modifier.
  !
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_HORIZ') then
  !
  ! Les articles de documentation des domaines
  ! sont a modifier.
  !
elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_DOMAIN') then
  !
  ! Les articles de documentation des domaines
  ! sont a modifier.
  !
  do jindoma=1,indoma
    !
    ! Le domaine courant est a recopier dans le fichier de sortie.
    !
    do jdoc=1,jpdoc
      zdocd3(jdoc,jindoma)=zdocd1(jdoc,idoma(jindoma))
    enddo
  enddo
else
  !
  ! Dans tous les autres cas, la documentation
  ! des domaines est celle du fichier d'entree 1.
  !
  idom3=idom1
  do jdom=1,idom3
    do jdoc=1,jpdoc
      zdocd3(jdoc,jdom)=zdocd1(jdoc,jdom)
    enddo
  enddo
endif
!
! ** Ecriture des articles documentaires du fichier de sortie.
!
call lfaouv(iul3,cgnfso,'W')
call lfapreci(iul3,jpprecint)
call lfaprecr(iul3,jpprecree)
call lfames(iul3,imes)
call lfaecrc(iul3,'INDICE EXPERIENCE',clnamx3,1)
call lfaecri(iul3,'DATE',idatef3,11)
call lfaecri(iul3,'DOCFICHIER',idocfi3,17)
call lfaecrr(iul3,'ECHEANCE',zech3,1)
call lfaecrr(iul3,'ECHEANCEDV',zechdv3,1)
do jdom=1,idom3
  do jdoc=1,jpdoc
    zdocd(jdoc)=zdocd3(jdoc,jdom)
  enddo
  call lited(iul3,jdom,'W',zdocd,irep)
enddo
if(cgconf(1:len_trim(cgconf)) == 'SEPAR_PR_COMPL') then
  !
  ! On écrit sur le lfa de sortie les articles
  ! de coordonnées permettant le tracé
  ! ultérieur.
  !
  call lfacas(iul1,'VCP0',cltype,ilong,irep)
  if(irep == 0) then
    cladcou='VCP0'
    call lfalecr(iul1,cladcou,jpprod,zpre_ini_1,ilong,irep)
    call lfaecrr(iul3,cladcou,zpre_ini_1,ilong)
    cladcou='VCZ0'
    call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
    call lfaecrr(iul3,cladcou,zprod1,ilong)
    cladcou='VCP1'
    call lfalecr(iul1,cladcou,jpprod,zpre_fin_1,ilong,irep)
    call lfaecrr(iul3,cladcou,zpre_fin_1,ilong)
    cladcou='VCZ1'
    call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
    call lfaecrr(iul3,cladcou,zprod1,ilong)
  else
    cladcou='VPP0'
    call lfalecr(iul1,cladcou,jpprod,zpre_ini_1,ilong,irep)
    call lfaecrr(iul3,cladcou,zpre_ini_1,ilong)
    cladcou='VEP0'
    call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
    call lfaecrr(iul3,cladcou,zprod1,ilong)
    cladcou='VPP1'
    call lfalecr(iul1,cladcou,jpprod,zpre_fin_1,ilong,irep)
    call lfaecrr(iul3,cladcou,zpre_fin_1,ilong)
    cladcou='VEP1'
    call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
    call lfaecrr(iul3,cladcou,zprod1,ilong)
  endif
  cladcou='PPP'
  call lfalecr(iul1,cladcou,jpprod,zpre_cum_1,ilong,irep)
  call lfaecrr(iul3,cladcou,zpre_cum_1,ilong)
endif
!
! ** Traitement des articles scientifiques.
!
! Boucle sur les champs.
!
do jlisct=1,ilisct
  clc=cllisct(jlisct)
  if((clc(2:3) == 'A1'.and.idocfi3(3) == 0) &
    &.or.(clc(2:3) == 'A2'.and.idocfi3(3) == 0) &
    &.or.(clc(2:3) == 'A3'.and.idocfi3(3) == 0) &
    &.or.(clc(2:3) == 'SS'.and.idocfi3(4) == 0) &
    &.or.clc(1:9) == 'DYNAMIQUE' &
    &) then
    !
    ! L'utilisateur a demandé de traiter
    ! des champs qui, d'après l'autodocumentation
    ! du fichier, ne peuvent y être.
    ! Afin de ne pas lancer de lecture inutile,
    ! on passe directement au champ suivant.
    !
    !if(clc(1:5) /= 'SVGFS' .and. clc(1:5) /= 'SFGFS') then
    !  print*,'ddht: ATTENTION: champ ',clc(1:13),' ignoré.'
    !endif
    cycle
  endif
  if (cgconf(1:len_trim(cgconf)) == 'SEPAR_PR_COMPL') then
    !
    ! Separation de la tendance totale en termes
    ! principal et complementaire.
    ! Le terme principal est celui
    ! donne explicitement par la
    ! liste des champs cllisct.
    ! Le terme complementaire est (Vxx1-Vxx0-principal).
    !
    if(lltous) then
      print*,'ddht/ERROR: to separate in tendencies '
      print*,'an explicit list of fields is required!'
                        call exit(1)
    endif
    if(jlisct == 1) then
      !
      ! Debut de liste des champs a traiter.
      !
      cladcou=cllisct(1)(1:4)
      call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
      call lfaecrr(iul3,cladcou,zprod1,ilong)
      cladcou=cllisct(2)(1:4)
      call lfalecr(iul1,cladcou,jpprod,zprod2,ilong,irep)
      call lfaecrr(iul3,cladcou,zprod2,ilong)
      !
      ! Il faut initialiser a zero le tableau de cumul.
      !
      do jprod=1,jpprod
        zcumt(jprod)=0.
      enddo
    else
      if(clc(1:1) == 'T'.and.clc(4:9) == 'DELTAP') then
        !
        ! L'article à cumuler est la tendance
        ! due a la variation de masse:
        ! -0.5*(Vx0/VPP0+Vx1/VPP1)*(VPP1-VPP0)*t_flux/t_var.
        !
        do jprod=1,imaxv1
          zprod3(jprod)=-0.5*(zprod1(jprod)/zpre_ini_1(jprod)+ &
&             zprod2(jprod)/zpre_fin_1(jprod))*(zpre_fin_1(jprod)-zpre_ini_1(jprod))*zech1(1)/ &
&             zechdv1(1)
        enddo
        !
        ! Cumul de l'article de ddh dans le tableau de tendance zcumt.
        !
        call cumul(clc,jpprod,zprod3,imaxv1,idom1,ilev1,zcumt)
      elseif(clc(1:1) == 'F'.or.clc(1:1) == 'T' .or. clc(1:2) == 'SF') then
        !
        ! Lecture de l'article.
        !
        call lfalecr(iul1,clc,jpprod,zprod3,ilong,irep)
        if(irep /= 0) then
          !
          !-------------------------------------------------
          ! L'article n'existe pas. On le met à une valeur nulle.
          !-------------------------------------------------
          !
          zprod3=0.
          !
          !-------------------------------------------------
          ! Afin de fournir la valeur correcte de ilong, on la prend sur une variable ou un flux.
          !-------------------------------------------------
          !
          if(clc(1:1) == 'F') then
            call lfacas(iul1,'FCTRAYSOL1',cltype,ilong,irep)
          elseif(clc(1:1) == 'T') then
            call lfacas(iul1,'TCTDIVFLUHOR',cltype,ilong,irep)
          else
            write(*,fmt=*) 'ddht/ERROR: unknown variable type!...'
            write(*,fmt=*) clc(1:1)
                                                call exit(1)
          endif
        endif
        !
        ! Cumul de l'article de ddh dans le tableau de tendance zcumt.
        !
        call cumul(clc,jpprod,zprod3,ilong,idom1,ilev1,zcumt)
      endif
    endif
    if(jlisct == ilisct) then
      !
      ! On a lu le dernier article de la liste des champs.
      ! Le resultat courant est donc a porter dans le fichier de DDH.
      !
      write(cltita,fmt='(A1,A2,A10)') 'T',cllisct(1)(2:3) &
&         ,'PRINCIPAL '
      call lfaecrr(iul3,cltita,zcumt,imaxv1)
      !
      ! Calcul du terme complementaire: COMPLEMENT =  Xfinal-Xinitial-PRINCIPAL
      !
      do jprod=1,imaxv1
        zprod2(jprod)=(zprod2(jprod)/zpre_fin_1(jprod)-zprod1(jprod)/ &
&           zpre_ini_1(jprod))/zechdv1(1)*zpre_cum_1(jprod)-zcumt(jprod)
      enddo
      write(cltita,fmt='(A1,A2,A10)') 'T',cllisct(1)(2:3) &
&         ,'COMPLEMENT'
      call lfaecrr(iul3,cltita,zprod2,imaxv1)
    endif
  669     continue
  elseif(cgconf(1:len_trim(cgconf)) == 'CALC') then
    !
    ! Fonction calculatrice.
    ! On rend fatale toute erreur de lecture du fichier.
    !
    call lfaerf(iul1,.true.)
    !
    ! On casse la chaîne en ses différents mots:
    ! il peut y avoir plusieurs ordres par ligne.
    !
    call casc(clc,1,clmot,ilmot)
    !
    ! Boucle sur les différents ordres de la ligne.
    !
    do jmot=1,ilmot
      clc=clmot(jmot)
      ilc=len_trim(clc)
      if(jmot == 1.and.llpremr) then
        !
        ! Premier mot de la première ligne rencontré.
        !
        if(clc(1:ilc) == 'NCOORD') then
          !
          ! L'utilisateur veut que les coordonnées
          ! (champs VPP0, 1, PPP, etc...)
          ! ne soient pas recopiées dans le fichier de sortie.
          !
        else
          !
          ! L'utilisateur veut que les coordonnées
          ! (champs VPP0, 1, PPP, etc...)
          ! soient recopiées dans le fichier de sortie.
          !
          ! On écrit sur le lfa de sortie les articles
          ! de coordonnées permettant le tracé
          ! ultérieur.
          !
          call lfacas(iul1,'VCP0',cltype,ilong,irep)
          if(irep == 0) then
            cladcou='VCP0'
            call lfalecr(iul1,cladcou,jpprod,zpre_ini_1,ilong,irep)
            call lfaecrr(iul3,cladcou,zpre_ini_1,ilong)
            cladcou='VCZ0'
            call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
            call lfaecrr(iul3,cladcou,zprod1,ilong)
            cladcou='VCP1'
            call lfalecr(iul1,cladcou,jpprod,zpre_fin_1,ilong,irep)
            call lfaecrr(iul3,cladcou,zpre_fin_1,ilong)
            cladcou='VCZ1'
            call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
            call lfaecrr(iul3,cladcou,zprod1,ilong)
          else
            cladcou='VPP0'
            call lfalecr(iul1,cladcou,jpprod,zpre_ini_1,ilong,irep)
            call lfaecrr(iul3,cladcou,zpre_ini_1,ilong)
            cladcou='VEP0'
            call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
            call lfaecrr(iul3,cladcou,zprod1,ilong)
            cladcou='VPP1'
            call lfalecr(iul1,cladcou,jpprod,zpre_fin_1,ilong,irep)
            call lfaecrr(iul3,cladcou,zpre_fin_1,ilong)
            cladcou='VEP1'
            call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
            call lfaecrr(iul3,cladcou,zprod1,ilong)
          endif
          cladcou='PPP'
          call lfalecr(iul1,cladcou,jpprod,zpre_cum_1,ilong,irep)
          call lfaecrr(iul3,cladcou,zpre_cum_1,ilong)
        endif
        llpremr=.false.
      endif
      if(clc(1:ilc) == 'NCOORD') then
      elseif(clc(1:ilc) == '+') then
        !
        ! Addition.
        !
        do jprod=1,jpprod
          zcalcx(jprod)=zcalcx(jprod)+zcalcy(jprod)
          zcalcy(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalcv(jprod)
        enddo
      elseif(clc(1:ilc) == '-') then
        !
        ! Soustraction.
        !
        do jprod=1,jpprod
          zcalcx(jprod)=zcalcy(jprod)-zcalcx(jprod)
          zcalcy(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalcv(jprod)
        enddo
      elseif(clc(1:ilc) == '/') then
        !
        ! Division.
        !
        do jprod=1,jpprod
          zcalcx(jprod)=zcalcy(jprod)/zcalcx(jprod)
          zcalcy(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalcv(jprod)
        enddo
      elseif(clc(1:ilc) == '*') then
        !
        ! Multiplication.
        !
        do jprod=1,jpprod
          zcalcx(jprod)=zcalcy(jprod)*zcalcx(jprod)
          zcalcy(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalcv(jprod)
        enddo
      elseif(clc(1:ilc) == 'X<>Y') then
        !
        ! Permutation d'X et Y.
        !
        do jprod=1,jpprod
          ztmp=zcalcx(jprod)
          zcalcx(jprod)=zcalcy(jprod)
          zcalcy(jprod)=ztmp
        enddo
      elseif(clc(1:ilc) == 'ECHEANCE') then
        do jprod=1,jpprod
          zcalcv(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalcy(jprod)
          zcalcy(jprod)=zcalcx(jprod)
          zcalcx(jprod)=zech1(1)
        enddo
      elseif(clc(1:ilc) == 'ECHEANCEDV') then
        do jprod=1,jpprod
          zcalcv(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalcy(jprod)
          zcalcy(jprod)=zcalcx(jprod)
          zcalcx(jprod)=zechdv1(1)
        enddo
      elseif(clc(1:4) == 'ECR"') then
        !
        ! La valeur de la pile X doit être portée sur fichier.
        !
        cladcou=' '
        cladcou(1:ilc-1-5+1)=clc(5:ilc-1)
        call lfaecrr(iul3,cladcou,zcalcx,idimpile)
      elseif(clc(1:4) == 'F>V') then
        !
        ! Passage d'un flux à une tendance sur les niveaux de variables.
        !
        if(idimpile /= (ilev1+1)*idom1) then
          print* &
&             ,'ddht/ERROR: convert flux > tendency on a field, which is not a flux!...'
          print*,idimpile,ilev1,idom1,clc
          call exit(1)
        endif
        !
        ! On initialise à 0 le tableau de cumul.
        !
        do jprod=1,jpprod
          zcumt(jprod)=0.
        enddo
        !
        ! On cumule dedans le flux avec opération
        ! de différenciation verticale.
        !
        call cumul(clc,jpprod,zcalcx,idimpile,idom1,ilev1,zcumt)
        !
        ! Le résultat zcumt est porté sur la pile X.
        !
        idimpile=ilev1*idom1 ! dimension de sortie (variable).
        do jprod=1,idimpile
          zcalcx(jprod)=zcumt(jprod)
        enddo
    elseif(clc(1:4) == 'V>F') then
        !
        ! Passage d'une tendance ou variable à un flux.
        !
        if(idimpile /= ilev1*idom1) then
          print* &
&             ,'ddht/ERROR: convert tendency > flux on a field, which is not a variable nor a tendency !...'
          print*,idimpile,ilev1,idom1,clc
          call exit(1)
        endif
        !
        ! On initialise à 0 le tableau de cumul.
        !
        do jprod=1,jpprod
          zcumt(jprod)=0.
        enddo
        !
        ! On cumule dans zcumt le cumul sur la verticale de zcalcx.
        !
        ilev1flux=ilev1+1 ! nb de niveaux de flux.
        call cumul(clc,jpprod,zcalcx,idimpile,idom1,ilev1flux,zcumt)
        !
        ! Le résultat zcumt est porté sur la pile X.
        !
        idimpile=ilev1flux*idom1 ! dimension de sortie (variable).
        do jprod=1,idimpile
          zcalcx(jprod)=zcumt(jprod)
        enddo
      elseif(clc(1:1) == 'V'.or.clc(1:1) == 'T'.or.clc(1:1) == 'F' .or. clc(1:2) == 'SF') then
        !
        ! Lecture sur fichier d'un article de DDH.
        !
        do jprod=1,jpprod
          zcalcv(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalcy(jprod)
          zcalcy(jprod)=zcalcx(jprod)
        enddo
        if(clc(4:9) == 'DELTAP') then
          !
          ! L'article souhaité est la tendance
          ! due a la variation de masse:
          ! -0.5*(Vx0/VPP0+Vx1/VPP1)*(VPP1-VPP0)*t_flux/t_var.
          !
          cladcou='V'//clc(2:3)//'0'
          call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
          cladcou='V'//clc(2:3)//'1'
          call lfalecr(iul1,cladcou,jpprod,zprod2,ilong,irep)
          idimpile=ilong
          do jprod=1,idimpile
            zcalcx(jprod)=-0.5*(zprod1(jprod)/zpre_ini_1(jprod)+ &
&               zprod2(jprod)/zpre_fin_1(jprod))*(zpre_fin_1(jprod)-zpre_ini_1(jprod))*zech1(1)/ &
&               zechdv1(1)
          enddo
          do jprod=idimpile+1,jpprod
            zcalcx(jprod)=0.
          enddo
        else
          !
          ! L'article souhaité est une simple
          ! lecture sur fichier DDH.
          !
          call lfalecr(iul1,clc,jpprod,zcalcx,ilong,irep)
          idimpile=ilong
        endif
      elseif(clc(1:ilc) == 'COMPLEMENT') then
        !
        ! Calcul du complément : final - initial - somme des flux ou tendances.
        !
        if(idimpile == 0) then
          write(*,fmt=*)
          write(*,fmt=*) 'ddht/ERROR: idimpile = 0 !...'
          write(*,fmt=*) 'clc=',trim(clc)
          call exit(1)
        elseif(idimpile == iindef) then
          write(*,fmt=*)
          write(*,fmt=*) 'ddht/ERROR: idimpile = iindef !...'
          write(*,fmt=*) 'clc=',trim(clc)
          call exit(1)
        elseif(idimpile == imaxv1) then
          !
          ! En sortie on veut des tendances.
          ! Calcul du complément à l'aide
          ! des valeurs de la pile X, Y et Z,
          ! par PPP/echdv*(Y/VPP1-Z/VPP0)-X.
          !
          do jprod=1,idimpile
            zcalcx(jprod)=(zcalcy(jprod)/zpre_fin_1(jprod)-zcalcz(jprod) &
              & /zpre_ini_1(jprod))/zechdv1(1)*zpre_cum_1(jprod)-zcalcx(jprod)
            zcalcz(jprod)=zcalct(jprod)
            zcalcy(jprod)=zcalct(jprod)
            zcalct(jprod)=zcalcu(jprod)
            zcalcu(jprod)=zcalcv(jprod)
          enddo
        elseif(idimpile == imaxv1flux) then
          !
          ! En sortie on veut des flux.
          ! Calcul du complément à l'aide
          ! des valeurs de la pile X, Y et Z,
          ! par Y-Z-X.
          !
          do jprod=1,idimpile
            zcalcx(jprod)=zcalcy(jprod)-zcalcz(jprod)-zcalcx(jprod)
            zcalcz(jprod)=zcalct(jprod)
            zcalcy(jprod)=zcalct(jprod)
            zcalct(jprod)=zcalcu(jprod)
            zcalcu(jprod)=zcalcv(jprod)
          enddo
        else
          write(*,fmt=*)
          write(*,fmt=*) 'ddht/ERROR: unexpected idimpile value !...'
          write(*,fmt=*) 'idimpile=',idimpile
          write(*,fmt=*) 'clc=',trim(clc)
          call exit(1)
        endif
      else
        !
        ! Entrée numérique.
        !
        call carree(clc,ilc,zval)
        do jprod=1,imaxv1
          zcalcv(jprod)=zcalcu(jprod)
          zcalcu(jprod)=zcalct(jprod)
          zcalct(jprod)=zcalcz(jprod)
          zcalcz(jprod)=zcalcy(jprod)
          zcalcy(jprod)=zcalcx(jprod)
          zcalcx(jprod)=zval
        enddo
        if(idimpile == 0) idimpile=1 ! cas d'une entrée numérique avant celle d'un champ DDH.
      endif
    enddo
  else
    !
    ! On n'est pas dans le cas d'une separation de tendance
    ! ni de la calculatrice.
    ! Lecture d'article sur le fichier d'entree 1.
    !
    cladcou=clc
    inlev=idocfi1(6)
    call longa(cladcou,inlev,ilev1)
    call lfalecr(iul1,cladcou,jpprod,zprod1,ilong,irep)
    iprod1=ilong
    if(cladcou(1:2) == 'SF' .or. (irep == 0.and.inivv >= 0 .and. .not. (cladcou(1:1) == 'V' &
    & .and. len_trim(cladcou) > 4))) then
      !
      ! L'article existe dans le fichier 1.
      ! On le lit eventuellement dans le fichier 2.
      !
      if (cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE'.or.trim(cgconf) == 'INTERPOL') then
        !
        ! Il faut lire l'article du fichier 2.
        !
        call lfalecr(iul2,cladcou,jpprod,zprod2,ilong,irep)
        iprod2=ilong
      endif
      if (cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE') then
        !
        ! Il faut calculer une combinaison de deux champs.
        !
        if(cladcou(1:1) == 'G') then
          !-------------------------------------------------
          ! Flux en surface: G12 (ARPEGE).
          !-------------------------------------------------
          zech1_scal=zech1(1)
          zech2_scal=zech2(1)
          call somd_sol(cladcou,iul1,iul2,iul3,idocfi1(12),idocfi1(14),zech1_scal,zech2_scal,idom1,idoma,jpdom,indoma)
        elseif(cladcou(1:1) == 'S') then
          !-------------------------------------------------
          ! Variables libres  en surface: S04_0 (ARPEGE), SVTS1 (AROME), SFRAYSODW (AROME), etc.
          !-------------------------------------------------
          if(lgrens) print*,'CHAMPS LIBRES'
          zech1_scal=zech1(1)
          zech2_scal=zech2(1)
          call somd_libres(cladcou,iul1,iul2,iul3,idocfi1(16),idocfi1(17),zech1_scal,zech2_scal,idom1,idoma,jpdom,indoma)
        else
          !-------------------------------------------------
          ! Autres cas: VCT0, VUU1, FCTRAYSOL1, etc.
          !-------------------------------------------------
          if(irep == 0) call somd_alt(zprod1,zprod2,jpprod,ilong,ilev1 &
          & ,idom1 &
          & ,cladcou,cgconf,len_trim(cgconf),zech1,zech2,iul3 &
          & ,zpre_ini_1,zpre_fin_1,zpre_cum_1 &
          & ,zpre_ini_2,zpre_fin_2,zpre_cum_2 &
          & )
        endif
      elseif(cgconf(1:len_trim(cgconf)) == 'MOY_VERTIC') then
        !
        ! Profil horizontal.
        ! On va moyenner sur la verticale.
        !
        ib=0
        imodv=ilong/idom1 ! nombre de niveaux verticaux du champ courant.
        if(cladcou(1:1) == 'F') then
          !
          ! Flux sur les inter-niveaux;
          ! on conserve les deux valeurs sommet et base.
          !
          do jb=1,ilong
            imod=mod(jb-1,imodv)
            if(imod == 0.or.imod == imodv-1) then
              ib=ib+1
              zprod3(ib)=zprod1(jb)
            endif
          enddo
        else
          !
          ! Variables ou tendances sur les niveaux;
          ! on effectue la somme verticale.
          !
          do jb=1,ilong
            imod=mod(jb-1,imodv)
            if(imod == 0) zsom=0.
            zsom=zsom+zprod1(jb)
            if(imod == imodv-1) then
              ib=ib+1
              zprod3(ib)=zsom
            endif
          enddo
        endif
        !
        ! Ecriture du tableau resultat.
        !
        call lfaecrr(iul3,cladcou,zprod3,ib)
      elseif(cgconf(1:len_trim(cgconf)) == 'INTERPOL') then
        !
        !-------------------------------------------------
        ! Le profil 3 va recevoir le 1, interpolé sur la grille verticale du 2.
        !-------------------------------------------------
        !
        if(idom1 /= idom2) then
          write(*,fmt=*) ' '
          write(*,fmt=*) 'ddht/ERROR: the configuration "INTERPOL" deals only with vertical interpolation,'
          write(*,fmt=*) '  although the file 2 has a different number of domains than file 1.'
          write(*,fmt=*) 
        endif
        if(cladcou(1:1) == 'F') then
          ilev2cou=ilev2+1
          iprod3=idom1*(ilev2+1)
        else
          ilev2cou=ilev2
          iprod3=idom1*ilev2
        endif
        zech1_scal=zech1(1)
        zech2_scal=zech2(1)
        call interpolddh(cladcou,zech1_scal,zech2_scal,zprod1,ilev1,ilev2cou,idom1,idom2,iprod1,jpprod &
        &,zpre_ini_1,zpre_ini_2 &
        &,zpre_fin_1,zpre_fin_2 &
        &,zpre_cum_1,zpre_cum_2 &
        &,zprod3)
        !
        ! Ecriture du tableau resultat.
        !
        call lfaecrr(iul3,cladcou,zprod3,iprod3)
      elseif(cgconf(1:len_trim(cgconf)) == 'MOY_HORIZ') then
        !
        ! Profil vertical.
        ! On va moyenner sur l'horizontale.
        !
        imodv=ilong/idom1 ! nombre de niveaux verticaux du champ courant.
        znorm=1./float(idom1)
        do jlev=1,imodv
          zprod3(jlev)=0.
        enddo
        do jb=1,ilong
          ilev=mod(jb-1,imodv)+1
          zprod3(ilev)=zprod3(ilev)+zprod1(jb)*znorm
        enddo
        !
        ! Ecriture du tableau resultat.
        !
        call lfaecrr(iul3,cladcou,zprod3,imodv)
      elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_DOMAIN') then
        !
        ! Extraction de certains domaines du fichier lfa d'entree.
        !
        imodv=ilong/idom1 ! nombre de niveaux verticaux du champ courant.
        ilones=imodv*indoma ! longueur a ecrire en sortie.
        do jb=1,ilong
          idomcou=(jb-1)/ilev1+1 ! numero du domaine courant.
          do jindoma=1,indoma
            if(idomcou == idoma(jindoma)) then
              !
              ! Le domaine courant est a recopier dans le fichier de sortie.
              !
              ilev=mod(jb-1,imodv)+1 ! niveau vertical du point courant.
              iprod3=(jindoma-1)*ilev1+ilev ! abscisse du point courant dans le tableau de cumul unidimensionnel.
              zprod3(iprod3)=zprod1(jb)
            endif
          enddo
        enddo
        !
        ! Ecriture du tableau resultat.
        !
        call lfaecrr(iul3,cladcou,zprod3,ilones)
      elseif(cgconf(1:len_trim(cgconf)) == 'EXTRAIT_NIVEAUX') then
        !
        ! Extraction de certains niveaux du fichier lfa d'entree.
        !
        imodv=ilong/idom1 ! nombre de niveaux verticaux du champ courant.
        !ilones=idom1*indoma ! longueur à ecrire en sortie. !!! bug
        if(imodv == ilev_ref) then
          !
          !-------------------------------------------------
          ! Champ de variable.
          !-------------------------------------------------
          !
          ilev_lit_ecr=indoma ! nb de niveaux à extraire.
        else
          !
          !-------------------------------------------------
          ! Champ de flux.
          !-------------------------------------------------
          !
          ilev_lit_ecr=indoma+1 ! nb de niveaux à extraire + 1.
        endif
        ilones=idom1*ilev_lit_ecr ! longueur à ecrire en sortie.
        iprod3=0
        do j1=1,idom1
          do j2=1,ilev_lit_ecr
            iprod3=iprod3+1
            if(j2 <= indoma) then
              ib=(j1-1)*imodv+idoma(j2)
            else
              ib=(j1-1)*imodv+idoma(j2-1)+1
            endif
            zprod3(iprod3)=zprod1(ib)
          enddo
        enddo
        !
        ! Ecriture du tableau resultat.
        !
        call lfaecrr(iul3,cladcou,zprod3,ilones)
      endif
    endif
  endif
enddo ! jlisct
!
! ** Fermeture du fichier d'entree.
!
call lfafer(iul1)
!
! ** Fermeture du fichier de sortie.
!
call lfafer(iul3)
end
subroutine cumul(cdc,kprod,pprod,klong,kdom1,klev1,pcumt)
! --------------------------------------------------------------------------
! **** *CUMUL* Cumul de tendances ou flux sur un tableau 2D.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 96-02-14, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
! En entree:
! cdc       nom de l'article à cumuler.
! kprod     dimension du tableau pprod.
! pprod     tableau des réels à cumuler.
! klong     nombre de réels à lire sur pprod.
! kdom1     nombre de domaines du champ à cumuler.
! klev1     nombre de niveaux du champ à cumuler.
!
! En sortie:
! pcumt     tableau recevant le cumul.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
INTEGER(KIND=4) :: KDOM1
INTEGER(KIND=4) :: KLEV1
INTEGER(KIND=4) :: KPROD
INTEGER(KIND=4) :: IFLUX
INTEGER(KIND=4) :: ILEV
INTEGER(KIND=4) :: IPROD
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: JLEV
INTEGER(KIND=4) :: KLONG

REAL(KIND=8) pprod(kprod)
REAL(KIND=8) pcumt(kprod)
REAL(KIND=8) zinterm(kdom1,klev1+1)
CHARACTER*(*) cdc
if(lgdebu) print*,'début cumul'
if(cdc(1:1) == 'F') then
  !
  ! Champ de flux.
  !
  ilev=klev1+1
  iflux=1
else
  !
  ! Champ de tendance.
  !
  ilev=klev1
  iflux=0
endif
!
! On copie le tableau 1D lu sur le fichier
! sur un tableau 2D afin de faciliter la différenciation.
!
iprod=0
do jdom=1,kdom1
  do jlev=1,ilev
    iprod=iprod+1
    zinterm(jdom,jlev)=pprod(iprod)
  enddo
enddo
if(iprod /= klong) then
  print*,'ddht/CUMUL/ERROR: wrong lengths: ',iprod,klong
        call exit(1)
endif
if(iflux == 1) then
  !
  ! Champ de flux.
  ! On opère la différenciation sur la verticale.
  !
  do jdom=1,kdom1
    do jlev=1,klev1
      zinterm(jdom,jlev)=zinterm(jdom,jlev)-zinterm(jdom,jlev+1)
    enddo
  enddo
endif
!
! Cumul sur le tableau de sortie.
!
iprod=0
do jdom=1,kdom1
  do jlev=1,klev1
    iprod=iprod+1
    pcumt(iprod)=pcumt(iprod)+zinterm(jdom,jlev)
  enddo
enddo
if(lgdebu) print*,'fin cumul'
end
subroutine inide(kul,cdlisde,kdoma,kdom,kndoma)
! --------------------------------------------------------------------------
! **** *INIDE* Initialisation des domaines a extraire.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 95-03-15, J.M. Piriou.
!
! Modifications:
! --------------
! 2006-04-04, J.M. Piriou: prise en compte de domaines donnés par leur long. et lat.
! --------------------------------------------------------------------------
! En entree:
! kul unité logique du fichier LFA de DDH.
! cdlisde   chaine contenant les domaines a extraire.
!
! En sortie:
! kdoma     tableau contenant les domaines a extraire.
! kdom      dimension physique de kdoma.
! kndoma    nombre d'entiers ecrits sur kdoma (i.e. nombre total de domaines a extraire).
!
! Exemple:
! cdlisde='1,2,4' ==> kndoma=3, kdom(1)=1, kdom(2)=2, kdom(3)=4.
! cdlisde='7-9' ==> kndoma=3, kdom(1)=7, kdom(2)=8, kdom(3)=9.
! cdlisde='+001.374_+43.575_Toulouse-Meteopole,-000.692_+44.832_Bordeaux' ==> kndoma=2, kdom(1)=domaine proche de ce (lon,lat), kdom(2)=domaine proche de ce (lon,lat).
!
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
!
! Variables arguments.
!
CHARACTER*(*), intent(in) :: cdlisde
INTEGER(KIND=4), intent(in) :: kul,kdom
INTEGER(KIND=4), intent(out) :: kdoma(kdom),kndoma
!
! Variables locales.
!
CHARACTER *80 clmot(180)
INTEGER(KIND=4) :: ilmot,jmot,ipos_und,ipos_tra,idom1,idom2,jdom,idom
REAL(KIND=8) :: zdist
if(lgdebu) print*,'début inide'
!
!-------------------------------------------------
! Initialisation du nombre de domaines rencontrés.
!-------------------------------------------------
!
kndoma=0
!
!-------------------------------------------------
! On casse la chaîne fournie par l'utilisateur en n mots, 
! le séparateur entre mots étant la virgule.
!-------------------------------------------------
!
call casc(cdlisde,10,clmot,ilmot)
!
!-------------------------------------------------
! Boucle sur le nombre de mots.
!-------------------------------------------------
!
do jmot=1,ilmot
  ipos_und=index(clmot(jmot),'_') ! underscore.
  ipos_tra=index(clmot(jmot),'-') ! trait d'union.
  if(ipos_und /= 0) then
    !
    !-------------------------------------------------
    ! Il y a un "_" dans le mot.
    ! L'utilisateur a fourni un couple (lon, lat) du type "-000.692_+44.832_Bordeaux".
    ! Recherche du domaine DDH le plus proche de ce couple (lon,lat).
    !-------------------------------------------------
    !
    call recddpp(kul,clmot(jmot),idom,zdist)
    kndoma=kndoma+1
    kdoma(kndoma)=idom
    !
    !-------------------------------------------------
    ! Affichage sur output standard de la distance au domaine le plus proche.
    !-------------------------------------------------
    !
    write(*,fmt=*) 'ddht: domaine le plus proche: '
    write(*,fmt=*) '  numéro = ',idom
    write(*,fmt=*) '  dist(km) = ',zdist/1000.
  elseif (ipos_tra /= 0) then
    !
    !-------------------------------------------------
    ! Il y a un "-" dans le mot.
    ! L'utilisateur a fourni une liste de domaines du type "5-9".
    !-------------------------------------------------
    !
    read(clmot(jmot)(1:ipos_tra-1),fmt=*) idom1
    read(clmot(jmot)(ipos_tra+1:),fmt=*) idom2
    do jdom=idom1,idom2
      kndoma=kndoma+1
      kdoma(kndoma)=jdom
    enddo
  else
    !
    !-------------------------------------------------
    ! Il n'y a ni "_" ni "-" dans le mot.
    ! L'utilisateur a donc fourni un domaine simple, type "8".
    !-------------------------------------------------
    !
    kndoma=kndoma+1
    read(clmot(jmot),fmt=*) kdoma(kndoma)
  endif
enddo
if(lgdebu) print*,'fin inide'
end
subroutine recddpp(kul,cdmot,kdom,pdist)
! --------------------------------------------------------------------------
! **** *recddpp* Recherche du domaine DDH le plus proche d'un couple (lon,lat) donné.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 2006-04-05, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
! En entree:
! kul unité logique du fichier LFA de DDH.
! cdmot   chaine contenant le couple (lon,lat) au format "+001.374_+43.575_Toulouse-Meteopole".
!
! En sortie:
! kdom   numéro du domaine DDH le plus proche.
! pdist  distance (en m) entre le (lon,lat) utilisateur et le domaine DDH le plus proche.
!
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p) 
#include"ddhpar.h"
#include"ddht_yom_ent.h"
!
! Variables arguments.
!
CHARACTER*(*), intent(in) :: cdmot
INTEGER(KIND=4), intent(in) :: kul
INTEGER(KIND=4), intent(out) :: kdom
REAL(KIND=8), intent(out) :: pdist
!
! Variables locales.
!
CHARACTER *80 clmot(180),clart
INTEGER(KIND=4) :: ilmot,ilong,ierr,idom,jdom
INTEGER(KIND=4) idocfi(17)
REAL(KIND=8) zdocd(11),zmodyx
REAL(KIND=8) :: zlon,zlat,zpi,zmpi,zlon1,zlon2,zlon3,zlon4
REAL(KIND=8) :: zlat1,zlat2,zlat3,zlat4,zproscal,zproscal_loc,zlonu,zlatu,zproscalx,zcondr
if(lgdebu) print*,'début recddpp'
!
!-------------------------------------------------
! On casse la chaîne d'entrée en mots, le séparateur étant l'underscore.
!-------------------------------------------------
!
call casc(cdmot,-95,clmot,ilmot)
!
!-------------------------------------------------
! Lecture de longitude et latitude.
!-------------------------------------------------
!
read(clmot(1),fmt=*) zlonu
read(clmot(2),fmt=*) zlatu
!
!-------------------------------------------------
! Conversion en radians.
!-------------------------------------------------
!
zcondr=atan(1.)/45.
zlonu=zlonu*zcondr
zlatu=zlatu*zcondr
!
!-------------------------------------------------
! Lecture de l'autodocumentation des domaines du fichier de DDH.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Nombre de domaines.
!-------------------------------------------------
!
call lfaleci(kul,'DOCFICHIER',17,idocfi,ilong,ierr)
idom=idocfi(15) ! nombre de domaines.
!
!-------------------------------------------------
! Boucle sur les domaines de ce fichier DDH,
! pour rechercher celui dont le centre de gravité est le plus proche
! du (lon,lat) demandé.
!-------------------------------------------------
!
zproscalx=-1.01 ! on initialise le produit scalaire entre vecteurs normés à celui de la distance maximale.
kdom=1
do jdom=1,idom
  !
  ! On lit la documentation du domaine jdom.
  !
  call lited(kul,jdom,'R',zdocd,irep)
  !
  !-------------------------------------------------
  ! On gère chaque type de domaine: points limités, quadrilatère, etc.
  !-------------------------------------------------
  !
  zpi=4.*atan(1.)
  zmpi=-zpi
  if(nint(zdocd(11)) == 1.or.nint(zdocd(11)) == 4) then
    !
    ! Type point.
    !
    zlon=zdocd(3)
    zlat=asin(zdocd(4))
    zlon=zmodyx(zlon,zmpi,zpi)
  elseif(nint(zdocd(11)) == 2) then
    !
    ! Type quadrilatère.
    !
    zlon1=zdocd(3)
    zlon2=zdocd(5)
    zlon3=zdocd(7)
    zlon4=zdocd(9)
    !
    zlat1=asin(zdocd(4))
    zlat2=asin(zdocd(6))
    zlat3=asin(zdocd(8))
    zlat4=asin(zdocd(10))
    !
    zlon1=zmodyx(zlon1,zmpi,zpi)
    zlon2=zmodyx(zlon2,zmpi,zpi)
    zlon3=zmodyx(zlon3,zmpi,zpi)
    zlon4=zmodyx(zlon4,zmpi,zpi)
    !
    zlon=0.5*(zlon1+zlon2)
    zlat=0.5*(zlat1+zlat2)
  elseif(nint(zdocd(11)) == 3) then
    !
    ! Type rectangle.
    !
    zlon1=zdocd(3)
    zlat1=asin(min(1.,max(-1.,zdocd(4))))
    zlon2=zdocd(5)
    zlat2=asin(min(1.,max(-1.,zdocd(8))))
    !
    zlon1=zmodyx(zlon1,zmpi,zpi)
    zlon2=zmodyx(zlon2,zmpi,zpi)
    if(zlon2 < zlon1) zlon2=zlon2+360.
    zlon=0.5*(zlon1+zlon2)
    zlat=0.5*(zlat1+zlat2)
  elseif(nint(zdocd(11)) == 5) then
    !
    ! Type globe.
    !
    zlon=0.
    zlat=0.
  elseif(nint(zdocd(11)) == 6) then
    !
    ! Type bande zonale.
    !
    zlon=0.
    zlat=asin(zdocd(4))
  else
    print*,'ddht/WARNING: the kind of domain ',nint(zdocd(11)),' is not yet taken into account!...'
    write(*,fmt=*) 'zdocd(11)=',zdocd(11)
    cycle
  endif
  !
  !-------------------------------------------------
  ! A ce stade on a la position du domaine DDH dans (zlon,zlat)
  ! et celle demandée par l'utilisateur dans (zlonu,zlatu).
  ! Calcul du produit scalaire de ces 2 vecteurs.
  !-------------------------------------------------
  !
  zproscal_loc=zproscal(zlon,zlat,zlonu,zlatu)
  zdist=6371229.*acos(zproscal_loc)
  write(*,fmt=*) 'ddht/recherche intermédiaire : domaine n° ',jdom,' distance = ',zdist,' m'
  if(zproscal_loc > zproscalx) then
    !
    !-------------------------------------------------
    ! Le domaine courant est plus proche
    ! de celui souhaité par l'utilisateur
    ! que le précédent.
    !-------------------------------------------------
    !
    kdom=jdom
    pdist=zdist
    zproscalx=zproscal_loc
  endif
enddo
if(lgdebu) print*,'fin recddpp'
end
function zproscal(plon,plat,plonu,platu)
! --------------------------------------------------------------------------
! **** *zproscal* Produit scalaire de deux vecteurs normés donnés par leur (lon,lat).
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 2006-04-05, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
REAL(KIND=8), intent(in) :: plon,plat,plonu,platu
REAL(KIND=8) :: zproscal
zproscal=cos(plat)*cos(plon)*cos(platu)*cos(plonu) &
& +cos(plat)*sin(plon)*cos(platu)*sin(plonu) &
& +sin(plat)*sin(platu)
end
subroutine somd_alt(pprod1,pprod2,kprod,klong,klev1,kdom1 &
&   ,cdadcou,cdconf,klconf,pech1,pech2,kul3 &
&   ,ppre_ini_1,ppre_fin_1,ppre_cum_1 &
&   ,ppre_ini_2,ppre_fin_2,ppre_cum_2 &
&   )
! --------------------------------------------------------------------------
! **** *somd_alt* Somme ou difference de deux fichiers de DDH, champs d'altitude.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 92-03-01, J.M. Piriou.
!
! Modifications:
! --------------
! 1992-12-04, J.M. Piriou: adaptation au calcul sur CRAY.
! 1994-10-27, J.M. Piriou: adaptation au calcul sur lfa.
! 2015-06-19, J.M. Piriou: création des articles VCZ et VCP, en cas de différence de fichiers.
!
! --------------------------------------------------------------------------
! En entree:
! pprod1, pprod2: champs scientifiques a additionner
! ou retrancher.
! kprod: taille physique de ces tableaux.
! klong: taille reelle de ces tableaux.
! klev1: nombre de niveaux.
! kdom1: nombre de domaines.
! cdadcou: nom de l'article.
! cdconf: configuration de travail a effectuer.
! klconf: nb de car. ecrits sur cdconf.
! pech1: echeance du fichier 1.
! pech2: echeance du fichier 2.
! kul3: unite logique de sortie.
! ppre_ini_1 VPP0 fichier 1.
! ppre_fin_1 VPP1 fichier 1.
! ppre_cum_1 PPP fichier 1.
! ppre_ini_2 VPP0 fichier 2.
! ppre_fin_2 VPP1 fichier 2.
! ppre_cum_2 PPP fichier 2.
! En sortie:
! ecriture d'un article sur l'unite logique kul3.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
INTEGER(KIND=4) :: KPROD
INTEGER(KIND=4) :: JPROD
INTEGER(KIND=4) :: KDOM1
INTEGER(KIND=4) :: KLCONF
INTEGER(KIND=4) :: KLEV1
INTEGER(KIND=4) :: KLONG
INTEGER(KIND=4) :: KUL3
REAL(KIND=8) :: pech1(1)
REAL(KIND=8) :: pech2(1)
REAL(KIND=8) :: ZCORR
REAL(KIND=8) :: ZDENO
REAL(KIND=8) :: ZPOIE1
REAL(KIND=8) :: ZPOIE2
REAL(KIND=8) :: ZPOIV1
REAL(KIND=8) :: ZPOIV2

REAL(KIND=8) pprod1(kprod)
REAL(KIND=8) pprod2(kprod)
REAL(KIND=8) zprod3(jpprod)
REAL(KIND=8) ppre_ini_1(kprod) ! VPP0 fichier 1.
REAL(KIND=8) ppre_fin_1(kprod) ! VPP1 fichier 1.
REAL(KIND=8) ppre_cum_1(kprod) ! PPP fichier 1.
REAL(KIND=8) ppre_ini_2(kprod) ! VPP0 fichier 2.
REAL(KIND=8) ppre_fin_2(kprod) ! VPP1 fichier 2.
REAL(KIND=8) ppre_cum_2(kprod) ! PPP fichier 2.
CHARACTER*(*) cdadcou,cdconf
character*200 clart
if(lgdebu) print*,'début somd_alt'
!
! Calcul des poids à appliquer lors de sommes ou différences.
!
zech1_scal=pech1(1)
zech2_scal=pech2(1)
call calpasd(cdconf,klconf,zech1_scal,zech2_scal,zpoiv1,zpoiv2,zdeno,zpoie1 &
&   ,zpoie2)
if(cdadcou(1:1) == 'V'.and.cdconf(1:len_trim(cdconf)) == 'DIFFE_EXP_REFE') then
  !
  ! Article de type variable, configuration="DIFFE_EXP_REFE".
  !
  do jprod=1,klong
    if(cdadcou(4:4) == '0' .and. cdadcou(2:3) /= 'PP') then
      zcorr=ppre_ini_1(jprod)/ppre_ini_2(jprod)
    elseif(cdadcou(4:4) == '1' .and. cdadcou(2:3) /= 'PP') then
      zcorr=ppre_fin_1(jprod)/ppre_fin_2(jprod)
    else
      zcorr=1.
    endif
    zprod3(jprod)=(pprod1(jprod)*zpoiv1+pprod2(jprod)*zpoiv2*zcorr)/zdeno
  enddo
elseif(cdadcou(1:1) == 'V') then
  !
  ! Article de type variable, autres configurations.
  !
  do jprod=1,klong
    zprod3(jprod)=(pprod1(jprod)*zpoiv1+pprod2(jprod)*zpoiv2)/zdeno
  enddo
else
  !
  ! Article de type extensif dans le temps.
  !
  do jprod=1,klong
    zprod3(jprod)=pprod1(jprod)*zpoie1+pprod2(jprod)*zpoie2
  enddo
endif
!
! Ecriture du champ combine sur le fichier de sortie.
! ---------------------------------------------------
!
if(cdadcou(1:3) == 'VPP'.or.cdadcou(1:3) == 'VEP') then
  !
  ! Variables de coordonnees instantanees.
  !
  if(cdconf(1:14) == 'SOMME_PONDEREE' &
&     .or.cdconf(1:14) == 'DIFFE_PONDEREE') then
    call lfaecrr(kul3,cdadcou,zprod3,klong)
  elseif(cdconf(1:14) == 'DIFFE_EC2_EC1 ' &
&     .and.cdadcou(4:4) == '1') then
    call lfaecrr(kul3,cdadcou,pprod2,klong)
    cdadcou(4:4)='0'
    call lfaecrr(kul3,cdadcou,pprod1,klong)
  elseif(cdconf(1:14) == 'DIFFE_EC2_EC1 ' &
&     .and.cdadcou(4:4) == '0') then
    !
    ! Rien a faire: ce champ n'a pas sa place
    ! dans le fichier resultant.
    !
  elseif(cdconf(1:14) == 'DIFFE_EXP_REFE') then
    call lfaecrr(kul3,cdadcou,zprod3,klong)
    !
    !-------------------------------------------------
    ! On va créer un article VCZ (coordonnée Z) ou VCP (coordonnée P), lequel est
    ! une simple copie (sans différence).
    !-------------------------------------------------
    !
    clart=cdadcou(1:4)
    if(cdadcou(1:3) == 'VPP') then
      clart(2:3)='CP'
    else
      clart(2:3)='CZ'
    endif
    call lfaecrr(kul3,clart,pprod1,klong)
  elseif(cdconf(1:14) == 'SOMME_CONTIGUE' &
&     .and.cdadcou(4:4) == '0') then
    call lfaecrr(kul3,cdadcou,pprod1,klong)
  elseif(cdconf(1:14) == 'SOMME_CONTIGUE' &
&     .and.cdadcou(4:4) == '1') then
    call lfaecrr(kul3,cdadcou,pprod2,klong)
  else
    print*,'DDHT/SOMDD/ERROR: var. non coord.'
    call exit(1)
  endif
elseif(cdadcou(1:1) == 'V' .or. cdadcou(1:2) == 'SV') then
  !
  ! Variables non de coordonnees instantanees.
  !
  if(cdconf(1:14) == 'DIFFE_EXP_REFE' &
&     .or.cdconf(1:14) == 'DIFFE_PONDEREE' &
&     .or.cdconf(1:14) == 'SOMME_PONDEREE') then
    call lfaecrr(kul3,cdadcou,zprod3,klong)
  elseif(cdconf(1:14) == 'DIFFE_EC2_EC1 ' &
&     .and.cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '1') then
    call lfaecrr(kul3,cdadcou,pprod2,klong)
    cdadcou(len_trim(cdadcou):len_trim(cdadcou))='0'
    call lfaecrr(kul3,cdadcou,pprod1,klong)
  elseif(cdconf(1:14) == 'DIFFE_EC2_EC1 ' &
&     .and.cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '0') then
    ! Rien a faire: ce champ n'a pas sa place
    ! dans le fichier resultant.
  elseif(cdconf(1:14) == 'SOMME_CONTIGUE' &
&     .and.cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '0') then
    call lfaecrr(kul3,cdadcou,pprod1,klong)
  elseif(cdconf(1:14) == 'SOMME_CONTIGUE' &
&     .and.cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '1') then
    call lfaecrr(kul3,cdadcou,pprod2,klong)
  else
    print*,'DDHT/SOMDD/WARNING: var. syntax unknown'
    print*,trim(cdconf)
    print*,trim(cdadcou)
    !call exit(1)
    return
  endif
elseif(cdadcou(1:1) == 'P' .or. cdadcou(1:2) == 'SF') then
  !
  ! Masse cumulee.
  !
  if(cdconf(1:14) == 'DIFFE_EXP_REFE') then
    call lfaecrr(kul3,cdadcou,pprod1,klong)
  else
    call lfaecrr(kul3,cdadcou,zprod3,klong)
  endif
elseif(cdadcou(1:1) == 'F'.or.cdadcou(1:1) == 'T' .or. cdadcou(1:2) == 'SF') then
  !
  ! Flux ou tendance.
  !
  call lfaecrr(kul3,cdadcou,zprod3,klong)
else
  !
  ! Erreur: type de champ non prevu.
  !
  print* &
&     ,'DDHT/SOMDD/ERROR: unexpected field for the cumulate/differenciate operation.'
  print*,cdadcou
  call exit(1)
endif
if(lgdebu) print*,'fin somd_alt'
end

subroutine calpasd(cdconf,klconf,pech1,pech2,ppoiv1,ppoiv2,pdeno, &
&   ppoie1,ppoie2)
! --------------------------------------------------------------
! **** *calpasd* Calcul des poids à appliquer lors de sommes ou différences.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! cdconf(1:klconf): ex: 'DIFFE_PONDEREE'
! pech1, pech2: échéances des fichiers d'entrée.
! En sortie:
! ppoiv1, ppoiv2: poids à appliquer aux variables des fichiers 1 et 2 pour les combiner.
! pdeno: dénominateur de normalisation.
! ppoie1, ppoie2: poids à appliquer aux champs extensifs dans le temps des fichiers 1 et 2 pour les combiner.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
INTEGER(KIND=4) :: KLCONF
REAL(KIND=8) :: PDENO
REAL(KIND=8) :: PECH1
REAL(KIND=8) :: PECH2
REAL(KIND=8) :: PPOIE1
REAL(KIND=8) :: PPOIE2
REAL(KIND=8) :: PPOIV1
REAL(KIND=8) :: PPOIV2
CHARACTER*(*) cdconf
if(lgdebu) print*,'début calpasd'
if(cdconf(1:klconf) == 'DIFFE_PONDEREE') then
  !
  ! On effectue la difference ponderee entre les deux fichiers.
  !
  ppoiv1=-pech1
  ppoiv2=pech2
  pdeno=ppoiv1+ppoiv2
  ppoie1=-1.
  ppoie2= 1.
elseif(cdconf(1:5) == 'DIFFE') then
  !
  ! On effectue la difference entre les deux fichiers.
  !
  ppoiv1=-1.
  ppoiv2= 1.
  pdeno= 1.
  ppoie1=-1.
  ppoie2= 1.
elseif(cdconf(1:5) == 'SOMME') then
  ppoiv1=pech1
  ppoiv2=pech2
  pdeno=ppoiv1+ppoiv2
  ppoie1= 1.
  ppoie2= 1.
else
  print*,'ddht/ERROR in calpasd: combination options.'
        call exit(1)
endif
if(lgdebu) print*,'fin calpasd'
end
subroutine somd_sol(cdadcou,kul1,kul2,kul3,knomvif,knomf,pech1,pech2 &
& ,kdom,kdoma,kddom,kndoma)
! --------------------------------------------------------------------------
! **** *somd_sol* Traitement des champs sol.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 94-11-16, J.M. Piriou.
!
! Modifications:
! --------------
! 96-03-26, J.M. Piriou: autres options que DIFFE_EC2_EC1.
! --------------------------------------------------------------------------
! En entree:
! kul1     unite logique du fichier d'entree 1.
! kul2     unite logique du fichier d'entree 2.
! kul3     unite logique du fichier de sortie 3.
! knomvif  nombre de variables initiales ou finales.
! knomf    nombre de flux.
! pech1    échéance du fichier 1.
! pech2    échéance du fichier 2.
! En sortie:
! ecriture des champs sol modifies sur le fichier de sortie kul3.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
!
INTEGER(KIND=4) :: KUL1
INTEGER(KIND=4) :: KUL2
INTEGER(KIND=4) :: KUL3
INTEGER(KIND=4) :: KNOMF
INTEGER(KIND=4) :: KNOMVIF
REAL(KIND=8) :: PECH1
REAL(KIND=8) :: PECH2
REAL(KIND=8) :: ZPOIV1
REAL(KIND=8) :: ZDENO
REAL(KIND=8) :: ZPOIE1
REAL(KIND=8) :: ZPOIV2
character(len=*) :: cdadcou
INTEGER(KIND=4) :: INOMVS
INTEGER(KIND=4) :: KDOM,kddom,jindoma
INTEGER(KIND=4) :: kdoma(kddom)
INTEGER(KIND=4) :: kndoma
INTEGER(KIND=4) :: JVAR
REAL(KIND=8) :: ZPOIE2
CHARACTER*200 :: CLSOL
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: JCHAMP
INTEGER(KIND=4) :: IREP
INTEGER(KIND=4) :: JDOM
REAL(KIND=8) :: zmoy(1)

CHARACTER*200 :: CGNFE1
CHARACTER*200 :: CGNFE2
CHARACTER*200 :: CGNFLC
CHARACTER*200 :: CGNFSO
LOGICAL :: LGFILAF
LOGICAL :: LGRENS

REAL(KIND=8) zsols1(jpdom) ! tableau de donnees au sol.
REAL(KIND=8) zsols2(jpdom) ! tableau de donnees au sol.
REAL(KIND=8) zsols3(jpdom) ! tableau de donnees au sol.
if(lgdebu) print*,'début somd_sol'
!
! On rend fatale toute erreur de lecture du fichier.
!
call lfaerf(kul1,.true.)
if(cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE') then
  !
  ! Il faut lire le deuxieme champ.
  !
  call lfaerf(kul2,.true.)
  !
  ! Calcul des poids à appliquer lors de sommes ou différences.
  !
  call calpasd(cgconf,len_trim(cgconf),pech1,pech2,zpoiv1,zpoiv2,zdeno, &
&     zpoie1,zpoie2)
  !
  ! Traitement des flux.
  !
  !
  ! Lecture des deux champs d'entrée.
  !
  call lfalecr(kul1,cdadcou,jpdom,zsols1,ilong,irep)
  call lfalecr(kul2,cdadcou,jpdom,zsols2,ilong,irep)
  !
  ! On combine les deux champs d'entrée.
  !
  do jdom=1,kdom
    zsols3(jdom)=zsols1(jdom)*zpoie1+zsols2(jdom)*zpoie2
  enddo
  call lfaecrr(kul3,cdadcou,zsols3,kdom)
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_HORIZ') then
  !
  ! Traitement des flux.
  !
  call lfalecr(kul1,cdadcou,jpdom,zsols1,ilong,irep)
  !
  ! Operation a effectuer sur le champ sol
  ! et reecriture d'icelui sur
  ! le fichier de sortie.
  !
  ! On va moyenner selon l'horizontale.
  !
  zmoy(1)=0.
  do jdom=1,ilong
    zmoy(1)=zmoy(1)+zsols1(jdom)/float(ilong)
  enddo
  call lfaecrr(kul3,cdadcou,zmoy,1)
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_VERTIC') then
  !
  ! Traitement des flux.
  !
  !
  !-------------------------------------------------
  ! Simple recopie d'un article d'un fichier à l'autre.
  !-------------------------------------------------
  !
  call lfacop(kul1,cdadcou,cdadcou,kul3)
elseif(trim(cgconf) == 'EXTRAIT_DOMAIN') then
  !
  ! Traitement des flux.
  !
  call lfalecr(kul1,cdadcou,jpdom,zsols1,ilong,irep)
  !
  ! Operation a effectuer sur le champ sol:
  ! extraire certains domaines,
  ! puis reecriture d'icelui sur le fichier de sortie.
  !
  do jindoma=1,kndoma
    zsols3(jindoma)=zsols1(kdoma(jindoma))
  enddo
  call lfaecrr(kul3,cdadcou,zsols3,kndoma)
else
  !
  ! Autres cas; ils sont non prevus.
  !
  print* &
&     ,'ddht/somd_sol 2/ATTENTION: traitement des champs sol de l''option ', &
&     cgconf(1:len_trim(cgconf)),' non encore implemente.'
  return
endif
if(lgdebu) print*,'fin somd_sol'
end
subroutine somd_libres(cdadcou,kul1,kul2,kul3,knomvif,knomf,pech1,pech2 &
& ,kdom,kdoma,kddom,kndoma)
! --------------------------------------------------------------------------
! **** *somd_libres* Traitement des champs libres.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 2003-10-29, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
! En entree:
! kul1     unite logique du fichier d'entree 1.
! kul2     unite logique du fichier d'entree 2.
! kul3     unite logique du fichier de sortie 3.
! knomvif  nombre de variables finales.
! knomf    nombre de flux.
! pech1    échéance du fichier 1.
! pech2    échéance du fichier 2.
! En sortie:
! ecriture des champs libres modifies sur le fichier de sortie kul3.
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
!
character(len=*) :: cdadcou
CHARACTER*200 :: CGNFE1
CHARACTER*200 :: CGNFE2
CHARACTER*200 :: CGNFLC
CHARACTER*200 :: CGNFSO
LOGICAL :: LGFILAF
LOGICAL :: LGRENS
INTEGER(KIND=4) :: KNOMF
INTEGER(KIND=4) :: KNOMVIF
INTEGER(KIND=4) :: KUL1
INTEGER(KIND=4) :: KUL2
INTEGER(KIND=4) :: KUL3
REAL(KIND=8) :: PECH1
REAL(KIND=8) :: PECH2
INTEGER(KIND=4) :: INOMVS
INTEGER(KIND=4) :: KDOM,kddom,jindoma
INTEGER(KIND=4) :: kdoma(kddom)
INTEGER(KIND=4) :: kndoma
REAL(KIND=8) :: ZDENO
REAL(KIND=8) :: ZPOIE1
REAL(KIND=8) :: ZPOIE2
REAL(KIND=8) :: ZPOIV1
REAL(KIND=8) :: ZPOIV2
CHARACTER*200 :: CLLIBRE
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: IREP
INTEGER(KIND=4) :: JCHAMP
INTEGER(KIND=4) :: JDOM
REAL(KIND=8) :: zmoy(1)

REAL(KIND=8) zlibres1(jpdom)
REAL(KIND=8) zlibres2(jpdom)
REAL(KIND=8) zlibres3(jpdom)
if(lgdebu) print*,'début somd_libres'
!
! On rend fatale toute erreur de lecture du fichier.
!
call lfaerf(kul1,.true.)
if(cgconf(1:5) == 'SOMME'.or.cgconf(1:5) == 'DIFFE') then
  !
  ! Il faut lire le deuxieme champ.
  !
  call lfaerf(kul2,.true.)
  !
  ! Calcul des poids à appliquer lors de sommes ou différences.
  !
  call calpasd(cgconf,len_trim(cgconf),pech1,pech2,zpoiv1,zpoiv2,zdeno, &
  & zpoie1,zpoie2)
  if(cdadcou(1:2) == 'SF') then
    
    !-------------------------------------------------
    ! Flux.
    !-------------------------------------------------
    !
    ! Traitement des flux.
    !
    !
    ! Lecture des deux champs d'entrée.
    !
    call lfalecr(kul1,cdadcou,jpdom,zlibres1,ilong,irep)
    call lfalecr(kul2,cdadcou,jpdom,zlibres2,ilong,irep)
    !
    ! On combine les deux champs d'entrée.
    !
    do jdom=1,kdom
      zlibres3(jdom)=zlibres1(jdom)*zpoie1+zlibres2(jdom)*zpoie2
    enddo
    call lfaecrr(kul3,cdadcou,zlibres3,kdom)
  else
    
    !-------------------------------------------------
    ! Variable.
    !-------------------------------------------------
    !
    ! Traitement des variables.
    !
    !
    ! Lecture des deux champs d'entrée.
    !
    call lfalecr(kul1,cdadcou,jpdom,zlibres1,ilong,irep)
    call lfalecr(kul2,cdadcou,jpdom,zlibres2,ilong,irep)
    !
    ! On combine les deux champs d'entrée.
    !
    do jdom=1,kdom
      zlibres3(jdom)=(zlibres1(jdom)*zpoiv1+zlibres2(jdom)*zpoiv2)/zdeno
    enddo
    if(cgconf(1:14) == 'DIFFE_EXP_REFE' &
  &         .or.cgconf(1:14) == 'DIFFE_PONDEREE' &
  &         .or.cgconf(1:14) == 'SOMME_PONDEREE') then
      call lfaecrr(kul3,cdadcou,zlibres3,kdom)
    elseif(cgconf(1:14) == 'DIFFE_EC2_EC1 ') then
      if(cdadcou(1:5) == 'SVGFS' .or. (cdadcou(len_trim(cdadcou):len_trim(cdadcou)) /= '0' .and. cdadcou(len_trim(cdadcou):len_trim(cdadcou)) /= '1')) then
        !-------------------------------------------------
        ! Cas de variables libres dont le fichier DDH ne contient que la valeur finale (initiale absente).
        ! Exemples: SVGFS01, SVGFS02 (ARPEGE), SVTCLS, SVOROG (AROME), etc.
        !-------------------------------------------------
        call lfaecrr(kul3,cdadcou,zlibres2,kdom)
      elseif(cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '1') then
        !-------------------------------------------------
        ! Cas de variables dont le fichier DDH contient la valeur initiale et finale.
        ! Ce sont des variables dont on n'a effectué la moyenne par les DDH que sur continent.
        ! Exemples: SVTS1 (AROME), S01_1 (ARPEGE).
        !-------------------------------------------------
        call lfaecrr(kul3,cdadcou,zlibres2,kdom)
      elseif(cdadcou(len_trim(cdadcou):len_trim(cdadcou)) == '0') then
        !-------------------------------------------------
        ! Cas de variables dont le fichier DDH contient la valeur initiale et finale.
        ! Ce sont des variables dont on n'a effectué la moyenne par les DDH que sur continent.
        ! Exemples: SVTS0 (AROME), S01_0 (ARPEGE).
        !-------------------------------------------------
        clfinal=cdadcou(1:len_trim(cdadcou)-1)//'1'
        call lfalecr(kul1,clfinal,jpdom,zlibres1,ilong,irep)
        call lfaecrr(kul3,cdadcou,zlibres1,kdom)
      else
        write(*,fmt=*) 'ddht/WARNING : the variable name ',trim(cdadcou),' is not recognized. This article is ignored.' 
      endif
    elseif(cgconf(1:14) == 'SOMME_CONTIGUE') then
      call lfaecrr(kul3,cdadcou,zlibres2,kdom)
    else
      !
      ! Autres cas; ils sont non prevus.
      !
      print*,'ddht/somd_libres 1/ATTENTION: traitement des champs libres de l''option ', &
  &           cgconf(1:len_trim(cgconf)),' non encore implemente.'
      return
    endif
  endif
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_HORIZ') then
  call lfalecr(kul1,cdadcou,jpdom,zlibres1,ilong,irep)
  !
  ! Operation a effectuer sur le champ libre
  ! et reecriture d'icelui sur
  ! le fichier de sortie.
  !
  ! On va moyenner selon l'horizontale.
  !
  zmoy(1)=0.
  do jdom=1,ilong
    zmoy(1)=zmoy(1)+zlibres1(jdom)/float(ilong)
  enddo
  call lfaecrr(kul3,cdadcou,zmoy,1)
elseif(cgconf(1:len_trim(cgconf)) == 'MOY_VERTIC') then
  !
  !-------------------------------------------------
  ! Simple recopie d'un article d'un fichier à l'autre.
  !-------------------------------------------------
  !
  call lfacop(kul1,cdadcou,cdadcou,kul3)
elseif(trim(cgconf) == 'EXTRAIT_DOMAIN') then
  !
  ! Traitement des variables.
  !
  call lfalecr(kul1,cdadcou,jpdom,zlibres1,ilong,irep)
  !
  ! Operation a effectuer sur le champ libre:
  ! extraire certains domaines,
  ! puis reecriture d'icelui sur le fichier de sortie.
  !
  do jindoma=1,kndoma
    zlibres3(jindoma)=zlibres1(kdoma(jindoma))
  enddo
  call lfaecrr(kul3,cdadcou,zlibres3,kndoma)
else
  !
  ! Autres cas; ils sont non prevus.
  !
  print* &
&     ,'ddht/somd_libres 2/ATTENTION: traitement des champs libres de l''option ', &
&     cgconf(1:len_trim(cgconf)),' non encore implemente.'
  return
endif
if(lgdebu) print*,'fin somd_libres'
end
subroutine interpolddh(cdadcou,pech1,pech2,pchamp_uni_1,klev1,klev2,kdom1,kdom2,kuni1,kuni &
&,ppre_uni_ini_1,ppre_uni_ini_2 &
&,ppre_uni_fin_1,ppre_uni_fin_2 &
&,ppre_uni_cum_1,ppre_uni_cum_2 &
&,pchamp_uni_3)
! --------------------------------------------------------------------------
! **** *interpolddh* Interpolation verticale de variables et flux.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 2006-08-17, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
! En entree:
! pchamp_uni_1: profil 1.
! klev1,klev2: nb de niveaux des 2 fichiers d'entrée.
! kdom1,kdom2: nb de domaines des 2 fichiers d'entrée.
! kuni1: dimension physique réelle des tableaux de réels écrits sur le fichier d'entrée 1.
! kuni:         dimension maximale des tableaux de réels écrits sur le fichier de sortie 3 (parameter).
! ppre_uni_ini_1,ppre_uni_ini_2: profils de VPP0 des fichiers d'entrée 1 et 2.
! ppre_uni_fin_1,ppre_uni_fin_2: profils de VPP1 des fichiers d'entrée 1 et 2.
! ppre_uni_cum_1,ppre_uni_cum_2: profils de PPP  des fichiers d'entrée 1 et 2.
!
! En sortie:
! pchamp_uni_3: profil 3, résultat de l'interpolation du profil 1 sur la grille 2.
!
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Variables arguments.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
CHARACTER*(*), intent(in) :: CDADCOU
REAL(KIND=8), INTENT(in) :: pchamp_uni_1(kuni)
INTEGER(KIND=4), intent(in) :: klev1,klev2,kdom1,kdom2,kuni1,kuni
REAL(KIND=8), INTENT(in), dimension(kuni) :: ppre_uni_ini_1,ppre_uni_ini_2 &
&,ppre_uni_fin_1,ppre_uni_fin_2 &
&,ppre_uni_cum_1,ppre_uni_cum_2
REAL(KIND=8), INTENT(out) :: pchamp_uni_3(kuni)
!
!-------------------------------------------------
! Variables locales.
!-------------------------------------------------
!
INTEGER(KIND=4) :: juni,jdom,jlev,ilev1,ilev2,iuni1,iuni2,iuni
REAL(KIND=8), dimension(kdom1,klev1) :: zchamp_dn_1,zpre_dn_ini_1,zpre_dn_fin_1,zpre_dn_cum_1
REAL(KIND=8), dimension(kdom1,klev1) :: zcoo_ini_1,zcoo_fin_1,zcoo_cum_1
REAL(KIND=8), dimension(kdom2,klev2) :: zchamp_dn_2,zpre_dn_ini_2,zpre_dn_fin_2,zpre_dn_cum_2
REAL(KIND=8), dimension(kdom2,klev2) :: zcoo_ini_2,zcoo_fin_2,zcoo_cum_2
REAL(KIND=8), dimension(kdom2,klev2) :: zsigma_ini,zsigma_fin,zsigma_cum,zgripc_ini,zgripc_fin,zgripc_cum
REAL(KIND=8), dimension(klev1) :: zchamp_g1,zcoop_g1
REAL(KIND=8), dimension(klev2) :: zchamp_g2,zcoop_g2
logical llflux
if(lgdebu) print*,'début interpolddh'
if(trim(cdadcou) == 'VPP0') then
  !
  !-------------------------------------------------
  ! Champ pression initiale.
  ! Pas d'interpolation: on recopie celui du fichier 2,
  ! puisque la grille verticale visée est celle du fichier 2.
  !-------------------------------------------------
  !
  do juni=1,klev2*kdom2
    pchamp_uni_3(juni)=ppre_uni_ini_2(juni)
  enddo
elseif(trim(cdadcou) == 'VPP1') then
  !
  !-------------------------------------------------
  ! Champ pression finale.
  ! Pas d'interpolation: on recopie celui du fichier 2,
  ! puisque la grille verticale visée est celle du fichier 2.
  !-------------------------------------------------
  !
  do juni=1,klev2*kdom2
    pchamp_uni_3(juni)=ppre_uni_fin_2(juni)
  enddo
elseif(trim(cdadcou) == 'PPP') then
  !
  !-------------------------------------------------
  ! Champ pression cumulée temporellement.
  ! Pas d'interpolation: on recopie celui du fichier 2 (corrigée de l'échéance
  ! du fichier), puisque la grille verticale visée est celle du fichier 2.
  !-------------------------------------------------
  !
  do juni=1,klev2*kdom2
    pchamp_uni_3(juni)=ppre_uni_cum_2(juni)*pech1/pech2
  enddo
else
  !
  !-------------------------------------------------
  ! Champs autres que la pression.
  ! Interpolation verticale.
  !-------------------------------------------------
  !
  !-------------------------------------------------
  ! Conversion des tableaux 1D issus de la lecture sur fichier de DDH
  ! en tableaux 2D (domaine, niveau).
  !-------------------------------------------------
  !
  if(cdadcou(1:1) == 'F') then
    ilev1=klev1-1
    ilev2=klev2-1
    llflux=.true.
  else
    ilev1=klev1
    ilev2=klev2
    llflux=.false.
  endif
  iuni1=ilev1*kdom1
  iuni2=ilev2*kdom2
  call uni2dn(pchamp_uni_1,kuni1,kdom1,klev1,zchamp_dn_1)
  call uni2dn(ppre_uni_ini_1,iuni1,kdom1,ilev1,zpre_dn_ini_1)
  call uni2dn(ppre_uni_fin_1,iuni1,kdom1,ilev1,zpre_dn_fin_1)
  call uni2dn(ppre_uni_cum_1,iuni1,kdom1,ilev1,zpre_dn_cum_1)
  call uni2dn(ppre_uni_ini_2,iuni2,kdom2,ilev2,zpre_dn_ini_2)
  call uni2dn(ppre_uni_fin_2,iuni2,kdom2,ilev2,zpre_dn_fin_2)
  call uni2dn(ppre_uni_cum_2,iuni2,kdom2,ilev2,zpre_dn_cum_2)
  !
  !-------------------------------------------------
  ! Interpolation verticale proprement dite:
  ! 1. Calcul des (ilev2+1) niveaux sigma (sigma = p/ps) de la grille 2.
  ! 1.1. On remplace dans les tableaux zpre_dn* le contenu deltap
  ! en un contenu p.
  !-------------------------------------------------
  !
  zcoo_ini_1=zpre_dn_ini_1
  zcoo_fin_1=zpre_dn_fin_1
  zcoo_cum_1=zpre_dn_cum_1
  do jdom=1,kdom1
    do jlev=2,ilev1
      zcoo_ini_1(jdom,jlev)=zpre_dn_ini_1(jdom,jlev)+zcoo_ini_1(jdom,jlev-1)
      zcoo_fin_1(jdom,jlev)=zpre_dn_fin_1(jdom,jlev)+zcoo_fin_1(jdom,jlev-1)
      zcoo_cum_1(jdom,jlev)=zpre_dn_cum_1(jdom,jlev)+zcoo_cum_1(jdom,jlev-1)
      if(lgdebu) then
        if(jlev == 2) then
          print*,'25456',jdom,jlev,zpre_dn_cum_1(jdom,jlev),zcoo_cum_1(jdom,jlev-1), &
          &  zcoo_cum_1(jdom,jlev)
        endif
      endif
    enddo
  enddo
  zcoo_ini_2=0.
  zcoo_fin_2=0.
  zcoo_cum_2=0.
  do jdom=1,kdom2
    do jlev=2,ilev2
      zcoo_ini_2(jdom,jlev)=zpre_dn_ini_2(jdom,jlev)+zcoo_ini_2(jdom,jlev-1)
      zcoo_fin_2(jdom,jlev)=zpre_dn_fin_2(jdom,jlev)+zcoo_fin_2(jdom,jlev-1)
      zcoo_cum_2(jdom,jlev)=zpre_dn_cum_2(jdom,jlev)+zcoo_cum_2(jdom,jlev-1)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! A ce stade les tableaux zcoo* contiennent la pression
  ! de chaque niveau de flux courant. 
  ! 1.2. On en déduit sigma sur les niveaux de flux.
  !-------------------------------------------------
  !
  do jdom=1,kdom2
    do jlev=1,ilev2
      zsigma_ini(jdom,jlev)=zcoo_ini_2(jdom,jlev)/zcoo_ini_2(jdom,ilev2)
      zsigma_fin(jdom,jlev)=zcoo_fin_2(jdom,jlev)/zcoo_fin_2(jdom,ilev2)
      zsigma_cum(jdom,jlev)=zcoo_cum_2(jdom,jlev)/zcoo_cum_2(jdom,ilev2)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! 2. Soit ps1 la pression de surface du fichier 1. 
  ! On va appliquer les (ilev2+1) niveaux sigma de la grille 2
  ! à ps1, i.e. on détermine la grille-pression cible en niveaux de flux 
  ! comme p(j)=ps1*sigma(j), j=1,ilev2+1.
  !-------------------------------------------------
  !
  do jdom=1,kdom2
    do jlev=1,ilev2
      zgripc_ini(jdom,jlev)=zcoo_ini_1(jdom,ilev1)*zsigma_ini(jdom,jlev)
      zgripc_fin(jdom,jlev)=zcoo_fin_1(jdom,ilev1)*zsigma_fin(jdom,jlev)
      zgripc_cum(jdom,jlev)=zcoo_cum_1(jdom,ilev1)*zsigma_cum(jdom,jlev)
      if(lgdebu) print*,'14785:',jdom,jlev,zcoo_cum_1(jdom,ilev1),zsigma_cum(jdom,jlev)
    enddo
  enddo
  iuni=0
  do jdom=1,kdom1
    !
    !-------------------------------------------------
    ! On va générer les tableaux attendus par la routine d'interpolation:
    ! - zchamp_g1: champ sur la grille 1.
    ! - zcoop_g1: coordonnée pression de la grille 1.
    ! - zcoop_g2: coordonnée pression de la grille 2.
    !-------------------------------------------------
    !
    if(lgdebu) print*,'llflux=',llflux
    if(llflux) then
      !
      !-------------------------------------------------
      ! Cas des flux. On veut une coordonnée-pression sur les niveaux de flux.
      !-------------------------------------------------
      !
      do jlev=1,klev1
        if(jlev == 1) then
          zcoop_g1(jlev)=0.
        else
          zcoop_g1(jlev)=zcoo_cum_1(jdom,jlev-1)
        endif
        if(lgdebu) print*,'zcoop_g1(',jlev,')=',zcoop_g1(jlev)
        zchamp_g1(jlev)=zchamp_dn_1(jdom,jlev)
      enddo
      do jlev=1,klev2
        if(jlev == 1) then
          zcoop_g2(jlev)=0.
        else
          zcoop_g2(jlev)=zgripc_cum(jdom,jlev-1)
        endif
      enddo
    else
      !
      !-------------------------------------------------
      ! Cas des variables. On veut une coordonnée-pression sur les niveaux de variables.
      !-------------------------------------------------
      !
      do jlev=1,klev1
        if(jlev == 1) then
          if(cdadcou(4:4) == '0') then
            zcoop_g1(jlev)=0.5*zcoo_ini_1(jdom,1)
          elseif(cdadcou(4:4) == '1') then
            zcoop_g1(jlev)=0.5*zcoo_fin_1(jdom,1)
          else
            zcoop_g1(jlev)=0.5*zcoo_cum_1(jdom,1)
          endif
        else
          if(cdadcou(4:4) == '0') then
            zcoop_g1(jlev)=0.5*(zcoo_ini_1(jdom,jlev)+zcoo_ini_1(jdom,jlev-1))
          elseif(cdadcou(4:4) == '1') then
            zcoop_g1(jlev)=0.5*(zcoo_fin_1(jdom,jlev)+zcoo_fin_1(jdom,jlev-1))
          else
            zcoop_g1(jlev)=0.5*(zcoo_cum_1(jdom,jlev)+zcoo_cum_1(jdom,jlev-1))
          endif
        endif
        if(cdadcou(4:4) == '0') then
          zchamp_g1(jlev)=zchamp_dn_1(jdom,jlev)/zpre_dn_ini_1(jdom,jlev)
        elseif(cdadcou(4:4) == '1') then
          zchamp_g1(jlev)=zchamp_dn_1(jdom,jlev)/zpre_dn_fin_1(jdom,jlev)
        else
          zchamp_g1(jlev)=zchamp_dn_1(jdom,jlev)/zpre_dn_cum_1(jdom,jlev)
        endif
      enddo
      do jlev=1,klev2
        if(jlev == 1) then
          if(cdadcou(4:4) == '0') then
            zcoop_g2(jlev)=0.5*zgripc_ini(jdom,1)
          elseif(cdadcou(4:4) == '1') then
            zcoop_g2(jlev)=0.5*zgripc_fin(jdom,1)
          else
            zcoop_g2(jlev)=0.5*zgripc_cum(jdom,1)
          endif
        else
          if(cdadcou(4:4) == '0') then
            zcoop_g2(jlev)=0.5*(zgripc_ini(jdom,jlev)+zgripc_ini(jdom,jlev-1))
          elseif(cdadcou(4:4) == '1') then
            zcoop_g2(jlev)=0.5*(zgripc_fin(jdom,jlev)+zgripc_fin(jdom,jlev-1))
          else
            zcoop_g2(jlev)=0.5*(zgripc_cum(jdom,jlev)+zgripc_cum(jdom,jlev-1))
          endif
        endif
      enddo
    endif
    !
    !-------------------------------------------------
    ! 3. Interpolation du champ courant pchamp_uni_1 sur la grille-pression cible.
    !-------------------------------------------------
    !
    if(lgdebu) print*,'interpolddh: pré call interpol1d ',klev1,klev2,trim(cdadcou)
    if(lgdebu) then
      do jlev=1,klev1
        write(*,'(i4,2g16.7)') jlev,zcoop_g1(jlev)
      enddo
    endif
    call interpol1d(klev1,zcoop_g1,zchamp_g1,klev2,zcoop_g2,zchamp_g2)
    !
    !-------------------------------------------------
    ! 4. Copie du résultat interpolé du domaine courant
    ! sur le tableau recevant tous les domaines.
    !-------------------------------------------------
    !
    do jlev=1,klev2
      iuni=iuni+1
      if(llflux) then
        pchamp_uni_3(iuni)=zchamp_g2(jlev)
      else
        if(cdadcou(4:4) == '0') then
          pchamp_uni_3(iuni)=zchamp_g2(jlev)*zpre_dn_ini_2(jdom,jlev)
        elseif(cdadcou(4:4) == '1') then
          pchamp_uni_3(iuni)=zchamp_g2(jlev)*zpre_dn_fin_2(jdom,jlev)
        else
          pchamp_uni_3(iuni)=zchamp_g2(jlev)*zpre_dn_cum_2(jdom,jlev)
        endif
      endif
    enddo
  enddo
endif
if(lgdebu) print*,'fin interpolddh'
end
subroutine uni2dn(pchamp_uni,kuni,kdom,klev,pchamp_dn)
! --------------------------------------------------------------------------
! **** *uni2dn* Conversion d'un tableau 1D contenant toutes les valeurs (dom,niv) à un tableau 2D (dom,niv).
! --------------------------------------------------------------------------
! Sujet:
! ------
! Cette routine convient aussi bien aux tableaux de variables (VPP0, VCT0, etc) que de flux (FCTRAYTER1, etc).
! Les arguments kuni et klev sont alors différents (klev est + grand de 1 et kuni est + grand de kdom, si flux).
!
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
! 2006-08-17, J.M. Piriou.
!
! Modifications:
! --------------
! --------------------------------------------------------------------------
! En entree:
! pchamp_uni: tableau 1D contenant toutes les valeurs (dom,niv).
! kuni: produit nb domaines * nb niveaux.
! kdom: nombre de domaines.
! klev: nombre de niveaux.
! En sortie:
! pchamp_dn: tableau 2D (dom,niv).
!
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Variables arguments.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
REAL(KIND=8), INTENT(in) :: pchamp_uni(kuni)
INTEGER(KIND=4), intent(in) :: kuni,kdom,klev
REAL(KIND=8), INTENT(out) :: pchamp_dn(kdom,klev)
!
!-------------------------------------------------
! Variables locales.
!-------------------------------------------------
!
INTEGER(KIND=4) :: jdom,jlev,i_uni
if(lgdebu) print*,'début uni2dn'
!
!-------------------------------------------------
! Copie du tableau 1D sur un tableau 2D.
!-------------------------------------------------
!
do jdom=1,kdom
  do jlev=1,klev
    i_uni=(jdom-1)*klev+jlev
    pchamp_dn(jdom,jlev)=pchamp_uni(i_uni)
  enddo
enddo
if(lgdebu) print*,'fin uni2dn'
end
subroutine interpol1d(klev1,pcoo1,pval1,klev2,pcoo2,pval2)
! --------------------------------------------------------------
! **** ** Interpolation d'un tableau 1D sur un autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  klev1: nb de niveaux en entrée.
!  pcoo1: coordonnée irrégulière du tableau 1 d'entrée.
!  pval1: valeurs du tableau 1 d'entrée.
!  klev2: nb de niveaux en sortie.
!  pcoo2: coordonnée irrégulière du tableau 2 de sortie.
! En sortie:
!  pval2: valeurs du tableau 2 de sortie.
! --------------------------------------------------------------
! Les coordonnées peuvent être croissantes ou décroissantes.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p) 
#include"ddhpar.h"
#include"ddht_yom_ent.h"
REAL(KIND=8), INTENT(in) :: pcoo1(klev1),pval1(klev1),pcoo2(klev2)
INTEGER(KIND=4), intent(in) :: klev1,klev2
REAL(KIND=8), INTENT(out) :: pval2(klev2)
!
!-------------------------------------------------
! Variables locales.
!-------------------------------------------------
!
REAL(KIND=8) :: zcoo1(klev1),zval1(klev1),zcoo2(klev2),zcoo
INTEGER(KIND=4) :: jlev,jlev1
logical llok
if(lgdebu) print*,'début interpol1d'
if(lgdebu) then
  print*,'tableaux en entrée:'
  do jlev=1,klev1
    write(*,'(i4,2g16.7)') jlev,pcoo1(jlev),pval1(jlev)
  enddo
endif
!
!-------------------------------------------------
! Test de croissance ou décroissance de la coordonnée.
!-------------------------------------------------
!
if(pcoo1(1) > pcoo1(klev1)) then
  !
  !-------------------------------------------------
  ! On permute tout.
  !-------------------------------------------------
  !
  do jlev=1,klev1
    zcoo1(jlev)=pcoo1(klev1-jlev+1) 
    zval1(jlev)=pval1(klev1-jlev+1)
  enddo
else
  !
  !-------------------------------------------------
  ! Rien à permuter.
  !-------------------------------------------------
  !
  zcoo1=pcoo1
  zval1=pval1
endif
if(lgdebu) print*,'interpol1d: post permutation'
if(pcoo2(1) > pcoo2(klev2)) then
  !
  !-------------------------------------------------
  ! On permute tout.
  !-------------------------------------------------
  !
  do jlev=1,klev2
    zcoo2(jlev)=pcoo2(klev2-jlev+1) 
  enddo
else
  !
  !-------------------------------------------------
  ! Rien à permuter.
  !-------------------------------------------------
  !
  zcoo2=pcoo2
endif
!
!-------------------------------------------------
! Boucle sur les valeurs du tableau de sortie.
!-------------------------------------------------
!
if(lgdebu) print*,'interpol1d: pré boucle  valeurs du tableau de sortie'
if(lgdebu) print*,'interpol1d: klev2=',klev2
do jlev=1,klev2
  if(lgdebu) print*,'interpol1d: jlev=',jlev
  zcoo=zcoo2(jlev)
  if(zcoo <= zcoo1(1)) then
    if(lgdebu) print*,'interpol1d: cas zcoo <= zcoo1(1)'
    !
    !-------------------------------------------------
    ! Echantillonnage.
    !-------------------------------------------------
    !
    !pval2(jlev)=zval1(1)
    !
    !-------------------------------------------------
    ! Extrapolation.
    !-------------------------------------------------
    !
    if(lgdebu) print*,'interpol1d: ',zval1(1)
    if(lgdebu) print*,'interpol1d: ',zval1(2)
    if(lgdebu) print*,'interpol1d: ',zcoo1(1)
    if(lgdebu) print*,'interpol1d: ',zcoo1(2)
    if(lgdebu) print*,'interpol1d: ',zcoo
    if(lgdebu) then
      do jlevprint=1,klev1
        print*,'223:',jlevprint,zcoo1(jlevprint)
      enddo
    endif
    pval2(jlev)=zval1(1)+(zval1(1+1)-zval1(1))*(zcoo-zcoo1(1))/(zcoo1(1+1)-zcoo1(1))
  elseif(zcoo >= zcoo1(klev1)) then
    if(lgdebu) print*,'interpol1d: cas zcoo >= zcoo1(klev1)'
    !
    !-------------------------------------------------
    ! Echantillonnage.
    !-------------------------------------------------
    !
    !pval2(jlev)=zval1(klev1)
    !
    !-------------------------------------------------
    ! Extrapolation.
    !-------------------------------------------------
    !
    pval2(jlev)=zval1(klev1-1)+(zval1(klev1-1+1)-zval1(klev1-1))*(zcoo-zcoo1(klev1-1))/(zcoo1(klev1-1+1)-zcoo1(klev1-1))
  else
    if(lgdebu) print*,'interpol1d: cas else'
    llok=.false.
    do jlev1=1,klev1-1
      if(lgdebu) print*,'interpol1d: jlev1=',jlev1
      if(zcoo >= zcoo1(jlev1) .and. zcoo <= zcoo1(jlev1+1)) then
        llok=.true.
        pval2(jlev)=zval1(jlev1)+(zval1(jlev1+1)-zval1(jlev1))*(zcoo-zcoo1(jlev1))/(zcoo1(jlev1+1)-zcoo1(jlev1))
      endif
    enddo
    if(.not.llok) then
      write(*,fmt=*) 'interpol1d/ERROR: the interpolation of an array of size ',klev1,' on an array of size ',klev2
      write(*,fmt=*) '  no solution can be provided!...'
      write(*,fmt=*) '  likely an ERROR in the input coordinates.'
      call exit(1)
    endif
  endif
enddo
if(lgdebu) print*,'interpol1d: 2'
!
!-------------------------------------------------
! Test de croissance ou décroissance de la coordonnée.
!-------------------------------------------------
!
if(pcoo2(1) > pcoo2(klev2)) then
  !
  !-------------------------------------------------
  ! On permute tout.
  !-------------------------------------------------
  !
  do jlev=1,klev2/2
    call permute(pval2(jlev),pval2(klev2-jlev+1))
  enddo
endif
if(lgdebu) print*,'fin interpol1d'
end
subroutine permute(px1,px2)
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
REAL(KIND=8) :: px1,px2,zx
zx=px1
px1=px2
px2=zx
end
subroutine longa(cdcar,knlev,klev1)
! --------------------------------------------------------------------------
! **** *LONGA  * Type d'article DDH > nombre de niveaux correspondant.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Original :
! ----------
!  1992-12-07, J.M. Piriou.
!
! Modifications:
! --------------
!  2008-11-05, J.M. Piriou: correction de bug dans les poids d'interpolation verticale.
!
! --------------------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit integer(kind=4) (i,k)
implicit real(kind=8) (z,p)
#include"ddhpar.h"
#include"ddht_yom_ent.h"
character(len=*), intent(in) :: cdcar
INTEGER(KIND=4), intent(in) :: knlev
INTEGER(KIND=4), intent(out) :: klev1

! Determination de la longueur de champ
if(cdcar(1:1) == 'V') then
  ! Champ de type variable: N niveaux.
  klev1=knlev
elseif(cdcar(1:1) == 'T') then
  ! Champ de type tendance: N niveaux.
  klev1=knlev
elseif(cdcar(1:1) == 'F') then
  ! Champ de type flux    : N+1 niveaux.
  klev1=knlev+1
elseif(cdcar(1:1) == 'P') then
  ! Champ de type cumul de masse: N niveaux.
  klev1=knlev
elseif(cdcar(1:2) == 'SV') then
  ! Champ de type variable de surface.
  klev1=1
elseif(cdcar(1:2) == 'SF') then
  ! Champ de type flux en surface.
  klev1=1
else
  ! Attention: cas de champ non prevu.
  print*,'ddht/LONGA/ATTENTION:'
  print*,'cdcar=',trim(cdcar)
  print*,'Type de champ non prevu.'
  klev1=-1
endif
end
#include"lited.F90"
