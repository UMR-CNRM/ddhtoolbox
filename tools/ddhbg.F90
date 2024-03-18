program ddhbg
! --------------------------------------------------------------------------
! **** *ddhbg*  Bilan d'un fichier de DDH global.
! --------------------------------------------------------------------------
! Sujet:
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
! Auteur:         1997-02, J.M. Piriou.
! -------
! Modifications:
!                 2008-07-10, J.M. Piriou: use different budget packages.
!                 2008-07-18, J.M. Piriou: translate to English.
!                 2012-03-29, J.M. Piriou: adapt budgets to PCMT convection scheme.
! --------------------------------------------------------------------------
#include"implicit_r8i4.h"
#include"ddhpar.h"
CHARACTER *200 clmot(180)
CHARACTER *200 clloc
CHARACTER *200 clformat
CHARACTER*200 :: CLARG
CHARACTER*200 :: CLNAMX
CHARACTER*200 :: CLTYPE
CHARACTER*200 :: CLBP
CHARACTER*200 :: clprec(jparg)
CHARACTER*200 :: clprec_re(jparg)
CHARACTER*200 :: clprec_sm(jparg)
CHARACTER*200 :: clbilqvcond(jparg)
CHARACTER*200 :: clfqvtur(jparg)
CHARACTER*200 :: clfctrayso(jparg)
CHARACTER*200 :: clfctrayth(jparg)
CHARACTER*200 :: clfcttur(jparg)
CHARACTER*200 :: clfctlat(jparg)
CHARACTER*200 :: clforu
INTEGER(KIND=4) :: inarg,imot,jmot
INTEGER(KIND=4) :: idepart
INTEGER(KIND=4) :: IARG
INTEGER(KIND=4) :: IARGCP
INTEGER(KIND=4) :: IDOM
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: ILEV
INTEGER(KIND=4) :: ILFIC
INTEGER(KIND=4) :: ILNAMX
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: JARG
REAL(KIND=8) :: ZCLIM
REAL(KIND=8) :: ZCLIMGPCP
REAL(KIND=8) :: ZG
REAL(KIND=8) :: ZKK
REAL(KIND=8) :: ZNT
REAL(KIND=8) :: ZRESIDU
REAL(KIND=8) :: ZRR
REAL(KIND=8) :: ZSOR
REAL(KIND=8) :: zvct0(1)
REAL(KIND=8) :: zvct1(1),zsigne
REAL(KIND=8) :: zx(2),zy(2),zm(2,2),zdet
logical :: lldebu
logical :: llut

!
character*240 clfic(jparg)
INTEGER(KIND=4) iul(jparg)
INTEGER(KIND=4) itaille
!
INTEGER(KIND=4) idocfi(17)
INTEGER(KIND=4) idatef(11)
!
REAL(KIND=8) zval(jpprod)
REAL(KIND=8) zechflux(jparg)
REAL(KIND=8) zechvar(jparg)
!
! pp.
!
REAL(KIND=8) zpp0(jparg),zpp1(jparg),zitm(jparg)
REAL(KIND=8) zbilpptend(jparg),zbilppdynh(jparg),zbilppdynv(jparg)
REAL(KIND=8) zbilppprec(jparg)
!
! qv.
!
REAL(KIND=8) zbilqvtend(jparg),zbilqvcond(jparg),zbilqvevap(jparg)
REAL(KIND=8) zbilqvdynh(jparg),zbilqvdynv(jparg)
!
! ct.
!
REAL(KIND=8) zsolaire_des_surface(jparg)
REAL(KIND=8) zsolaire_mon_surface(jparg)
REAL(KIND=8) zsolaire_net_surface(jparg)
REAL(KIND=8) zsolaire_mon_sommet(jparg)
REAL(KIND=8) zsolaire_des_sommet(jparg)
REAL(KIND=8) zsolaire_net_sommet(jparg)
REAL(KIND=8) zbilcttend(jparg),zbilctraysol(jparg),zbilctrayther(jparg) &
& ,zbilctlat(jparg),zbilctsens(jparg)
REAL(KIND=8) zbilctdynh(jparg),zbilctdynv(jparg)
!
character clexp(jparg)*30
REAL(KIND=8) zsens(jparg), zlat(jparg), znetsolar_toa(jparg)
REAL(KIND=8) zsol_net_surface(jparg)
REAL(KIND=8) zir_net_surface(jparg)
REAL(KIND=8) zcond
REAL(KIND=8) ztransm_atm,zabsorp_atm,zreflex_atm,zreflex_planete
REAL(KIND=8) :: zreflex_surface
REAL(KIND=8) :: zp_fin(1)
REAL(KIND=8) :: zp_ini(1)
character(len=200) :: clfor_solc
!
lldebu=.false.
zg=9.80665
inarg=iargcp()
if(inarg == 0) then
  !
  !-------------------------------------------------
  ! Pas d'argument sur la ligne de commande.
  !-------------------------------------------------
  !
  print*,' '
  print*,'Budget of a global DDH file on standard output.'
  print*,' '
  print*,'Usage: ddhbg [-e] file1 [file2] ... [filen]'
  print*,'where '
  print*,'  -e activates output on file of every budget field. '
  print*,' '
  print*,'Examples: '
  print*,'  ddhbg DDH1.lfa.mh DDH2.lfa.mg'
  print*,'  ddhbg -e DDH1.lfa.mh DDH2.lfa.mg'
  print*,' '
  stop
endif
!
!-------------------------------------------------
! Format par défaut.
!-------------------------------------------------
!
clforu='f9.1'
llut=.false.
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
call getargp(1,clarg)
if(clarg(1:2) == '-e') then
  !
  !-------------------------------------------------
  ! L'utilisateur spécifie son format d'écriture des réels.
  !-------------------------------------------------
  !
  idepart=2
  llut=.true.
  llecr=.true.
else
  idepart=1
  llut=.false.
  llecr=.false.
endif
clformat='(2a,'//trim(clforu)//',a,1x,7'//trim(clforu)//')'
do jarg=idepart,inarg
  call getargp(jarg,clarg)
  if(llut) then
    iarg=jarg-1
  else
    iarg=jarg
  endif
  clfic(iarg)=clarg
  iul(iarg)=10+iarg
enddo
if(llut) then
  inarg=inarg-1
endif
do jarg=1,inarg
  !
  !-------------------------------------------------
  ! Ouverture du jarg-ième fichier de DDH.
  !-------------------------------------------------
  !
  call lfaouv(iul(jarg),clfic(jarg),'R')
  !
  !-------------------------------------------------
  ! Articles de documentation.
  !-------------------------------------------------
  !
  call lfaleci(iul(jarg),'DATE',11,idatef,ilong,ierr)
  call lfaleci(iul(jarg),'DOCFICHIER',17,idocfi,ilong,ierr)
  ilev=idocfi(6)
  idom=idocfi(15)
  !
  !-------------------------------------------------
  ! Date julienne.
  !-------------------------------------------------
  !
  zseconde=0.
  call AMQHMS_VERS_DJ(idatef(1),idatef(2),idatef(3),idatef(4),idatef(5),zseconde,zjul)
  !
  !-------------------------------------------------
  ! On contrôle si le fichier est bien global.
  !-------------------------------------------------
  !
  if(idom /= 1) then
    print*,'ddhbg/ERROR: number of domains > 1!...'
    print*,idom
    call exit(1)
  endif
  if(ilev /= 1) then
    print*,'ddhbg/ERROR: number of levels > 1!...'
    print*,ilev
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Type de budget package.
  !-------------------------------------------------
  !
  call get_budget_package(iul(jarg),clbp)
  if(trim(clbp) == "BP_GMAP_2008_07") then
    !
    !-------------------------------------------------
    ! Budget type ARPEGE oper.
    !-------------------------------------------------
    !
    clprec(jarg)="FQTPRECICOL FQTPRECICON FQTPRECISTL FQTPRECISTN"
    clprec_re(jarg)="FQTPRECISTL FQTPRECISTN"
    clprec_sm(jarg)="FQTPRECICOL FQTPRECICON"
    clbilqvcond(jarg)="FQTCONDESTL FQTCONDESTN FQVEVPL- FQVEVPN- FQTCONDECOL FQTCONDECON "
    clfqvtur(jarg)="FQVTUR"
    clfctrayso(jarg)="FCTRAYSOL1"
    clfctrayth(jarg)="FCTRAYTER1"
    clfcttur(jarg)="FCTTUR"
    clfctlat(jarg)="FCTPRECICOL FCTPRECICON FCTPRECISTL FCTPRECISTN FCTPRECCSCOL FCTPRECCSCON FCTPRECCSSTL FCTPRECCSSTN"
  elseif(trim(clbp) == "BP_IFS_2018") then
    !
    !-------------------------------------------------
    ! Budget type IFS oper de 2018.
    !-------------------------------------------------
    !
    clprec(jarg)="FQTPRECICOL FQTPRECICON FQTPRECISTL FQTPRECISTN"
    clprec_re(jarg)="FQTPRECISTL FQTPRECISTN"
    clprec_sm(jarg)="FQTPRECICOL FQTPRECICON"
    clbilqvcond(jarg)="FQTCONDESTL FQTCONDESTN FQTCONDECOL FQTCONDECON"
    clfqvtur(jarg)="FQVTUR"
    clfctrayso(jarg)="FCTRAYSOL1"
    clfctrayth(jarg)="FCTRAYTER1"
    clfcttur(jarg)="FCTTUR"
    clfctlat(jarg)="FCTPRECISTL FCTPRECISTN FCTPRECCSSTL FCTPRECCSSTN FCTPRECICOL FCTPRECICON FCTPRECCSCOL FCTPRECCSCON"
  elseif(trim(clbp) == "BP_oper2012_lflexdia") then
    !
    !-------------------------------------------------
    ! Budget type ARPEGE oper 2012, sous DDH flexibles.
    !-------------------------------------------------
    !
    clprec(jarg)="FQVPRECICOL FQVPRECICON FQVPRECISTL FQVPRECISTN"
    clprec_re(jarg)="FQTPRECISTL FQTPRECISTN"
    clprec_sm(jarg)="FQTPRECICOL FQTPRECICON"
    clbilqvcond(jarg)="FQTCONDESTL FQTCONDESTN FQVEVPL- FQVEVPN- FQTCONDECOL FQTCONDECON "
    clfqvtur(jarg)="FQVTUR"
    clfctrayso(jarg)="FCTRAYSOL1"
    clfctrayth(jarg)="FCTRAYTER1"
    clfcttur(jarg)="FCTTUR"
    clfctlat(jarg)="FCTPRECICOL FCTPRECICON FCTPRECISTL FCTPRECISTN FCTPRECSCOL FCTPRECSCON FCTPRECSSTL FCTPRECSSTN"
  elseif(trim(clbp) == "BP_oper2014_lflexdia") then
    !
    !-------------------------------------------------
    ! Budget type ARPEGE oper 2014, sous DDH flexibles.
    !-------------------------------------------------
    !
    clprec(jarg)="FQRPL FQSPN FQVCCQL FQVCCQN"
    clprec_re(jarg)="FQRPL FQSPN"
    clprec_sm(jarg)="FQVCCQL FQVCCQN"
    clbilqvcond(jarg)="FQVCCQL FQVCCQN FQVECL- FQVESL- FQVECN- FQVESN- "
    clfqvtur(jarg)="FQVTUR"
    clfctrayso(jarg)="FCTRSO"
    clfctrayth(jarg)="FCTRTH"
    clfcttur(jarg)="FCTDIFT"
    clfctlat(jarg)="FCTSENSPREC FCTCCQL FCTCSQL FCTESL FCTECL FCTCCQN FCTCSQN FCTESN FCTECN "
  elseif(trim(clbp) == "BP_ALARO_2008_07") then
    !
    !-------------------------------------------------
    ! Budget type ALARO.
    !-------------------------------------------------
    !
    clprec(jarg)="FQRPLC FQSPLC FQRPLS FQSPLS"
    clprec_re(jarg)="FQRPLS FQSPLS"
    clprec_sm(jarg)="FQRPLC FQSPLC"
    clbilqvcond(jarg)="FQTCONDESTL FQTCONDESTN FQVEVPL- FQVEVPN- FQTCONDECOL FQTCONDECON "
    clfqvtur(jarg)="FQVDIFT"
    clfctrayso(jarg)="FCTRSO"
    clfctrayth(jarg)="FCTRTH"
    clfcttur(jarg)="FCTDIFT"
    clfctlat(jarg)="FCTHPCL FCTHPCN FCTHPSL FCTHPSN FCTHSCL FCTHSCN FCTHSSL FCTHSSN"
  elseif(trim(clbp) == "BP_PCMT_2012_01") then
    !
    !-------------------------------------------------
    ! Budget type ARPEGE + PCMT.
    !-------------------------------------------------
    !
    clprec(jarg)="FQRPL FQSPN FCRSEDIM FCSSEDIM"
    clprec_re(jarg)="FQRPL FQSPN"
    clprec_sm(jarg)="FCRSEDIM FCSSEDIM"
    clbilqvcond(jarg)="FQVCSQL FQVCSQN FQVESL FQVESN FQVCCQL FQVCCQN FQVECL FQVECN "
    clfqvtur(jarg)="FQVTUR"
    clfctrayso(jarg)="FCTRSO"
    clfctrayth(jarg)="FCTRTH"
    clfcttur(jarg)="FCTDIFT"
    clfctlat(jarg)="FCTCSQL FCTCSQN FCTESL FCTESN FCTCCQL FCTCCQN FCTECL FCTECN FCTIMCC FCTDIFC FCTSENSPREC FCTDPSFI"
  else
    write(*,fmt=*) 
    write(*,fmt=*) 'ddhbg/ERROR: unknown budget package: ',trim(clbp),'!...'
    write(*,fmt=*) 
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Nom des expériences.
  !-------------------------------------------------
  !
  call lfalecc(iul(jarg),'INDICE EXPERIENCE',1,clnamx,ilong,ierr)
  ilnamx=len_trim(clnamx)
  clexp(jarg)=clnamx
  !
  !-------------------------------------------------
  ! Echéance.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'ECHEANCEDV',cltype,ilong,ierr)
  if(ierr == 0) then
    !
    ! L'article ECHEANCEDV existe.
    ! C'est lui qui est à utiliser pour les tendances
    ! de variables.
    !
    call lfalecr(iul(jarg),'ECHEANCEDV',jpprod,zval,ilong,ierr)
    zechvar(jarg)=zval(1)
    call lfalecr(iul(jarg),'ECHEANCE',jpprod,zval,ilong,ierr)
    zechflux(jarg)=zval(1)
  else
    !
    ! L'article ECHEANCEresol_syst.F90DV n'existe pas.
    ! On utilise la même durée pour variables et flux.
    !
    call lfalecr(iul(jarg),'ECHEANCE',jpprod,zval,ilong,ierr)
    zechvar(jarg)=zval(1)
    zechflux(jarg)=zval(1)
  endif
  !
  !-------------------------------------------------
  ! Impression de l'autodocumentation du fichier DDH.
  !-------------------------------------------------
  !
  write(*,'(2a)') ' '
  ilfic=len_trim(clfic(jarg))
  print*,'File ',clfic(jarg)(1:ilfic)
  call pridocf(idocfi,idatef,clnamx,zechflux(jarg),zechvar(jarg),6)
  write(*,fmt=*) 'Budget package = ',trim(clbp)
enddo
!
!-------------------------------------------------
! On crée une ligne renseignant les indices expériences
! des différents fichiers de DDH.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  '                                CLIM'
do jarg=1,inarg
  call lfalecc(iul(jarg),'INDICE EXPERIENCE',1,clnamx,ilong,ierr)
  ilnamx=len_trim(clnamx)
  write(*,'(2a,$)') '   ',clnamx(1:ilnamx)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Precipitation RE      mm/day'
write(*,'(a7,$)') '   //  '
!
! Boucle sur les jarg fichiers de DDH.
!
do jarg=1,inarg
  call casc(clprec_re(jarg),1,clmot,imot)
  !
  ! Boucle sur les imot articles de précipitation.
  !
  zsor=0.
  do jmot=1,imot
    call lfalecr(iul(jarg),clmot(jmot),jpprod,zval,ilong,ierr)
    zsor=zsor+zval(2)/zechflux(jarg)*86400.
  enddo
  write(*,'(a,f6.2,$)') ' ',zsor
  clficsor='Precipitations_RE' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Precipitation SM      mm/day'
write(*,'(a7,$)') '   //  '
!
! Boucle sur les jarg fichiers de DDH.
!
do jarg=1,inarg
  call casc(clprec_sm(jarg),1,clmot,imot)
  !
  ! Boucle sur les imot articles de précipitation.
  !
  zsor=0.
  do jmot=1,imot
    call lfalecr(iul(jarg),clmot(jmot),jpprod,zval,ilong,ierr)
    zsor=zsor+zval(2)/zechflux(jarg)*86400.
  enddo
  write(*,'(a,f6.2,$)') ' ',zsor
  clficsor='Precipitations_SM' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Precipitation TO      mm/day'
zclimgpcp=2.8
zclim=zclimgpcp ! clim GPCP tirée de l'article du BAMS vol 78, 1997.
write(*,'(a,f6.2,$)') ' ',zclim
!
! Boucle sur les jarg fichiers de DDH.
!
do jarg=1,inarg
  call casc(clprec(jarg),1,clmot,imot)
  !
  ! Boucle sur les imot articles de précipitation.
  !
  zsor=0.
  do jmot=1,imot
    call lfalecr(iul(jarg),clmot(jmot),jpprod,zval,ilong,ierr)
    zsor=zsor+zval(2)/zechflux(jarg)*86400.
  enddo
  write(*,'(a,f6.2,$)') ' ',zsor
  clficsor='Precipitations_TO' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
do jarg=1,inarg
  !
  !-------------------------------------------------
  ! QV dynamique horizontale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'TQVDIVFLUHOR',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'TQVDIVFLUHOR',jpprod,zval,ilong,ierr)
    zbilqvdynh(jarg)=zval(1)/zechflux(jarg)*86400.
  else
    zbilqvdynh(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! QV dynamique verticale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'FQVFLUVERTDYN',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'FQVFLUVERTDYN',jpprod,zval,ilong,ierr)
    zbilqvdynv(jarg)=(zval(2)-zval(1))/zechflux(jarg)*86400.
  else
    zbilqvdynv(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! CT dynamique horizontale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'TCTDIVFLUHOR',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'TCTDIVFLUHOR',jpprod,zval,ilong,ierr)
    zbilctdynh(jarg)=zval(1)/zechflux(jarg)
  else
    zbilctdynh(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! CT dynamique verticale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'FCTFLUVERTDYN',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'FCTFLUVERTDYN',jpprod,zval,ilong,ierr)
    zbilctdynv(jarg)=(zval(2)-zval(1))/zechflux(jarg)
  else
    zbilctdynv(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! PP dynamique horizontale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'TPPDIVFLUHOR',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'TPPDIVFLUHOR',jpprod,zval,ilong,ierr)
    zbilppdynh(jarg)=zval(1)/zechflux(jarg)*86400.
  else
    zbilppdynh(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! PP dynamique verticale.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'FPPFLUVERTDYN',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'FPPFLUVERTDYN',jpprod,zval,ilong,ierr)
    zbilppdynv(jarg)=(zval(2)-zval(1))/zechflux(jarg)*86400.
  else
    zbilppdynv(jarg)=0.
  endif
  !
  !-------------------------------------------------
  ! PP.
  !-------------------------------------------------
  !
  call lfacas(iul(jarg),'FPPSUMFPL',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'FPPSUMFPL    ',jpprod,zval,ilong,ierr)
    zbilppprec(jarg)=(zval(2)-zval(1))/zechflux(jarg)*86400.
  else
    zbilppprec(jarg)=0.
  endif
enddo
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Evaporation           mm/day'
zclim=-zclimgpcp ! clim GPCP tirée de l'article du BAMS vol 78, 1997.
write(*,'(a,f6.2,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfqvtur(jarg),jpprod,zval,ilong,ierr)
  zsor=zval(2)/zechflux(jarg)*86400.
  zbilqvevap(jarg)=-zsor
  write(*,'(a,f6.2,$)') ' ',zsor
  clficsor='Evaporation' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Net solar TOA           W*m-2'
zclim=240.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayso(jarg),jpprod,zval,ilong,ierr)
  znetsolar_toa(jarg)=zval(1)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',znetsolar_toa(jarg)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Net IR TOA              W*m-2'
zclim=-239.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayth(jarg),jpprod,zval,ilong,ierr)
  zsor=zval(1)/zechflux(jarg)
  zbilctrayther(jarg)=(zval(1)-zval(2))/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Net_IR_TOA' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Rad. budget TOA         W*m-2'
zclim=1.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayth(jarg),jpprod,zval,ilong,ierr)
  zsor=zval(1)/zechflux(jarg)
  call lfalecr(iul(jarg),clfctrayso(jarg),jpprod,zval,ilong,ierr)
  zsor=zsor+zval(1)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Rad._budget_TOA' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Rad. budget surface     W*m-2'
zclim=106.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayth(jarg),jpprod,zval,ilong,ierr)
  zsor=zval(2)/zechflux(jarg)
  call lfalecr(iul(jarg),clfctrayso(jarg),jpprod,zval,ilong,ierr)
  zsor=zsor+zval(2)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Rad._budget_surface' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Rad. budget atm.        W*m-2'
zclim=-105.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayth(jarg),jpprod,zval,ilong,ierr)
  zsor=(zval(1)-zval(2))/zechflux(jarg)
  call lfalecr(iul(jarg),clfctrayso(jarg),jpprod,zval,ilong,ierr)
  zsor=zsor+(zval(1)-zval(2))/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Rad._budget_atm' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Solar desc. surface     W*m-2'
zclim=185.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfacas(iul(jarg),'SFGFS01',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'SFGFS01',1,zval,ilong,ierr)
  else
    zval=999.999
  endif
  zsor=zval(1)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Solar_desc._surface' ; if(llecr) call ecrit(zjul,zsor,clficsor)
  zsolaire_des_surface(jarg)=zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'IR    desc. surface     W*m-2'
zclim=342.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfacas(iul(jarg),'SFGFS02',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'SFGFS02',1,zval,ilong,ierr)
  else
    zval=999.999
  endif
  zsor=zval(1)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='IR_desc._surface' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Sol. net surface        W*m-2'
zclim=161.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayso(jarg),jpprod,zval,ilong,ierr)
  zsol_net_surface(jarg)=zval(2)/zechflux(jarg)
  zbilctraysol(jarg)=(zval(1)-zval(2))/zechflux(jarg)
  zsolaire_net_sommet(jarg)=zval(1)/zechflux(jarg)
  zsolaire_net_surface(jarg)=zval(2)/zechflux(jarg)
  zsolaire_mon_surface(jarg)=zsolaire_des_surface(jarg)-zsolaire_net_surface(jarg)
  zsolaire_des_sommet(jarg)=340.
  zsolaire_mon_sommet(jarg)=zsolaire_des_sommet(jarg)-zsolaire_net_sommet(jarg)
  write(*,'(a,f6.1,$)') ' ',zsol_net_surface(jarg)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'IR.  net surface        W*m-2'
zclim=-55.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfctrayth(jarg),jpprod,zval,ilong,ierr)
  zir_net_surface(jarg)=zval(2)/zechflux(jarg)
  write(*,'(a,f6.1,$)') ' ',zir_net_surface(jarg)
enddo
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Sensible                W*m-2'
zclim=-20.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),clfcttur(jarg),jpprod,zval,ilong,ierr)
  zsor=(zval(2)-zval(1))/zechflux(jarg)
  zsens(jarg)=zsor
  zbilctsens(jarg)=-zsor
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Sensible' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Latent                  W*m-2'
zclim=-85.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call casc(clfctlat(jarg),1,clmot,imot)
  !
  ! Boucle sur les imot articles de précipitation.
  !
  zsor=0.
  do jmot=1,imot
    call lfalecr(iul(jarg),clmot(jmot),jpprod,zval,ilong,ierr)
    zsor=zsor+(zval(2)-zval(1))/zechflux(jarg)
  enddo
  zlat(jarg)=zsor
  zbilctlat(jarg)=-zsor
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Latent' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Sens + lat              W*m-2'
zclim=-105.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  zsor=zsens(jarg)+zlat(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Sens+latent' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(2a)') ' '
write(*,'(a,$)')  'Net budget at surface   W*m-2'
zclim=0.6
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  zsor=zsol_net_surface(jarg)-zbilctsens(jarg)-zbilctlat(jarg)+zir_net_surface(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
  clficsor='Net_budget_surface' ; if(llecr) call ecrit(zjul,zsor,clficsor)
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
write(*,'(a,$)')  '------------------------------------'
do jarg=1,inarg
  write(*,'(a,$)') '-------'
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'cp*T         ini     1E8*J/m2'
zcoef=1.e8
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VCT0      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)/zcoef
  write(*,'(a,f6.3,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'cp*T         fin     1E8*J/m2'
zcoef=1.e8
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VCT1      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)/zcoef
  write(*,'(a,f6.3,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Water vapour ini       kg*m-2'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VQV0      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)
  zbilqvtend(jarg)=zsor
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Water vapour final     kg*m-2'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VQV1      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)
  zbilqvtend(jarg)=(zsor-zbilqvtend(jarg))/zechvar(jarg)*86400.
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Wat liq+iceini kg*m-2 ech 100'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VQL0      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)
  call lfacas(iul(jarg),'VQN0',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'VQN0      ',jpprod,zval,ilong,ierr)
  else
    call lfalecr(iul(jarg),'VQI0      ',jpprod,zval,ilong,ierr)
  endif
  zsor=zsor+zval(1)
  write(*,'(a,f6.1,$)') ' ',100.*zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Wat liq+icefin kg*m-2 ech 100'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VQL1      ',jpprod,zval,ilong,ierr)
  zsor=zval(1)
  call lfacas(iul(jarg),'VQN1',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'VQN1      ',jpprod,zval,ilong,ierr)
  else
    call lfalecr(iul(jarg),'VQI1      ',jpprod,zval,ilong,ierr)
  endif
  zsor=zsor+zval(1)
  write(*,'(a,f6.1,$)') ' ',100.*zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Cloudiness    ini           %'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VNT0      ',jpprod,zval,ilong,ierr)
  znt=zval(1)
  call lfacas(iul(jarg),'VCP0',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'VCP0      ',jpprod,zval,ilong,ierr)
  else
    call lfalecr(iul(jarg),'VPP0      ',jpprod,zval,ilong,ierr)
  endif
  zpp0(jarg)=zval(1)
  zsor=100.*znt/zpp0(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Cloudiness    fin           %'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VNT1      ',jpprod,zval,ilong,ierr)
  znt=zval(1)
  call lfacas(iul(jarg),'VCP1',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(iul(jarg),'VCP1      ',jpprod,zval,ilong,ierr)
  else
    call lfalecr(iul(jarg),'VPP1      ',jpprod,zval,ilong,ierr)
  endif
  zpp1(jarg)=zval(1)
  zsor=100.*znt/zpp1(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Kinetic ener. ini        J/kg'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VKK0      ',jpprod,zval,ilong,ierr)
  zkk=zval(1)
  zsor=zkk/zpp0(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! Nouveau champ.
!-------------------------------------------------
!
write(*,'(a,$)')  'Kinetic ener. fin        J/kg'
zclim=0.
write(*,'(a,f6.1,$)') ' ',zclim
do jarg=1,inarg
  call lfalecr(iul(jarg),'VKK1      ',jpprod,zval,ilong,ierr)
  zkk=zval(1)
  zsor=zkk/zpp1(jarg)
  write(*,'(a,f6.1,$)') ' ',zsor
enddo
write(*,'(2a)') ' '
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! Bilan de masse d'air sec.
!-------------------------------------------------
!
! Légendes.
!
write(*,'(9a)') 'Dry air mass budget      (Pa/day)'
write(*,'(9a)')  '           tend   =    dynh  +  dynv  +  prec +   residu'
write(*,fmt=1300) 'CLIM     ',' ',0.,' =',0.,0.,0.,0.
1300  format(2a,f7.2,a,4(f8.2,1x))
do jarg=1,inarg
  !
  ! Calcul de la tendance.
  !
  call lfalecr(iul(jarg),'VPP1      ',jpprod,zp_fin,ilong,ierr)
  call lfalecr(iul(jarg),'VPP0      ',jpprod,zp_ini,ilong,ierr)
  zbilpptend(jarg)=(zp_fin(1)-zp_ini(1))/zechvar(jarg)*86400.
  !
  ! Estimation du résidu.
  !
  zresidu=zbilpptend(jarg)-zbilppdynh(jarg)-zbilppdynv(jarg)-zbilppprec(jarg)
  !
  ! Affichage.
  !
  write(*,fmt=1300) clexp(jarg)(1:9),' ',zbilpptend(jarg)*zg,' =',zbilppdynh(jarg)*zg,zbilppdynv(jarg)*zg,zbilppprec(jarg)*zg,zresidu*zg
enddo
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! Bilan de vapeur d'eau.
!-------------------------------------------------
!
! Légendes.
!
write(*,'(9a)') 'Water vapour budget    (qv flux in    mm/day)'
write(*,'(9a)')  '          tend   =     cond + ev.surf + dynh + dynv  + residu'
write(*,fmt=1100) 'CLIM     ',' ',0.,' =  ',-zclimgpcp,zclimgpcp,0.,0.,0.
1100  format(2a,f6.2,a,5(f7.2,1x))
do jarg=1,inarg
  call casc(clbilqvcond(jarg),1,clmot,imot)
  !
  ! Boucle sur les imot articles du bilan de qv.
  !
  zsor=0.
  do jmot=1,imot
    clloc=clmot(jmot)
    if(clloc(len_trim(clloc):len_trim(clloc)) == '-') then
      !
      !-------------------------------------------------
      ! Le dernier caractère du nom d'article est un "-".
      ! Cela veut dire que ce champ doit être soustrait, et non additionné, aux précédents.
      !-------------------------------------------------
      !
      clloc=clloc(1:len_trim(clloc)-1)
      zsigne=-1.
    else
      zsigne=1.
    endif
    call lfalecr(iul(jarg),clloc,jpprod,zval,ilong,ierr)
    zcond=zsigne*zval(2)/zechflux(jarg)*86400.
    zsor=zsor+zcond
    if(lldebu) then
      write(*,fmt=*) 'ddhbg: debug mot n° ',jmot,' ',trim(clloc),' zcond=',zcond,' zsor=',zsor
    endif
  enddo
  zbilqvcond(jarg)=-zsor
  !
  ! Estimation du résidu.
  !
  zresidu=zbilqvtend(jarg)-zbilqvcond(jarg)-zbilqvevap(jarg)-zbilqvdynh(jarg)-zbilqvdynv(jarg)
  !
  ! Affichage.
  !
  write(*,fmt=1100) clexp(jarg)(1:9),' ',zbilqvtend(jarg),' =  ',zbilqvcond(jarg),zbilqvevap(jarg),&
        &zbilqvdynh(jarg),zbilqvdynv(jarg),zresidu
enddo
!
!-------------------------------------------------
! On crée une ligne de séparation dont la longueur est proportionnelle au nombre de fichiers de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! Bilan de chaleur.
!-------------------------------------------------
!
! Légendes.
!
write(*,'(9a)') 'Temperature budget   (flux in W/m2)'
write(*,'(9a)')  '             tend   =      raysol +rayther + latent + sensible +dynh +  dynv   +  residu'
! Clim "Earth's global energy budget" Trenberth, Fasullo, Kiehl, BAMS 2009; rq : la somme fait bien -1 W/m2 sur l'article ! Les continents et la mer absorbent 1 W/m2.
write(*,fmt=clformat) 'CLIM     ',' ',0.,' =',79.,-184.,85.,20.,0.,0.,0.
do jarg=1,inarg
  !
  ! Calcul de la tendance en W/m2.
  !
  call lfalecr(iul(jarg),'VCT1',jpprod,zvct1,ilong,ierr)
  call lfalecr(iul(jarg),'VCT0',jpprod,zvct0,ilong,ierr)
  zbilcttend(jarg)=(zvct1(1)-zvct0(1))/zechvar(jarg)
  !
  ! Estimation du résidu.
  !
  zresidu=zbilcttend(jarg)-zbilctraysol(jarg)-zbilctrayther(jarg)-zbilctlat(jarg)-zbilctsens(jarg)-zbilctdynh(jarg)-zbilctdynv(jarg)
  !
  ! Affichage.
  !
  write(*,fmt=clformat) clexp(jarg)(1:9),' ',zbilcttend(jarg),' =' &
&   ,zbilctraysol(jarg),zbilctrayther(jarg) &
&   ,zbilctlat(jarg),zbilctsens(jarg),zbilctdynh(jarg),zbilctdynv(jarg),zresidu
enddo
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! Coefficients de réflexion, absorption, transmission dans le solaire.
!-------------------------------------------------
!
! Légendes.
!
write(*,'(9a)') 'Solar reflexion, absorption and transmission:'
write(*,'(9a)') ' '
write(*,'(9a)') '              atm_trans   atm_absorp   atm_reflex  surface_reflex planetary_reflex'
!
!-------------------------------------------------
! Calcul des valeurs climatologiques.
!-------------------------------------------------
!
zm(1,1)=340. ! solaire descendant au sommet.
zm(2,1)=24. ! solaire montant à la surface.
zm(1,2)=24.! solaire montant à la surface.
zm(2,2)=340. ! solaire descendant au sommet.  
zy(1)=185. ! solaire descendant à la surface.
zy(2)=100. ! solaire montant au sommet.
! La résolution de ce système donne dans zx(1) le coefficient t de transmission
! atmosphérique, et dans zx(2) le coefficient r de réflexion atmopshérique.
! Le coefficient a d'absorption atmosphérique peut alors se déduire par
! a+r+t=1..
!
!-------------------------------------------------
! On résout en x "m * x = y".
!-------------------------------------------------
!
itaille=2
call resol(zm,zy,itaille,zx,zdet)
ztransm_atm_clim=zx(1)
zreflex_atm_clim=zx(2)
zabsorp_atm_clim=1.-ztransm_atm_clim-zreflex_atm_clim
zreflex_surface=24./185.
zreflex_planetaire=100./340.
!
!-------------------------------------------------
! Format d'écriture des coefficients de réflexion, absorption, transmission dans le solaire.
!-------------------------------------------------
!
clfor_solc='(2a,15f13.3)'
!
!-------------------------------------------------
! Ecriture de la ligne avec les valeurs clim.
!-------------------------------------------------
!
write(*,fmt=clfor_solc) 'CLIM     ',' ',ztransm_atm_clim,zabsorp_atm_clim,zreflex_atm_clim,zreflex_surface,zreflex_planetaire
!
!-------------------------------------------------
! Ecriture des lignes avec les valeurs prévues par les modèles.
!-------------------------------------------------
!
do jarg=1,inarg
  !
  !-------------------------------------------------
  ! Réflexion (albédo) en surface: montant / descendant.
  !-------------------------------------------------
  !
  zreflex_surface=zsolaire_mon_surface(jarg)/zsolaire_des_surface(jarg)
  zreflex_planete=zsolaire_mon_sommet(jarg)/zsolaire_des_sommet(jarg)
  !
  !-------------------------------------------------
  ! Inversion de la matrice du système suivant:
  ! zsolaire_des_surface = zsolaire_des_sommet * t + zsolaire_mon_surface * r
  ! zsolaire_mon_sommet  = zsolaire_mon_surface* t + zsolaire_des_sommet  * r
  !-------------------------------------------------
  !
  zm(1,1)=zsolaire_des_sommet(jarg) ! solaire descendant au sommet.
  zm(2,1)=zsolaire_mon_surface(jarg) ! solaire montant à la surface.
  zm(1,2)=zsolaire_mon_surface(jarg) ! solaire montant à la surface.
  zm(2,2)=zsolaire_des_sommet(jarg) ! solaire descendant au sommet.  
  zy(1)=zsolaire_des_surface(jarg) ! solaire descendant à la surface.
  zy(2)=zsolaire_mon_sommet(jarg) ! solaire montant au sommet.
  ! La résolution de ce système donne dans zx(1) le coefficient t de transmission
  ! atmosphérique, et dans zx(2) le coefficient r de réflexion atmopshérique.
  ! Le coefficient a d'absorption atmosphérique peut alors se déduire par
  ! a+r+t=1..
  !
  !-------------------------------------------------
  ! On résout en x "m * x = y".
  !-------------------------------------------------
  !
  itaille=2
  call resol(zm,zy,itaille,zx,zdet)
  ztransm_atm=zx(1)
  zreflex_atm=zx(2)
  zabsorp_atm=1.-ztransm_atm-zreflex_atm
  write(*,fmt=clfor_solc) clexp(jarg)(1:9),' ',ztransm_atm,zabsorp_atm,zreflex_atm,zreflex_surface,zreflex_planete
enddo
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
!
!-------------------------------------------------
! En vue de montrer le bilan en surface on vérifie que tous les fichiers de DDH comportent l'information.
!-------------------------------------------------
!
llok=.true.
do jarg=1,inarg
  call lfacas(iul(jarg),'VLSM1',cltype,ilong,ierr)
  if(ierr /= 0) llok=.false.
enddo
if(llok) then
  !
  !-------------------------------------------------
  ! Flux sur continent.
  !-------------------------------------------------
  !
  write(*,'(a)')  'Flux over LAND:'
  !
  !-------------------------------------------------
  ! Lecture de la fraction de terre.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! On crée une ligne renseignant les indices expériences
  ! des différents fichiers de DDH.
  !-------------------------------------------------
  !
  write(*,'(2a)') ' '
  write(*,'(a,$)')  '                                CLIM'
  do jarg=1,inarg
    call lfalecc(iul(jarg),'INDICE EXPERIENCE',1,clnamx,ilong,ierr)
    ilnamx=len_trim(clnamx)
    write(*,'(2a,$)') '    ',clnamx(1:ilnamx)
  enddo
  write(*,'(2a)') ' '
  !
  !-------------------------------------------------
  ! Nouveau champ.
  !-------------------------------------------------
  !
  write(*,'(a,$)')  'Land fraction               %'
  zclim=29.
  write(*,'(a,f7.2,$)') ' ',zclim
  do jarg=1,inarg
    call lfaerf(iul(jarg),.false.) ! on rend LFA tolérant aux articles inexistants.
    call lfalecr(iul(jarg),'S01_0     ',jpprod,zval,ilong,ierr)
    if(ierr /= 0) call lfalecr(iul(jarg),'VLSM1     ',jpprod,zval,ilong,ierr)
    if(ierr /= 0) zval=0.
    zitm(jarg)=zval(1)
    zsor=100.*zitm(jarg)
    write(*,'(a,f7.2,$)') ' ',zsor
    if(zitm(jarg) == 0.) then
      !
      !-------------------------------------------------
      ! En cas de domaine entièrement sur mer,
      ! on préfère faire 0/1 que 0/0!...
      !-------------------------------------------------
      !
      zitm(jarg)=1.
    endif
  enddo
  write(*,'(2a)') ' '
  !
  !-------------------------------------------------
  ! Nouveau champ.
  !-------------------------------------------------
  !
  write(*,'(a,$)')  'Precipitation          mm/day'
  zclim=2.02
  write(*,'(a,f7.2,$)') ' ',zclim
  do jarg=1,inarg
    zrr=0.
    !
    !-------------------------------------------------
    call lfacas(iul(jarg),'G08',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G08       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSPRELIGE',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    !
    !-------------------------------------------------
    call lfacas(iul(jarg),'G09',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G09       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSPRENEGE',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    !
    !-------------------------------------------------
    call lfacas(iul(jarg),'G10',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G10       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSPRELICO',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    !
    !-------------------------------------------------
    call lfacas(iul(jarg),'G11',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G11       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSPRENECO',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    zsor=zrr/zechflux(jarg)*86400./zitm(jarg)
    write(*,'(a,f7.2,$)') ' ',zsor
  enddo
  write(*,'(2a)') ' '
  !
  !-------------------------------------------------
  ! Nouveau champ.
  !-------------------------------------------------
  !
  write(*,'(a,$)')  'Evaporation            mm/day'
  zclim=-1.33
  write(*,'(a,f7.2,$)') ' ',zclim
  do jarg=1,inarg
    zrr=0.
    call lfacas(iul(jarg),'G12',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G12       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSEVAPLIQ',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    call lfacas(iul(jarg),'G13',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G13       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSEVAPNEG',jpprod,zval,ilong,ierr)
    endif
    zrr=zrr+zval(1)
    zsor=zrr/zechflux(jarg)*86400./zitm(jarg)
    write(*,'(a,f7.2,$)') ' ',zsor
  enddo
  write(*,'(2a)') ' '
  !
  !-------------------------------------------------
  ! Nouveau champ.
  !-------------------------------------------------
  !
  write(*,'(a,$)')  'IR radiation            W*m-2'
  zclim=0.
  write(*,'(a,f7.2,$)') ' ',zclim
  do jarg=1,inarg
    call lfacas(iul(jarg),'G02',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G02       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSRAYTHDS',jpprod,zval,ilong,ierr)
    endif
    zsor=zval(1)/zechflux(jarg)/zitm(jarg)
    write(*,'(a,f7.2,$)') ' ',zsor
  enddo
  write(*,'(2a)') ' '
  !
  !-------------------------------------------------
  ! Nouveau champ.
  !-------------------------------------------------
  !
  write(*,'(a,$)')  'Solar radiation         W*m-2'
  zclim=0.
  write(*,'(a,f7.2,$)') ' ',zclim
  do jarg=1,inarg
    call lfacas(iul(jarg),'G01',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iul(jarg),'G01       ',jpprod,zval,ilong,ierr)
    else
      call lfalecr(iul(jarg),'FSRAYSOLR',jpprod,zval,ilong,ierr)
    endif
    zsor=zval(1)/zechflux(jarg)/zitm(jarg)
    write(*,'(a,f7.2,$)') ' ',zsor
  enddo
  write(*,'(2a)') ' '
endif ! llok
!
!-------------------------------------------------
! Fermeture des fichiers.
!-------------------------------------------------
!
do jarg=1,inarg
  call lfafer(iul(jarg))
enddo
!
!-------------------------------------------------
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers
! de DDH.
!-------------------------------------------------
!
call lignesep(inarg)
end
subroutine lignesep(karg)
! --------------------------------------------------------------------------
! **** *lignesep* Affichage d'une ligne de tirets.
! --------------------------------------------------------------------------
! Sujet:
! On crée une ligne de séparation
! dont la longueur est proportionnelle au nombre de fichiers de DDH.
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
! Auteur:         1997-09, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
#include"implicit_r8i4.h"
INTEGER(KIND=4) :: JARG
INTEGER(KIND=4) :: KARG

write(*,'(a,$)')  '------------------------------------'
do jarg=1,karg
  write(*,'(a,$)') '-------'
enddo
write(*,'(2a)') ' '
end
subroutine ecrit(pjul,pval,cdfic)
#include"implicit_r8i4.h"
clfic=trim(cdfic)//'.evol'
iul=40 ; open(iul,file=clfic,form='formatted',access='append')
write(iul,fmt=*) pjul,pval
close(iul)
end
#include"ddh_util.F90"
