!OPTIONS NODOUBLE
program ddhr
! --------------------------------------------------------------------------
! **** *DDHR* Renseignements sur le type de fichier de DDH.
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
!   92-12-04, J.M. Piriou: adaptation au calcul sur CRAY.
!   94-10-27, J.M. Piriou: adaptation au calcul sur lfa.
! 2008-07-17, J.M. Piriou: translate some outputs to English.
!
! --------------------------------------------------------------------------
implicit none
CHARACTER*200 :: CLARG
CHARACTER*200 :: CLART
CHARACTER*200 :: CLECH
CHARACTER*200 :: CLFDOC
CHARACTER*200 :: CLFDTA
CHARACTER*200 :: CLNFE
CHARACTER*200 :: CLPRI
CHARACTER*200 :: CLPRINT
CHARACTER*200 :: CLSAUT
CHARACTER*200 :: CLTYPE
CHARACTER*200 :: CLZE
CHARACTER*200 :: CLZUE
INTEGER(KIND=4) :: IARG
INTEGER(KIND=4) :: IARGCP
INTEGER(KIND=4) :: IDATE
INTEGER(KIND=4) :: IDATE_LOC
INTEGER(KIND=4) :: IDATEBJ
INTEGER(KIND=4) :: IDATECOU
INTEGER(KIND=4) :: IDELTA
INTEGER(KIND=4) :: IDOM
INTEGER(KIND=4) :: IECART
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: IFICDDH
INTEGER(KIND=4) :: ILARG
INTEGER(KIND=4) :: ILECH
INTEGER(KIND=4) :: ILEV
INTEGER(KIND=4) :: ILNAMX
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: ILPRI
INTEGER(KIND=4) :: ILSAUT
INTEGER(KIND=4) :: ILZE
INTEGER(KIND=4) :: IQUANT
INTEGER(KIND=4) :: IULDOC
INTEGER(KIND=4) :: IULDTA
INTEGER(KIND=4) :: IULENT
INTEGER(KIND=4) :: JARG
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: JFICDDH
INTEGER(KIND=4) :: JLEV
LOGICAL :: LLB
LOGICAL :: LLBA
LOGICAL :: LLBE
LOGICAL :: LLBJ
LOGICAL :: LLBJUL
LOGICAL :: LLBM
LOGICAL :: LLBQ
LOGICAL :: LLBS
LOGICAL :: LLD
LOGICAL :: LLDC
LOGICAL :: LLDDD
LOGICAL :: LLE
LOGICAL :: LLECH2
LOGICAL :: LLEH
LOGICAL :: LLEJ
LOGICAL :: LLEP
LOGICAL :: LLES
LOGICAL :: LLG
LOGICAL :: LLI
LOGICAL :: LLLFA
LOGICAL :: LLN
LOGICAL :: LLNNN
LOGICAL :: LLTOUT
LOGICAL :: LLTYPE
LOGICAL :: LLVAR
REAL(KIND=8) :: z180,zm180
REAL(KIND=8) :: ZANR
REAL(KIND=8) :: ZCONRD
REAL(KIND=8) :: ZDATECOU
REAL(KIND=8) :: ZECART
REAL(KIND=8) :: zech(1)
REAL(KIND=8) :: zechdv(1)
REAL(KIND=8) :: ZECHDVO
REAL(KIND=8) :: ZECHHEURES
REAL(KIND=8) :: ZECHO
REAL(KIND=8) :: ZLAT
REAL(KIND=8) :: ZLAT1
REAL(KIND=8) :: ZLAT2
REAL(KIND=8) :: ZLAT3
REAL(KIND=8) :: ZLAT4
REAL(KIND=8) :: ZLON
REAL(KIND=8) :: ZLON1
REAL(KIND=8) :: ZLON2
REAL(KIND=8) :: ZLON3
REAL(KIND=8) :: ZLON4
REAL(KIND=8) :: ZMODYX
REAL(KIND=8) :: ZMOISR
REAL(KIND=8) :: ZPRESL
REAL(KIND=8) :: ZPRESSION

CHARACTER*200 :: CGCONF
CHARACTER*200 :: CGNFE1
CHARACTER*200 :: CGNFE2
CHARACTER*200 :: CGNFLC
CHARACTER*200 :: CGNFSO
LOGICAL :: LGDEBU
LOGICAL :: LGFILAF
LOGICAL :: LGRENS

#include"ddhpar.h"
#include"ddht_yom_ent.h"
character*200 clnamx
character*200 clficddh(jparg) ! nom des fichiers de DDH d'entrée.
INTEGER(KIND=4) idatef(11)
INTEGER(KIND=4) idocfi(17)
INTEGER(KIND=4) irep
REAL(KIND=8) zdocd(11)
REAL(KIND=8) zvpp0(jpprod)
REAL(KIND=8) zpres(jplev)
REAL(KIND=8) zjul
REAL(KIND=8) zseconde
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  ! L'utilisateur n'a tape aucun argument.
  write(*,fmt='(a)') ' '
  write(*,fmt='(a)') 'Get some DDH file autodocumentation on standard output.'
  write(*,fmt='(a)') ' '
  write(*,fmt='(a)') 'Usage: ddhr [-i] [-b] [-bq] [-bm] [-bj] [-bjul] [-be] [-ech2] [-e] [-es] [-ej] [-eh] '&
        &'[-ep] [-var] [-d] [-dc] [-ddd] [-n] [-nnn] [-g] F1 [F2 ... Fn]'
  write(*,fmt='(a)') ' '
  write(*,fmt='(a)') 'where'
  write(*,fmt='(a)') '  F1 [F2 ... Fn] DDH file(s).'
  write(*,fmt='(a)') '  -i get the model run mnemonic (in 4 characters).'
  write(*,fmt='(a)') '  -b  get base; format: AAAA-MM-QQ-HH:MM.'
  write(*,fmt='(a)') '  -bs get base; format: AAAAMMQQ.'
  write(*,fmt='(a)') '  -bq get base, in number of days since year beginning.'
  write(*,fmt='(a)') '  -bm get base, in months: MM+(QQ-1)/31.'
  write(*,fmt='(a)') '  -ba get base; in years: AAAA + (MM-1)/12. + (QQ-1)/365.2425 + HH/8765.82 + MM/525949.2.'
  write(*,fmt='(a)') '  -be get base and prediction range.'
  write(*,fmt='(a)') '  -bjAAAAMMQQ base in days since the date AAAAMMQQ.'
  write(*,fmt='(a)') '  -bjul base in julian days.'
  write(*,fmt='(a)') '  -ech2 to get the date of the base as the "mean" one (Base + range / 2).'
  write(*,fmt='(a)') '  -e prediction range in days in text mode.'
  write(*,fmt='(a)') '  -es prediction range in seconds.'
  write(*,fmt='(a)') '  -ep prediction range in  time steps.'
  write(*,fmt='(a)') '  -eh prediction range in hours (closest integer(kind=4) value).'
  write(*,fmt='(a)') '  -ej prediction range in days.'
  write(*,fmt='(a)') '  -var if the prediction range to be used is that of the original file '&
        &'(in case of cumulated DDH files): article ECHEANCEDV from DDH file.'
  write(*,fmt='(a)') '                     By default the prediction range is that cumulated '&
        &'(in case of cumulated DDH files): article ECHEANCE   from DDH file.'
  write(*,fmt='(a)') '  -d   get the number of domains.'
  write(*,fmt='(a)') '  -dc  get autodocumentation about each domain.'
  write(*,fmt='(a)') '  -ddd get ASCII data ready to plot with longitude and latitudes of the domains'&
        &'; output on files type "doc dta".'
  write(*,fmt='(a)') '  -n number of model levels.'
  write(*,fmt='(a)') '  -nnn get pressure curve: p=f(level) in horizontal mean on all domains.'
  write(*,fmt='(a)') '  -g grid (domains + levels).'
  write(*,fmt='(a)') '  -type: writes out "lfa" on standard output if the file is lfa, NON_lfa else case.'
  stop
endif
!
! Initialisation par défaut.
!
lli=.false.
llb=.false.
llbs=.false.
llbq=.false.
llbm=.false.
llba=.false.
llbj=.false.
llbjul=.false.
llbe=.false.
llech2=.false.
lle=.false.
lles=.false.
llej=.false.
llep=.false.
lleh=.false.
lld=.false.
lldc=.false.
llddd=.false.
lln=.false.
llnnn=.false.
llg=.false.
lltype=.false.
lltout=.true.
clnfe='DZ.lfa'
llvar=.false.
ificddh=0 ! nombre de fichiers de DDH à traiter.
!
! Obtention des parametres d'entree par ligne de commande.
!
ierr=0
do jarg=1,iarg
  clarg=' '
  call getargp(jarg,clarg)
  ilarg=len_trim(clarg)
  if(clarg(1:ilarg) == '-i') then
    lli=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-b') then
    llb=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-bs') then
    llbs=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-bq') then
    llbq=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-bm') then
    llbm=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-ba') then
    llba=.true.
    lltout=.false.
  elseif(clarg(1:5) == '-bjul') then
    llbjul=.true.
    lltout=.false.
  elseif(clarg(1:3) == '-bj') then
    llbj=.true.
    read(clarg,fmt='(3x,i8)') idatebj
    lltout=.false.
  elseif(clarg(1:ilarg) == '-be') then
    llbe=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-ech2') then
    llech2=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-e') then
    lle=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-es') then
    lles=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-ej') then
    llej=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-ep') then
    llep=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-eh') then
    lleh=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-d') then
    lld=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-dc') then
    lldc=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-ddd') then
    llddd=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-n') then
    lln=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-nnn') then
    llnnn=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-g') then
    llg=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-type') then
    lltype=.true.
    lltout=.false.
  elseif(clarg(1:ilarg) == '-var') then
    llvar=.true.
  else
    !
    !-------------------------------------------------
    ! L'argument est un nom de fichier de DDH.
    !-------------------------------------------------
    !
    ificddh=ificddh+1
    if(ificddh > jparg) then
      !
      ! On va déborder physiquement le tableau clficddh.
      !
      print*,'DDHR/ERREUR: trop de fichiers d''entrée.'
      print*,'Recompiler avec une valeur plus grande de jparg!...'
      print*,jparg
      call exit(1)
    endif
    clficddh(ificddh)=clarg(1:ilarg)
  endif
enddo
!
!-------------------------------------------------
! Constantes.
!-------------------------------------------------
!
z180=180.
zm180=-180.
!
! -------------------------------------------------
! Boucle sur les ificddh fichiers de DDH à traiter.
! -------------------------------------------------
!
do jficddh=1,ificddh
  clnfe=clficddh(jficddh)
  if(lltout) then
    print*,'------------------------------------------------------'
    print*,'DDH file ',clnfe(1:index(clnfe,' ')-1)
  endif
  !
  ! --------------------------------------------------------------------------
  ! **   Ouverture.
  !
  iulent=14
  if(lltype) then
    !
    ! Teste si le fichier est bien lfa.
    !
    call lfatest(iulent,clnfe,lllfa)
    if(lllfa) then
      write(*,fmt='(a)') 'lfa'
    else
      write(*,fmt='(a)') 'NON_lfa'
    endif
    stop
  endif
  call lfaouv(iulent,clnfe,'R')
  !
  ! --------------------------------------------------------------------------
  ! **   Lecture des articles de documentation.
  !
  ! Lecture du premier article: indice en 4 lettres de l'experience.
  !
  call lfalecc(iulent,'INDICE EXPERIENCE',1,clnamx,ilong,ierr)
  !
  ! DATE.
  !
  call lfaleci(iulent,'DATE',11,idatef,ilong,ierr)
  !
  ! DOCFICHIER.
  !
  call lfaleci(iulent,'DOCFICHIER',17,idocfi,ilong,ierr)
  idom=idocfi(15) ! nombre de domaines.
  ilev=idocfi(6) ! nombre de niveaux.
  !
  !-------------------------------------------------
  ! ECHEANCE.
  !-------------------------------------------------
  !
  call lfalecr(iulent,'ECHEANCE',1,zech,ilong,ierr)
  !
  !-------------------------------------------------
  ! ECHEANCEDV.
  !-------------------------------------------------
  !
  call lfacas(iulent,'ECHEANCEDV',cltype,ilong,ierr)
  if(ierr == 0) then
    !
    ! Cet article existe bien dans le fichier.
    ! On le lit.
    !
    call lfalecr(iulent,'ECHEANCEDV',1,zechdv,ilong,ierr)
  else
    !
    ! Cet article n'existe pas dans le fichier.
    ! On assimile cette échéance à l'article ECHEANCE.
    !
    zechdv(1)=zech(1)
  endif
  !
  !-------------------------------------------------
  ! Si l'utilisateur souhaite que l'échéance du fichier
  ! soit celle des variables, on force la 1ère
  ! à la valeur de la seconde.
  !-------------------------------------------------
  !
  if(llvar) then
    zech(1)=zechdv(1)
  endif
  if(llech2) then
    !
    !-------------------------------------------------
    ! L'utilisateur souhaite utiliser comme base
    ! la date "moyenne" du run, soit base+ech/2.
    !-------------------------------------------------
    !
    idate_loc=10000*idatef(1)+100*idatef(2)+idatef(3)
    idelta=int(0.5*zech(1)/86400.)
    call daplusj(idate_loc,idelta,idate)
    idatef(3)=mod(idate,100) ; idate=idate/100
    idatef(2)=mod(idate,100) ; idate=idate/100
    idatef(1)=idate
  endif
  if(zech(1) >= 259200.) then
    ! Cas de runs de plus de 3 jours >> commentaire en jours
    zecho=zech(1)/86400.
    clzue=' J'
  else
    ! Cas de runs de moins de 3 jours >> commentaire en heures
    zecho=zech(1)/3600.
    clzue=' H'
  endif
  !
  ! Affichage de l'echeance avec deux chiffres apres la virgule.
  ! Si l'echeance est voisine d'un entier a mieux que 10**-5 pres,
  ! on l'affiche au format entier.
  !
  call reecar(zecho,2,2,clze,ilze)
  clech=clze(1:ilze)//clzue
  ilech=len_trim(clech)
  if(zechdv(1) >= 259200.) then
    ! Cas de runs de plus de 3 jours >> commentaire en jours
    zechdvo=zechdv(1)/86400.
    clzue=' J'
  else
    ! Cas de runs de moins de 3 jours >> commentaire en heures
    zechdvo=zechdv(1)/3600.
    clzue=' H'
  endif
  !
  ! Affichage de l'echeance avec deux chiffres apres la virgule.
  ! Si l'echeance est voisine d'un entier a mieux que 10**-5 pres,
  ! on l'affiche au format entier.
  !
  call reecar(zechdvo,2,2,clze,ilze)
  !
  ! Impression de la valeur de ces articles documentaires.
  !
  if(lli) then
    !
    ! Impression de l'indice expérience.
    !
    ilnamx=len_trim(clnamx)
    write(*,'(a)') clnamx(1:ilnamx)
  endif
  if(llb) then
    !
    ! Impression de la base.
    !
    write(*,'(i4.4,4(a,i2.2),a,f5.1,a)') idatef(1),'-',idatef(2),'-',idatef(3),'-',idatef(4),':',idatef(5)
  endif
  if(llbs) then
    !
    ! Impression de la base.
    !
    write(*,'(i4.4,2(i2.2))') idatef(1),idatef(2),idatef(3)
  endif
  if(llbq) then
    !
    ! Impression de la base, donnée par son quantième de l'année.
    !
    idate=10000*idatef(1)+100*idatef(2)+idatef(3)
    call quant(idate,iquant)
    write(*,'(i3.3)') iquant
  endif
  if(llbm) then
    !
    ! Impression de la base, donnée par son mois réel: MM+(QQ-1)/31.
    !
    zmoisr=real(idatef(2))+real(idatef(3)-1)/31.
    if(zmoisr >= 10.) then
      write(*,'(f7.4)') zmoisr
    else
      write(*,'(f6.4)') zmoisr
    endif
  endif
  if(llba) then
    !
    ! Impression de la base, donnée par son année réelle: AAAA+(MM-1)/12.+(QQ-1)/365.2425+...
    !
    zanr=real(idatef(1))+real(idatef(2)-1)/12.+real(idatef(3)-1)/365.2425 &
    & +real(idatef(4))/8765.82+real(idatef(5))/525949.2
    write(*,'(f11.6)') zanr
  endif
  if(llbj) then
    !
    ! Impression de la base en jours depuis une date donnée.
    !
    idatecou=idatef(1)*10000+idatef(2)*100+idatef(3)
    zdatecou=real(idatef(4))/24.+real(idatef(5))/1440.
    call ecartdj(idatebj,idatecou,iecart)
    if(zdatecou == 0.) then
      !
      !-------------------------------------------------
      ! L'heure est nulle.
      ! Ecriture du nombre de jours sous forme d'un entier.
      !-------------------------------------------------
      !
      write(*,'(i5.5)') iecart
    else
      !
      !-------------------------------------------------
      ! L'heure est non nulle.
      ! Ecriture du nombre de jours sous forme d'un réel.
      !-------------------------------------------------
      !
      zecart=iecart+zdatecou
      if(zecart >= 0.) then
        write(*,'(es13.7)') zecart
      else
        write(*,'(es14.7)') zecart
      endif
    endif
  endif
  if(llbjul) then
    !
    ! Impression de la base en date julienne.
    !
    zseconde=0.
    call AMQHMS_VERS_DJ(idatef(1),idatef(2),idatef(3),idatef(4),idatef(5),zseconde,zjul)
    write(*,*) zjul
  endif
  if(llbe) then
    !
    ! Impression de la base-échéance.
    !
    write(*,'(i4.4,4(a,i2.2),2a)') idatef(1),'-',idatef(2) &
&     ,'-',idatef(3),' ',idatef(4),':',idatef(5),' + ',clech(1:ilech)
  endif
  if(lle) then
    !
    ! Impression de l'échéance.
    !
    write(*,'(a)') clech(1:ilech)
  endif
  if(lles) then
    !
    ! Impression de l'échéance en secondes.
    !
    write(clech,fmt='(f11.2)') zech(1)
    call fillzero(clech,clprint)
    write(*,'(a)') clprint(1:len_trim(clprint))
  endif
  if(llej) then
    !
    ! Impression de l'échéance en jours.
    !
    write(clech,fmt=*) zech(1)/86400.
    call fillzero(clech,clprint)
    write(*,'(a)') clprint(1:len_trim(clprint))
  endif
  if(llep) then
    !
    ! Impression de l'échéance en pas de temps.
    !
    write(*,fmt='(i4.4)') idocfi(5)
  endif
  if(lleh) then
    !
    ! Impression de l'échéance en heures.
    !
    zechheures=zech(1)/3600.
    write(clech,fmt='(f10.4)') zechheures
    call fillzero(clech,clprint)
    write(*,'(a)') clprint(1:len_trim(clprint))
  endif
  if(lld) then
    !
    ! Impression du nombre de domaines.
    !
    call intcar(idocfi(15),clpri,ilpri)
    write(*,'(a)') clpri(1:ilpri)
  endif
  if(lldc.or.llddd) then
    !
    ! Impression de renseignements sur chacun des domaines.
    !
    zconrd=45./atan(1.)
    if(idocfi(1) == 1) then
      print*,'DOMAINES LIMITES'
    elseif(idocfi(1) == 5) then
      print*,'DOMAINE GLOBAL'
      stop
    elseif(idocfi(1) == 6) then
      print*,'BANDES ZONALES'
    else
      print*,'ddhr ERREUR: type de domaine inconnu!...'
      stop
    endif
    if(llddd) then
      clfdoc=clnfe(1:index(clnfe,' ')-1)//'.tmp.dom.doc'
      clfdta=clnfe(1:index(clnfe,' ')-1)//'.tmp.dom.dta'
      iuldoc=22
      iuldta=23
      open(iuldoc,file=clfdoc,form='formatted')
      open(iuldta,file=clfdta,form='formatted')
      clsaut='999.999 999.999' ! saut de plume.
      ilsaut=len_trim(clsaut)
      write(iuldoc,fmt='(9a)') '#FORMAT=LLT'
      write(iuldoc,'(a,i4.4,4(a,i2.2),3a)') '#DATE=',idatef(1),'-' &
&       ,idatef(2),'-',idatef(3),' ',idatef(4),':',idatef(5) &
&       ,' ECH ',clech(1:ilech),'.'
      write(iuldoc,fmt='(9a)') '#TITRE=Domaines DDH de ',clnfe(1:index(clnfe,' ')-1)
      write(iuldoc,fmt='(9a)') '#ORIGINE=',clnfe(1:index(clnfe,' ')-1)
      write(iuldoc,fmt='(9a)') '#LEGENDE_X=longitude (deg)'
      write(iuldoc,fmt='(9a)') '#LEGENDE_Y=latitude (deg)'
    endif
    do jdom=1,idom
      print*,'Domaine ',jdom,':'
      !
      ! On lit la documentation du domaine jdom.
      !
      call lited(iulent,jdom,'R',zdocd,irep)
      !
      ! Pour chaque type de domaine, on affiche
      ! ses caractéristiques.
      !
      if(nint(zdocd(11)) == 1.or.nint(zdocd(11)) == 4) then
        !
        ! Type point.
        !
        zlon=zconrd*zdocd(3)
        zlat=zconrd*asin(zdocd(4))
        zlon=zmodyx(zlon,zm180,z180)
        write(*,fmt='(a,f9.4,a,f9.4)') '  type point, longitude=',zlon,', latitude=',zlat
        if(llddd) then
          write(iuldta,fmt='(a)') clsaut(1:ilsaut)
          write(iuldta,*) zlon,zlat
          write(iuldta,*) zlon,zlat
        endif
      elseif(nint(zdocd(11)) == 2) then
        !
        ! Type quadrilatère.
        !
        zlon1=zconrd*zdocd(3)
        zlon2=zconrd*zdocd(5)
        zlon3=zconrd*zdocd(7)
        zlon4=zconrd*zdocd(9)
        !
        zlat1=zconrd*asin(zdocd(4))
        zlat2=zconrd*asin(zdocd(6))
        zlat3=zconrd*asin(zdocd(8))
        zlat4=zconrd*asin(zdocd(10))
        !
        zlon1=zmodyx(zlon1,zm180,z180)
        zlon2=zmodyx(zlon2,zm180,z180)
        zlon3=zmodyx(zlon3,zm180,z180)
        zlon4=zmodyx(zlon4,zm180,z180)
        !
        print*,'  type quadrilatère'
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin1: longitude=',zlon1,', latitude=',zlat1
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin2: longitude=',zlon2,', latitude=',zlat2
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin3: longitude=',zlon3,', latitude=',zlat3
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin4: longitude=',zlon4,', latitude=',zlat4
        if(llddd) then
          write(iuldta,fmt='(a)') clsaut(1:ilsaut)
          write(iuldta,*) zlon1,zlat1
          write(iuldta,*) zlon2,zlat2
          write(iuldta,*) zlon3,zlat3
          write(iuldta,*) zlon4,zlat4
          write(iuldta,*) zlon1,zlat1
        endif
      elseif(nint(zdocd(11)) == 3) then
        !
        ! Type rectangle.
        !
        zlon1=zconrd*zdocd(3)
        zlat1=zconrd*asin(min(1.,max(-1.,zdocd(4))))
        zlon2=zconrd*zdocd(5)
        zlat2=zconrd*asin(min(1.,max(-1.,zdocd(8))))
        !
        zlon1=zmodyx(zlon1,zm180,z180)
        zlon2=zmodyx(zlon2,zm180,z180)
        if(zlon2 < zlon1) zlon2=zlon2+360.
        !
        print*,'  type rectangle'
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin1: longitude=',zlon1,', latitude=',zlat1
        write(*,fmt='(a,f9.4,a,f9.4)') '  coin2: longitude=',zlon2,', latitude=',zlat2
        if(llddd) then
          write(iuldta,fmt='(a)') clsaut(1:ilsaut)
          write(iuldta,*) zlon1,zlat1
          write(iuldta,*) zlon2,zlat1
          write(iuldta,*) zlon2,zlat2
          write(iuldta,*) zlon1,zlat2
          write(iuldta,*) zlon1,zlat1
        endif
      elseif(nint(zdocd(11)) == 5) then
        !
        ! Type globe.
        !
        print*,'  type globe entier'
      elseif(nint(zdocd(11)) == 6) then
        !
        ! Type bande zonale.
        !
        zlat=zconrd*asin(zdocd(4))
        zlat1=zconrd*asin(1.-(float(jdom-1))*2./float(idocfi(15)))
        zlat2=zconrd*asin(1.-(float(jdom))*2./float(idocfi(15)))
        print*,'  type bande zonale, de latitude nord ',zlat1,', moyenne ',zlat,', sud ',zlat2
        if(llddd) then
          write(iuldta,fmt='(a)') clsaut(1:ilsaut)
          write(iuldta,*) zm180,zlat1
          write(iuldta,*)  z180,zlat1
          write(iuldta,*)  z180,zlat2
          write(iuldta,*) zm180,zlat2
          write(iuldta,*) zm180,zlat1
        endif
      else
        print*,'ddhr ERREUR: le type de domaine ',nint(zdocd(11)),' n''est pas traité actuellement!...'
        stop
      endif
    enddo
    if(llddd) then
      close(iuldoc)
      close(iuldta)
      print*,'Tracé des domaines porté sur les fichiers'
      print*,clfdoc(1:index(clfdoc,' ')-1)
      print*,clfdta(1:index(clfdta,' ')-1)
    endif
  endif
  if(lln) then
    !
    ! Impression du nombre de niveaux.
    !
    call intcar(ilev,clpri,ilpri)
    write(*,'(a)') clpri(1:ilpri)
  endif
  if(llnnn) then
    !
    ! Sortie de la courbe p=f(niveaux).
    !
    call lfacas(iulent,'VCP0',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iulent,'VCP0',jpprod,zvpp0,ilong,ierr)
    else
      call lfalecr(iulent,'VPP0',jpprod,zvpp0,ilong,ierr)
    endif
    clfdoc=clnfe(1:index(clnfe,' ')-1)//'.tmp.niv.doc'
    clfdta=clnfe(1:index(clnfe,' ')-1)//'.tmp.niv.dta'
    iuldoc=22
    iuldta=23
    open(iuldoc,file=clfdoc,form='formatted')
    open(iuldta,file=clfdta,form='formatted')
    write(iuldoc,fmt='(9a)') '#FORMAT=XV'
    write(iuldoc,'(a,i4.4,4(a,i2.2),3a)') '#DATE=',idatef(1),'-' &
&     ,idatef(2),'-',idatef(3),' ',idatef(4),':',idatef(5) &
&     ,' ECH ',clech(1:ilech),'.'
    write(iuldoc,fmt='(9a)') '#TITRE=Coordonnée pression de ',clnfe(1:index(clnfe,' ')-1)
    write(iuldoc,fmt='(9a)') '#ORIGINE=',clnfe(1:index(clnfe,' ')-1)
    write(iuldoc,fmt='(9a)') '#UNITE=hPa'
    write(iuldoc,fmt='(9a)') '#LEGENDE_X=niveaux'
    write(iuldoc,fmt='(9a)') '#LEGENDE_Y=pression (hPa)'
    do jlev=1,ilev
      zpres(jlev)=0.
    enddo
    do jdom=1,idom
      zpresl=0.
      do jlev=1,ilev
        !
        ! On intègre le champ VPP0
        ! moyen sur tous les domaines sur la verticale
        ! en le multipliant par g/100 pour avoir
        ! des hPa.
        !
        zpresl=zpresl+zvpp0(jlev)*9.80665/100.
        zpres(jlev)=zpres(jlev)+zpresl/idom
      enddo
    enddo
    do jlev=1,ilev
      if(jlev == 1) then
        zpression=0.5*zpres(jlev)
      else
        zpression=0.5*(zpres(jlev)+zpres(jlev-1))
      endif
      write(iuldta,fmt='(i3,f8.2)') jlev,zpression
    enddo
    close(iuldoc)
    close(iuldta)
    print*,'Tracé des niveaux moyens porté sur les fichiers'
    print*,clfdoc(1:index(clfdoc,' ')-1)
    print*,clfdta(1:index(clfdta,' ')-1)
  endif
  if(llg) then
    !
    ! Impression de la grille.
    !
    write(*,*) idocfi(15),' dom., ',ilev,' niv.'
  endif
  if(lltype) then
    !
    ! Teste si le fichier est bien lfa.
    !
    write(*,*) idocfi(15),' dom., ',ilev,' niv.'
  endif
  if(lltout) call pridocf(idocfi,idatef,clnamx,zech,zechdv,6)
  !
  ! --------------------------------------------------------------------------
  ! **   Fermeture des fichiers d'entree.
  !
  call lfafer(iulent)
enddo
end
#include"lited.F90"
