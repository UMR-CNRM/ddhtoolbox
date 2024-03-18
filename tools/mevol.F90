module evoldata
integer(kind=4), parameter :: jplevx=300
!
!-------------------------------------------------
! gprof: profil initial (celui du premier fichier).
!-------------------------------------------------
!
real(kind=8) :: gprof(jplevx)
logical :: lgdiff
end module evoldata
program mevol
! --------------------------------------------------------------
! **** *mevol* Evolution temporelle d'un champ de n fichiers LFA.
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
real(kind=8), allocatable :: zneb(:,:)
real(kind=8), allocatable :: zccb(:,:) ! status convectif local.
real(kind=8), allocatable :: zaut(:,:) ! status convectif local.
real(kind=8), allocatable :: zegn(:,:) ! status convectif local.
real(kind=8), allocatable :: zfm(:,:) ! status convectif local.
integer(kind=4), allocatable :: inlabPOST_FORC(:,:) ! status convectif local.
real(kind=8), allocatable :: zqc(:,:) ! status convectif local.
real(kind=8), allocatable :: zcape(:) ! status convectif local.
real(kind=8), allocatable :: zcin(:) ! status convectif local.
real(kind=8), allocatable :: zbuoy(:,:) ! status convectif local.
real(kind=8), allocatable :: ztemps(:)
real(kind=8), allocatable :: z1dloc_full(:)
real(kind=8), allocatable :: z1dloc_half(:)
real(kind=8), allocatable :: zpcl(:,:)
real(kind=8), allocatable :: zpcn(:,:)
real(kind=8), allocatable :: zpsl(:,:)
real(kind=8), allocatable :: zpsn(:,:)
real(kind=8), allocatable :: zhcla(:)
integer(kind=4), allocatable :: itypeprec(:,:)

real(kind=8), allocatable :: zpfplcl(:)
real(kind=8), allocatable :: zpfplcn(:)
real(kind=8), allocatable :: zpfplsl(:)
real(kind=8), allocatable :: zpfplsn(:)

real(kind=8), allocatable :: zcooz(:)

integer(kind=4), parameter :: jplev=300
real(kind=8) :: zt(jplev)
real(kind=8) :: zthetav(jplev)
real(kind=8) :: zqv(jplev)
real(kind=8) :: zp(jplev)
real(kind=8) :: zz(jplev)
real(kind=8) :: zelevation(jplev)
real(kind=8) :: fth_thetav
real(kind=8) :: fth_hcla
real(kind=8) :: ztab(1)
integer(kind=4) :: itab(1)
integer(kind=4) :: iarg

character*2 :: cltype
character*200, allocatable :: clligc(:)
character*200, allocatable :: clficlfa(:)
!
! -------------------------------------------------
! Valeurs par défaut.
! -------------------------------------------------
!
zcoef=1.
zadd=0.
clsuffdta='.tmp.evol'
clsuffdoc='.tmp.doc'
iniv=-1 ! niveaux souhaités en sortie: 0 dernier, -1 tous, sinon niveau.
clcooy='HEI' ! choix de coordonnée Y pour les tracés.
clcoox='LST' ! choix de coordonnée X pour les tracés.
clindef='dfhdshsfdh'
cluni=clindef ! unité de sortie du temps.
cluf=' ' ! unité du champ.
clna=' ' ! nom de l'article à lire.
iranf=0 ! rang du fichier LFA lu.
llkindat=.true. ! vrai si la date est donnée par KINDAT et KSSSSS, faux si NINDAT et NSSSSS.
llxcoo=.false.
lluserdta=.false. ! vrai si le nom du fichier de sortie est imposé par l'utilisateur.
lluserdoc=.false. ! vrai si le nom du fichier de sortie est imposé par l'utilisateur.
llft=.false. ! vrai si conversion des flux en tendances.
llopen=.false. ! vrai si le fichier ASCII de sortie est déjà ouvert.
llfirst=.true. ! vrai si le fichier courant est le premier lu.
zindef=-5.21324e26
iindef=-85632
zref_time=zindef
lgdiff=.false.
gprof=0.
llcompo=.false. ! vrai si l'utilisateur veut créer une image composite.
clfcompo='mevol.composite.gif'
ixcompo=600
iycompo=480
zprr=35. ! seuil de RR en mm/jour, conditionnant la densité de points tracés.
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
  write(*,'(9a)') &
&   'Temporal evolution on n LFA files' &
&   ,' coming out from 1D model.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   'Usage: mevol [-compo] [-xcompo] [-ycompo] [-nX] [-ft] [-diff] [-xX] [-uU] [-uf] [-yY] [-cC] [-aA]' &
&   ,' [-oF] [-dD] article ' &
&   ,'f1 [f2] ... [fn]'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'with'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  article: LFA article from which you want to ' &
&   ,'get the temporal evolution.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') '  -compo writes a composite image with cloudiness and precipitation'
  write(*,'(9a)') '    Default: no image output.'
  write(*,'(9a)') '  -fcompo file name of this composite image.'
  write(*,'(9a)') '    Default: ',trim(clfcompo)
  write(*,'(9a)') '  -xcompo X size in pixels of the composite image.'
  write(*,'(a,i4)') '    Default: ',ixcompo
  write(*,'(9a)') '  -ycompo Y size in pixels of the composite image.'
  write(*,'(a,i4)') '    Default: ',iycompo
  write(*,'(9a)') '  Shading legend of these composite images: '
  write(*,'(9a)') '       - The background color goes from white to grey as cloudiness goes from 0 to 1..'
  write(*,'(9a)') '       - Resolved-scale precipitation: red dots.'
  write(*,'(9a)') '       - Sub-grid-scale precipitation: dark cyan dots.'
  write(*,'(9a)') '       - The density of precipitation dots is proportional '
  write(*,'(a,g16.7,a)') '         to the local precipitation rate divided by ',zprr,' mm/day.'
  write(*,'(9a)') '       - The black line is the top of PBL.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -nX vertical levels you want: you give here' &
&   ,' in X a level value'
  write(*,'(9a)') '    or the word "integ" if you want ' &
&   ,'the vertical integral from that field(article)'
  write(*,'(9a)') '       weighted by delta_p/g'
  write(*,'(9a)') '    or the word "last" or "l" if you want the lowest level.'
  write(*,'(9a)') '    If the number of levels requested by the user is bigger than 1'
  write(*,'(9a)') '    the output file format will be "time level value".'
  write(*,'(9a)') &
&   '    The unit for time depends on -u choice (see below).'
  write(*,'(9a)') '    Default: output from all levels.'
  write(*,'(9a)') '    Examples: -nlast, -nl, -n6, -ninteg.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') '  -ft if you want fluxes to be converted into tendencies;'
  write(*,'(9a)') '    in that case the operation -g*dA/dp is performed,'
  write(*,'(9a)') '    A being the article name given on command line.'
  write(*,'(9a)') '    Default: no conversion done.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') '  -diff if you want in output the difference between each profile and INITIAL profile'
  write(*,'(9a)') '    rather than profile value.'
  write(*,'(9a)') '    Default: no difference done.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)')  '  -xX time coordinate (x coordinate) to be computed:'
  write(*,'(9a)')  '    - if X is RSTATI the time is given by the RSTATI field from current LFA file (model integration time).'
  write(*,'(9a)') '    - if X is NINDAT, time is computed as elapsed time between '
  write(*,'(9a)') '      NINDAT+NSSSSS+RSTATI from first file and same expresssion from current one.'
  write(*,'(9a)') '    - if X is LST (local solar time), time is computed as elapsed time between '
  write(*,'(9a)')  '      NINDAT+NSSSSS+RSTATI from current file and last time for which local solar time'
  write(*,'(9a)')  '      from that place was 00.'
  write(*,'(9a)')  '      Goal of that time coordinate: that time is a multiple of day duration'
  write(*,'(9a)')  '      at each mean solar time midnight.'
  write(*,'(9a)') '    - if X is SYB (Since Year Begining), time is computed as elapsed time between '
  write(*,'(9a)')  '      NINDAT+NSSSSS+RSTATI from current file and 1st january at 0h UTC.'
  write(*,'(9a)') '    - if X is JD (Julian Days), time is computed as current julian date.'
  write(*,'(9a)') '      WARNING: if one wants this date to be expressed in days, the use of -ud as described below is required!'
  write(*,'(9a)') '    - if X is TSTEP time is the time step number.'
  write(*,'(9a)') '    Default: ',clcoox(1:len_trim(clcoox)),'.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '    WARNING: if only one file is given in input,'
  write(*,'(9a)') &
&   '    no x coordinate is provided.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -uU unit of time for x above coordinate:' &
&   ,' U is s for seconds,' &
&   ,' h for hours, d for days.'
  write(*,'(9a)') '    Default: hours for runs shorter than 3 days, days else case.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') '  -ufU unit of field. the U characters will be written in the autodocumentation file.'
  write(*,'(9a)') '    Default: blank.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -yY kind of vertical coordinate (y coordinate)' &
&   ,' to be computed:'
  write(*,'(9a)') &
&   '    if Y is LEV the vertical coordinate will be ' &
&   ,'the levels.'
  write(*,'(9a)') &
&   '    if Y is PRE the vertical coordinate will be ' &
&   ,'the pressure (hPa).'
  write(*,'(9a)') &
&   '    if Y is HEI the vertical coordinate will be ' &
&   ,'the height (km).'
  write(*,'(9a)') '    Default: ',clcooy(1:len_trim(clcooy))
  write(*,'(9a)') &
&   '    This option is used only if more than 1 level in the field.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -cC conversion factor: the field read on input ' &
&   ,'file will be multiplied' &
&     ,' by C'
  write(*,'(9a)') &
&     '    before being written on output file.'
  write(*,'(9a)') &
&     '    Example: -c86400. to convert K/s to K/day.'
  write(*,'(9a)') '    Default: C=1..'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -aA addition value: the field read on input ' &
&   ,'file will be augmented' &
&     ,' by A'
  write(*,'(9a)') &
&     '    before being written on output file.'
  write(*,'(9a)') &
&     '    Example: -a-273.16 to convert K to °C.'
  write(*,'(9a)') '    Default: A=0..'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '    WARNING: if -c and -a options are used simultaneously,'
  write(*,'(9a)') &
&   '    addition is performed before multiplication.'
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -oF output file name.'
  write(*,'(9a)') '    Default: F=$article' &
&   ,clsuffdta(1:len_trim(clsuffdta))
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  -dD autodocumentation output file name.'
  write(*,'(9a)') '    Default: D=$article' &
&   ,clsuffdoc(1:len_trim(clsuffdoc))
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') &
&   '  f1 [f2] ... [fn]: LFA files, each one contains' &
&   ,' fields at a given time' &
&   ,' from evolution.'
  write(*,'(9a)')
  ! -------------------------------------------------
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Examples:'
  write(*,'(9a)') '  - mevol PFPLCL OUT*'
  write(*,'(9a)') &
&   '    will create a file containing on each line "<time> ' &
&   ,'<pressure> <PFPLCL value at all this level>"'
  write(*,'(9a)') '  - mevol PFPLCL -nl OUT*'
  write(*,'(9a)') &
&   '    will create a file containing "<time> ' &
&   ,'<PFPLCL value at surface>"'
  write(*,'(9a)') '  - mevol -xNINDAT -ud -nl PFPLCL OUT*'
  write(*,'(9a)') &
&   '    will do the same instead of time as elapsed' &
&   ,' one from first file' &
&   ,' and time in days.'
  write(*,'(9a)') '  - mevol PFPLCL -n10 OUT*'
  write(*,'(9a)') &
&   '    will create a file containing "<time> ' &
&   ,'<PFPLCL value at level 10>"'
  write(*,'(9a)') '  - mevol PFPLCL -nlast -oOF OUT*'
  write(*,'(9a)') &
&   '    will create a file named OF containing "<time> ' &
&   ,'<PFPLCL value at surface>"'
  write(*,'(9a)') '  - mevol -ninteg PQ OUT*'
  write(*,'(9a)') &
&   '    will create a file containing "<time> ' &
&   ,'<vertical integral of water vapour in_kg/m2>."'
  write(*,'(9a)') '  - mevol -compo -xcompo400 -ycompo250 O*lfa'
  write(*,'(9a)') ' '
  write(*,'(9a)') ' '
  ! -------------------------------------------------
  stop
endif
!
!-------------------------------------------------
! On copie la ligne de commande sur un tableau
! et on analyse cette ligne de commande.
!-------------------------------------------------
!
allocate(clligc(iarg))
allocate(clficlfa(iarg))
inficlfa=0
do jarg=1,iarg
  call getargp(jarg,clligc(jarg))
  clarg=clligc(jarg)
  !
  !-------------------------------------------------
  ! Le nom courant est-il celui d'un fichier?
  !-------------------------------------------------
  !
  inquire(file=clarg,exist=llexist)
  !
  !-------------------------------------------------
  ! Test de la ligne de commande.
  !-------------------------------------------------
  !
  if(clarg(1:2) == '-n') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de niveaux.
    ! -------------------------------------------------
    !
    if(clarg(3:7) == 'integ') then
      iniv=-2
    elseif(clarg(3:3) == 'l') then
      iniv=0
    else
      !
      ! -------------------------------------------------
      ! Lecture du niveau souhaité.
      ! -------------------------------------------------
      !
      read(clarg(3:),fmt=*) iniv
    endif
  elseif(clarg(1:3) == '-uf') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix d'unité pour le temps.
    ! -------------------------------------------------
    !
    cluf=clarg(4:)
  elseif(clarg(1:2) == '-u') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix d'unité pour le temps.
    ! -------------------------------------------------
    !
    cluni=clarg(3:)
  elseif(clarg(1:3) == '-ft') then
    !
    ! -------------------------------------------------
    ! L'utilisateur veut convertir les flux en tendences.
    ! -------------------------------------------------
    !
    llft=.true.
  elseif(trim(clarg) == '-compo') then
    !
    ! -------------------------------------------------
    ! L'utilisateur veut créer une image composite.
    ! -------------------------------------------------
    !
    llcompo=.true.
  elseif(clarg(1:7) == '-xcompo') then
    !
    ! -------------------------------------------------
    ! L'utilisateur veut créer une image composite.
    ! -------------------------------------------------
    !
    read(clarg(8:),fmt=*) ixcompo
  elseif(clarg(1:7) == '-ycompo') then
    !
    ! -------------------------------------------------
    ! L'utilisateur veut créer une image composite.
    ! -------------------------------------------------
    !
    read(clarg(8:),fmt=*) iycompo
  elseif(clarg(1:7) == '-fcompo') then
    !
    ! -------------------------------------------------
    ! Nom du fichier-image composite.
    ! -------------------------------------------------
    !
    clfcompo=clarg(8:)
  elseif(clarg(1:2) == '-o') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de nom de fichier de sortie.
    ! -------------------------------------------------
    !
    clficsdta=clarg(3:)
    lluserdta=.true.
  elseif(clarg(1:2) == '-x') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de coordonnée X.
    ! -------------------------------------------------
    !
    clcoox=clarg(3:)
  elseif(clarg(1:5) == '-diff') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de coordonnée Y.
    ! -------------------------------------------------
    !
    lgdiff=.true.
  elseif(clarg(1:2) == '-y') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de coordonnée Y.
    ! -------------------------------------------------
    !
    clcooy=clarg(3:)
  elseif(clarg(1:2) == '-d') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de nom de fichier de sortie.
    ! -------------------------------------------------
    !
    clficsdoc=clarg(3:)
    lluserdoc=.true.
  elseif(clarg(1:2) == '-c') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de facteur multiplicatif.
    ! -------------------------------------------------
    !
    read(clarg(3:),fmt=*) zcoef
  elseif(clarg(1:2) == '-a') then
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit son choix de facteur additif.
    ! -------------------------------------------------
    !
    read(clarg(3:),fmt=*) zadd
  elseif(clna == ' ' .and. .not.llexist) then
    !
    ! -------------------------------------------------
    ! On n'a pas encore saisi le nom d'article à lire.
    ! -------------------------------------------------
    !
    clna=clarg
  else
    !
    ! -------------------------------------------------
    ! L'utilisateur fournit un nom de fichier LFA.
    ! -------------------------------------------------
    !
    inficlfa=inficlfa+1
    clficlfa(inficlfa)=clarg
  endif
enddo
if(clna /= ' ') then
  !
  !-------------------------------------------------
  ! Si clna est non blanc, c'est que l'utilisateur
  ! a fourni un nom d'article. Il veut donc un article spécifique.
  !-------------------------------------------------
  !
  llveut_art=.true.
  write(*,'(9a)') ' '
  write(*,'(9a)') 'mevol/temporal evolution of ',trim(clna),' written on files:'
else
  !
  !-------------------------------------------------
  ! Si clna est blanc, c'est que l'utilisateur
  ! n'a pas fourni de nom d'article.
  ! On suppose qu'il veut seulement une image composite.
  !-------------------------------------------------
  !
  llveut_art=.false.
  llcompo=.true.
  clna='PAPRSF'
endif
do jficlfa=1,inficlfa
  !
  ! -------------------------------------------------
  ! Nom des fichiers de sortie.
  ! -------------------------------------------------
  !
  if(.not.lluserdta) then
    !
    !-------------------------------------------------
    ! Le nom du fichier de sortie n'est pas fourni par l'utilisateur.
    !-------------------------------------------------
    !
    clficsdta=trim(clna)//clsuffdta(1:len_trim(clsuffdta))
  endif
  if(.not.lluserdoc) then
    !
    !-------------------------------------------------
    ! Le nom du fichier de sortie n'est pas fourni par l'utilisateur.
    !-------------------------------------------------
    !
    if(lluserdta) then
      if(clficsdta(len_trim(clficsdta)-3:len_trim(clficsdta)) == '.dta') then
        clficsdoc=clficsdta(1:len_trim(clficsdta)-4)//'.doc'
      elseif(clficsdta(len_trim(clficsdta)-4:len_trim(clficsdta)) == '.evol') then
        clficsdoc=clficsdta(1:len_trim(clficsdta)-5)//'.doc'
      else
        clficsdoc=clficsdta(1:len_trim(clficsdta))//'.doc'
      endif
    else
      clficsdoc=trim(clna)//clsuffdoc(1:len_trim(clsuffdoc))
    endif
  endif
  if(.not.llopen) then
    !
    !-------------------------------------------------
    ! Ouverture du fichier de données.
    !-------------------------------------------------
    !
    iuls=77
    open(iuls,file=clficsdta,form='formatted')
    llopen=.true.
    !
    !-------------------------------------------------
    ! Ouverture du fichier de documentation: titre, unités, etc...
    !-------------------------------------------------
    !
    iuldoc=78
    open(iuldoc,file=clficsdoc,form='formatted')
  endif
  !
  ! Nombre de fichiers LFA à ouvrir.
  !
  if(inficlfa > 1) then
    !
    ! Au moins 2 fichiers LFA à ouvrir.
    ! Il faut écrire sur les fichiers
    ! de sortie la coordonnée temporelle.
    !
    llxcoo=.true.
  endif
  if(cluni == clindef) then
    !
    !-------------------------------------------------
    ! L'unité de temps n'a pas été imposée par l''utilisateur.
    ! On la déduit de la durée totale du run.
    !-------------------------------------------------
    call choisit_unite(iarg,cluni)
  endif
  !
  ! -------------------------------------------------
  ! On va ouvrir ce fichier LFA.
  ! -------------------------------------------------
  !
  iule=56
  call lfaouv(iule,clficlfa(jficlfa),'R')
  iranf=iranf+1
  call niveaux(iule,ilev)
  !
  !-------------------------------------------------
  ! Indice expérience.
  !-------------------------------------------------
  !
  call indice_en_clair(clficlfa(jficlfa),iule,clindice)
  if(jficlfa == 1) then
    write(*,fmt=*) ' '
    write(*,fmt=*) 'mevol: '
    write(*,'(9a)') '  MNEMONIC          :    ',trim(clindice)
    if(llcompo) then
      write(*,'(9a)') '  COMPOSITE IMAGE'
    elseif(clna /= ' ') then
      write(*,'(9a)') '  ARTICLE           :    ',trim(clna)
    endif
  endif
  !
  !-------------------------------------------------
  ! Coordonnée verticale:
  ! zz est une altitude par rapport au niveau de la mer.
  ! zelevation est une élévation au-dessus de la surface.
  !-------------------------------------------------
  !
  if(.not.allocated(zcooz)) allocate(zcooz(ilev))
  call coov('HEI',ilev,iule,zcooz)
  do jlev=1,ilev
    zz(jlev)=zcooz(jlev)*1000.
  enddo
  zzsurface=zz(ilev)-0.5*(zz(ilev-1)-zz(ilev))
  do jlev=1,ilev
    zelevation(jlev)=zz(jlev)-zzsurface
  enddo
  !
  ! -------------------------------------------------
  ! La date est-elle donnée par KINDAT ou NINDAT?
  ! -------------------------------------------------
  !
  call lfacas(iule,'KINDAT',cltype,ilong,ierr)
  if(ierr == 0) then
    llkindat=.true.
  else
    llkindat=.false.
  endif
  !
  ! -------------------------------------------------
  ! Dimension de l'article souhaité.
  ! -------------------------------------------------
  !
  call lfacas(iule,clna,cltype_souhaite,ilongart,ierr)
  if(ierr /= 0) then
    !
    ! L'article n'existe pas.
    ! On provoque l'arrêt par lecture
    ! de cet article, afin que l'utilisateur
    ! ait le message d'erreur explicite
    ! du logiciel LFA.
    !
    !itaille=1
    !call lfaleci(iule,clna,itaille,itab,ilong,ierr)
    write(*,fmt=*)
    write(*,fmt=*) 'mevol/ERROR: article ',trim(clna),' does not exist in file ',trim(clficlfa(jficlfa)),'!...'
    write(*,fmt=*)
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Dans le cas d'une coordonnée temps "LST",
  ! il faut lire la longitude du lieu.
  !-------------------------------------------------
  !
  if(clcoox(1:len_trim(clcoox)) == 'LST'.and.iranf == 1) then
    call lfacas(iule,'LONGITUDE',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'LONGITUDE',1,ztab,ilong,ierr)
      zlon=ztab(1)
    else
      call lfacas(iule,'PGELAM',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PGELAM',1,ztab,ilong,ierr)
        zlon=ztab(1)
      else
        print*,'MEVOL/WARNING: longitude information is not in the file!...'
        print*,'  zlon taken arbitrarily equal to zero!...'
        zlon=0.
      endif
    endif
  endif
  !
  !-------------------------------------------------
  ! Image composite.
  !-------------------------------------------------
  !
  if(llcompo) then
    if(.not.allocated (zneb)) then
      allocate(zneb(inficlfa,ilev))
      allocate(zccb(inficlfa,ilev))
      allocate(zaut(inficlfa,ilev))
      allocate(zegn(inficlfa,ilev))
      allocate(zfm(inficlfa,ilev))
      allocate(inlabPOST_FORC(inficlfa,ilev))
      allocate(zqc(inficlfa,ilev))
      allocate(zcape(inficlfa))
      allocate(zcin(inficlfa))
      allocate(zbuoy(inficlfa,ilev))
      allocate(zhcla(inficlfa))
      allocate(ztemps(inficlfa))
      allocate(z1dloc_full(ilev))
      allocate(z1dloc_half(ilev+1))

      allocate(zpcl(inficlfa,ilev+1))
      allocate(zpcn(inficlfa,ilev+1))
      allocate(zpsl(inficlfa,ilev+1))
      allocate(zpsn(inficlfa,ilev+1))
      zpcl=0.
      zpcn=0.
      zpsl=0.
      zpsn=0.
      allocate(itypeprec(inficlfa,ilev+1))

      allocate(zpfplcl(ilev+1))
      allocate(zpfplcn(ilev+1))
      allocate(zpfplsl(ilev+1))
      allocate(zpfplsn(ilev+1))
    endif
    !
    !-------------------------------------------------
    ! Nébulosité.
    !-------------------------------------------------
    !
    call lfacas(iule,'PNEB',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'PNEB',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=0.
    endif
    zneb(jficlfa,:)=z1dloc_full(:)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'CCB',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'CCB',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=zindef
    endif
    zccb(jficlfa,:)=z1dloc_full(:)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'AUT',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'AUT',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=zindef
    endif
    zaut(jficlfa,:)=z1dloc_full(:)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'EGN',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'EGN',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=zindef
    endif
    zegn(jficlfa,:)=z1dloc_full(:)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'FLUX_MASSE_CV_UD',cltype,ilong,ierr)
    if(ierr == 0) then
      ilev1=ilev+1
      call lfalecr(iule,'FLUX_MASSE_CV_UD',ilev1,z1dloc_half,ilong,ierr)
    else
      z1dloc_half=zindef
    endif
    do jlev=1,ilev
      zfm(jficlfa,jlev)=0.5*(z1dloc_half(jlev)+z1dloc_half(jlev+1))
    enddo
    inlabPOST_FORC(jficlfa,:)=iindef
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'LN_POST_AJ',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'LN_POST_AJ',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full(:)=zneb(jficlfa,:)
    endif
    zqc(jficlfa,:)=z1dloc_full(:)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'CAPE',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'CAPE',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=zindef
    endif
    zcape(jficlfa)=z1dloc_full(1)
    !
    !-------------------------------------------------
    ! Statut convectif.
    !-------------------------------------------------
    !
    call lfacas(iule,'CIN',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'CIN',ilev,z1dloc_full,ilong,ierr)
    else
      z1dloc_full=zindef
    endif
    zcin(jficlfa)=z1dloc_full(1)
    !
    !-------------------------------------------------
    ! Flottabilité.
    !-------------------------------------------------
    !
    call lfacas(iule,'W2BUOY',cltype,ilong,ierr)
    if(ierr == 0) then
      ilev1=ilev+1
      call lfalecr(iule,'W2BUOY',ilev1,z1dloc_half,ilong,ierr)
    else
      z1dloc_half=zindef
    endif
    do jlev=1,ilev
      zbuoy(jficlfa,jlev)=0.5*(z1dloc_half(jlev)+z1dloc_half(jlev+1))
    enddo
    !
    !-------------------------------------------------
    ! Hauteur de CLA.
    !-------------------------------------------------
    !
    call lfacas(iule,'PTT0',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'PTT0',jplev,zt,ilong,ierr)
    else
      call lfacas(iule,'PT',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PT',jplev,zt,ilong,ierr)
      else
        write(*,fmt=*) 
        write(*,fmt=*) 'mevol/ERROR: neither PTT0 nor PT articles available in the file!...'
        write(*,fmt=*) 
        call exit(1)
      endif
    endif
    call lfacas(iule,'PQT0',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'PQT0',jplev,zqv,ilong,ierr)
    else
      call lfacas(iule,'PQ',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PQ',jplev,zqv,ilong,ierr)
      else
        write(*,fmt=*) 
        write(*,fmt=*) 'mevol/ERROR: neither PQT0 nor PQ articles available in the file!...'
        write(*,fmt=*) 
        call exit(1)
      endif
    endif
    call coov('PRE',ilev,iule,zp)
    do jlev=1,ilev
      zp(jlev)=-100.*zp(jlev)
      zthetav(jlev)=fth_thetav(zp(jlev),zt(jlev),zqv(jlev))
    enddo
    zhcla(jficlfa)=fth_hcla(ilev,zthetav,zelevation)
    !
    ! Précip..
    !
    idim=ilev+1
    call lfacas(iule,'PFPLSL',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'PFPLSL',idim,zpfplsl,ilong,ierr)
      call lfalecr(iule,'PFPLSN',idim,zpfplsn,ilong,ierr)
    else
      zpfplsl=0.
      zpfplsn=0.
    endif
    call lfacas(iule,'FPCL',cltype,ilong,ierr)
    if(ierr == 0) then
      call lfalecr(iule,'FPCL',idim,zpfplcl,ilong,ierr)
      call lfalecr(iule,'FPCN',idim,zpfplcn,ilong,ierr)
    else
      call lfacas(iule,'PFPLCL',cltype,ilong,ierr)
      if(ierr == 0) then
        call lfalecr(iule,'PFPLCL',idim,zpfplcl,ilong,ierr)
        call lfalecr(iule,'PFPLCN',idim,zpfplcn,ilong,ierr)
      else
        zpfplcl=0.
        zpfplcn=0.
      endif
    endif
    zseuilprec=0.
    do jlev=1,ilong
      !
      ! Type de précipitations.
      !
      zrr_sgs=zpfplcl(jlev)+zpfplcn(jlev)+zpfplcl(min(ilong,jlev+1))+zpfplcn(min(ilong,jlev+1))
      zrr_resol=zpfplsl(jlev)+zpfplsn(jlev)+zpfplsl(min(ilong,jlev+1))+zpfplsn(min(ilong,jlev+1))
      if(zrr_sgs > zseuilprec) then
        itypeprec(jficlfa,jlev)=2 ! type précip. sous-maille.
      elseif(zrr_resol > zseuilprec) then
        itypeprec(jficlfa,jlev)=1 ! type précip. résolue.
      else
        itypeprec(jficlfa,jlev)=0 ! type pas de précip..
      endif
      !
      ! Précip. liquides et neigeuses.
      !
      zpcl(jficlfa,jlev)=zpfplcl(jlev)
      zpcn(jficlfa,jlev)=zpfplcn(jlev)
      zpsl(jficlfa,jlev)=zpfplsl(jlev)
      zpsn(jficlfa,jlev)=zpfplsn(jlev)
    enddo
  endif
  !
  ! -------------------------------------------------
  ! Lecture/écriture.
  ! -------------------------------------------------
  !
  call litecr(iranf,clcoox,clcooy,cluni,cluf,cldateref &
&       ,llkindat,iule,iuldoc,clficsdta,clna,cltype_souhaite,ilongart,iniv &
&        ,iuls,zcoef,zadd,llxcoo,llft,zlon,llfirst,zref_time,zindef,ztempsloc,cllegx)
  if(llcompo) then
    ztemps(jficlfa)=ztempsloc
  endif
  !
  ! -------------------------------------------------
  ! Fermeture du fichier.
  ! -------------------------------------------------
  !
  call lfafer(iule)
  llfirst=.false.
enddo
if(llcompo) then
  !
  !-------------------------------------------------
  ! 1ère image, avec CCB.
  !-------------------------------------------------
  !
  if(zfm(1,1) /= zindef) then
    clfcompo_ccb='mevol.composite_ccb.gif'
    call composite(cllegx,ilev,inficlfa,ztemps,zneb,zccb,inlabPOST_FORC, &
    & zaut,zegn,zfm,zqc,zcape,zcin,zbuoy,zindef,zpcl,&
    & zpcn,zpsl,zpsn,itypeprec &
    & ,clfcompo_ccb,ixcompo,iycompo,zcooz,clindice,zhcla,zprr)
    zccb=zindef
  endif
  !
  !-------------------------------------------------
  ! 2ème image, classique: nébulosité et précipitations.
  !-------------------------------------------------
  !
  zfm=zindef
  call composite(cllegx,ilev,inficlfa,ztemps,zneb,zccb,inlabPOST_FORC, &
  & zaut,zegn,zfm,zqc,zcape,zcin,zbuoy,zindef,zpcl,zpcn,&
  & zpsl,zpsn,itypeprec &
  & ,clfcompo,ixcompo,iycompo,zcooz,clindice,zhcla,zprr)
endif
if(llveut_art) then
  write(*,'(9a)') '  DATAFILE          :    ',clficsdta(1:len_trim(clficsdta))
  write(*,'(9a)') '  AUTODOCUMENTATION :    ',clficsdoc(1:len_trim(clficsdoc))
endif
print*,' '
end
#include"dates.F90"
#include"alea.F90"
#include"mautodoc.F90"
#include"fonctions.F90"
