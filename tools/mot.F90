program mot
! --------------------------------------------------------------
! **** *MOT* Fournit le n-ième mot d'une chaîne.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*2000 :: CLC
CHARACTER*2000 :: CLCRIM
CHARACTER*2000 :: CLNUM
CHARACTER*2000 :: CLRES
INTEGER(KIND=4) :: IARG
INTEGER(KIND=4) :: ICRIM
INTEGER(KIND=4) :: ILMOT
INTEGER(KIND=4) :: ILRES
INTEGER(KIND=4) :: INUM
INTEGER(KIND=4) :: ierr
LOGICAL :: LLDROITE,lldemnm

character*160 clmot(180)
iarg=iargc() ! nombre d'arguments.
if(iarg /= 3) then
  print*,' '
  print*,'Fourniture du n-ième mot d''une chaîne de caractères.'
  print*,' '
  print*,'Utilisation: mot crim n chaîne'
  print*,' '
  print*,'	avec crim critère de mot:'
  print*,'		1 si mot=suite continue de [non (blancs ou tabulations)].'
  print*,'		  (NON BLANCS)'
  print*,'		2 si mot=suite continue de [a-z,A-Z,0-9,.,_].'
  print*,'		  (LETTRES OU NOMBRES)'
  print*,'		3 si mot=suite continue de [0-9,E,e,+,-,.].'
  print*,'		  (NOMBRES)'
  print*,'		4 si mot=suite continue de [a-z,A-Z,0-9].'
  print*,'		  (LETTRES OU NOMBRES ENTIERS)'
  print*,'		5 si mot=suite continue de [a-z,A-Z,0-9,_,-].'
  print*,'		  (NOMS AVEC SOULIGNES OU TIRETS)'
  print*,'		6 si mot=suite continue de [non (- ou . ou blancs ou tabulations)].'
  print*,'		7 si mot=suite continue de [non .].'
  print*,'		8 si mot=suite continue de [0-9].'
  print*,'		  (NOMBRES ENTIERS)'
  print*,'		9 si mot=suite continue de [non /].'
  print*,'		  (EX: REPERTOIRES UNIX)'
  print*,'		10 si mot=suite continue de [non ,].'
  print*,'		< 0 si mot=suite continue de [non CHAR(-kcrim)].'
  print*,'	avec n rang du mot dans la chaîne chaîne'
  print*,'		n > 0: comptage à partir de la gauche de la chaîne.'
  print*,'		n < 0: comptage à partir de la droite.'
  print*,'		n = .: demande du nombre de mots de cette chaîne.'
  print*,'Exemples:'
  print*,'	mot 3 3 ''8_4 5.6 8_7 toto'' retournera ''5.6'' sur output standard.'
  print*,'	mot 5 3 ''8_4 5.6 8_7 toto'' retournera ''6'' sur output standard.'
  print*,'	mot 7 1 ''8_4 5.6 8_7 toto'' retournera ''8_4 5'' sur output standard.'
  print*,'	mot 7 -1 ''8_4 5.6 8_7 toto'' retournera ''6 8_7 toto'' sur output standard.'
  print*,'	mot 9 3 ''/usr/bin/dev/toto'' retournera ''dev'' sur output standard.'
  print*,'	mot 9 . ''/usr/bin/dev/toto'' retournera 004 sur output standard.'
  stop
endif
ierr=0
call getarg(1,clcrim)
call getarg(2,clnum)
call getarg(3,clc)
if(trim(clnum) == '.') then
  lldemnm=.true. ! ldemnm=T si l'utilisateur demande le nombre de mots, F sinon.
else
  lldemnm=.false.
  read(clnum,fmt=*) inum
endif
read(clcrim,fmt=*) icrim
call casc(clc,icrim,clmot,ilmot)
if(lldemnm) then
  write(*,fmt='(i3.3)') ilmot
  stop
endif
if(inum < 0) then
  !
  ! Comptage à partir de la droite.
  !
  lldroite=.true.
  inum=-inum
else
  !
  ! Comptage à partir de la gauche.
  !
  lldroite=.false.
endif
if(inum < 1.or.inum > ilmot) then
  !
  ! Le rang du mot fourni par l'utilisateur est absurde.
  !
  !print*,'MOT/ERREUR: le rang du mot demandé est inexistant!...'
  !print*,inum,ilmot
  !call exit(1)
  clres='ERREUR:rang_inexistant'
  ierr=1
else
  !
  ! Le rang du mot fourni par l'utilisateur est correct.
  !
  if(lldroite) then
    !
    ! Comptage à partir de la droite.
    !
    clres=clmot(ilmot-inum+1)
  else
    !
    ! Comptage à partir de la gauche.
    !
    clres=clmot(inum)
  endif
endif
write(*,fmt='(a)') trim(clres)
if(ierr == 1) then
  write(*,fmt=*)
  write(*,fmt=*) 'mot/ERREUR: ',trim(clres),'!...'
  write(*,fmt=*)
  call exit(1)
endif
end
