program asc2lfa
! --------------------------------------------------------------
! **** *asc2lfa* Conversion d'un fichier de DDH ASCII du CEP vers un fichier lfa.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*200 :: CLFE
CHARACTER*200 :: CLFS
CHARACTER*200 :: CLGOL
CHARACTER*200 :: CLNA
INTEGER(KIND=4) :: ILGOL
INTEGER(KIND=4) :: ILNA
INTEGER(KIND=4) :: JB

#include"ddhpar.h"
INTEGER(KIND=4) idatef(11)
INTEGER(KIND=4), parameter :: jpdocfi=17
INTEGER(KIND=4) idocfi(jpdocfi)
!
! Saisie de la ligne de commande.
!
call getargp(1,clfe)
call getargp(2,clfs)
if(clfs == ' ') then
  print*,' '
  print*,'Conversion d''un fichier de DDH ASCII du CEP vers un fichier lfa.'
  print*,' '
  print*,'Utilisation: ddh-asc2lfa fic_ascii fic_lfa'
  print*,' '
  stop
endif
!
! Ouverture du fichier ASCII texte d'entrée.
!
open(1,file=clfe,form='formatted')
!
! Ouverture du fichier lfa de sortie.
!
call lfaouv(2,clfs,'W')
call lfapreci(2,jpprecint)
call lfaprecr(2,jpprecree)
!
! Indice expérience.
!
read(1,fmt='(a)') clgol
ilgol=len_trim(clgol)
call lfaecrc(2,'INDICE EXPERIENCE',clgol,1)
!
! Date.
!
read(1,fmt='(a)') clna
!print*,clna(1:len_trim(clna))
ilna=len_trim(clna)
if(clna(1:ilna) == 'DATE') then
  read(1,fmt=*) (idatef(jb),jb=1,11)
  call lfaecri(2,'DATE',idatef,11)
else
  print*,'ASC2lfa/ERREUR: champ de DATE attendu!...'
  print*,clna
  call exit(1)
endif
!
! DOCFICHIER.
!
read(1,fmt='(a)') clna
!print*,clna(1:len_trim(clna))
ilna=len_trim(clna)
if(clna(1:ilna) == 'DOCFICHIER') then
  read(1,fmt=*) (idocfi(jb),jb=1,jpdocfi)
  call lfaecri(2,'DOCFICHIER',idocfi,jpdocfi)
else
  print*,'ASC2lfa/ERREUR: champ de DOCFICHIER attendu!...'
  print*,clna
  call exit(1)
endif
call lit(idocfi(6),idocfi(15))
close(1)
call lfafer(2)
end
subroutine lit(kniv,kdom)
! --------------------------------------------------------------
! **** *lit* Lecture/écriture séquentielle d'un fichier DDH-ASCII.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   96-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*200 :: CLNA
CHARACTER*200 :: CLNAS
INTEGER(KIND=4) :: ICHAMP
INTEGER(KIND=4) :: IDOM
INTEGER(KIND=4) :: IGOL
INTEGER(KIND=4) :: ILIG
INTEGER(KIND=4) :: ILNA
INTEGER(KIND=4) :: ITOTAL
INTEGER(KIND=4) :: JB
INTEGER(KIND=4) :: JCHAMP
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: JLEV
INTEGER(KIND=4) :: JVAR
INTEGER(KIND=4) :: KDOM
INTEGER(KIND=4) :: KNIV
REAL(KIND=8) :: zech(1)

INTEGER(KIND=4), parameter :: jpchamp=300
REAL(KIND=8) zprof(kniv+1,kdom,jpchamp) ! tableau des profils lus.
REAL(KIND=8) zdocd(11)
character*(80) clchamp(jpchamp) ! nom du champ.
INTEGER(KIND=4) ilevchamp(jpchamp) ! longueur de ce champ.
REAL(KIND=8) ztotal((kniv+1)*kdom) ! tableau atmosphérique pour lfaecrr.
REAL(KIND=8) zsol(kdom) ! tableau sol pour lfaecrr.
idom=0 ! numéro du domaine courant.
ilig=0 ! numéro de ligne dans le fichier ASCII d'entrée.
100  read(1,fmt='(a)',end=200) clna
!print*,clna(1:len_trim(clna))
ilig=ilig+1
ilna=len_trim(clna)
if(clna(1:ilna) == 'DOCD') then
  !
  ! Article de documentation des domaines.
  !
  idom=idom+1
  ichamp=0 ! nombre de champs scientifiques effectivement lus.
  read(1,fmt=*) (zdocd(jb), jb=1,11)
  write(clnas,fmt='(a,i3.3)') 'DOCD',idom
  call lfaecrr(2,clnas,zdocd,11)
elseif(clna(1:ilna) == 'ECHEANCE') then
  !
  ! Article contenant l'échéance du fichier.
  !
  read(1,fmt=*) zech(1)
  call lfaecrr(2,'ECHEANCE',zech,1)
  read(1,fmt=*) igol
else
  !
  ! Le nombre de champs scientifiques de ce domaine
  ! est augmenté de 1.
  !
  ichamp=ichamp+1
  if(ichamp > jpchamp) then
    print*,'ddh-asc2lfa/ERREUR: recompiler avec jpchamp plus grand!...'
    print*,ichamp,jpchamp
    print*,'Ligne ',ilig
    call exit(1)
  endif
  clchamp(ichamp)=clna
  !
  ! Lecture du champ scientifique.
  !
  read(1,fmt=*) ilevchamp(ichamp),(zprof(jlev,idom,ichamp),jlev=1,ilevchamp(ichamp))
endif
goto 100
200  continue
!
! A ce stade on a lu tout le fichier ASCII d'entrée,
! et les articles scientifiques sont sur le tableau
! gigogne zprof. On va copier ce tableau
! sur le fichier lfa de sortie.
!
do jchamp=1,ichamp
  clna=clchamp(jchamp) ! nom du champ.
  ilna=len_trim(clna)
  if(clna(1:ilna) == 'S V0') then
    !
    ! Champ sol initial.
    !
    do jvar=1,ilevchamp(jchamp) ! ilevchamp(jchamp) est le nb de variables initiales au sol.
      do jdom=1,kdom
        zsol(jdom)=zprof(jvar,jdom,jchamp)
      enddo
      write(clnas,fmt='(a,i2.2,a)') 'S',jvar,'_0'
         call lfaecrr(2,clnas,zsol,kdom)
    enddo
  elseif(clna(1:ilna) == 'S V1') then
    !
    ! Champ sol final.
    !
    do jvar=1,ilevchamp(jchamp) ! ilevchamp(jchamp) est le nb de variables finales au sol.
      do jdom=1,kdom
        zsol(jdom)=zprof(jvar,jdom,jchamp)
      enddo
      write(clnas,fmt='(a,i2.2,a)') 'S',jvar,'_1'
         call lfaecrr(2,clnas,zsol,kdom)
    enddo
  elseif(clna(1:ilna) == 'S FVRAC') then
    !
    ! Champ sol de flux.
    !
    do jvar=1,ilevchamp(jchamp) ! ilevchamp(jchamp) est le nb de flux au sol.
      do jdom=1,kdom
        zsol(jdom)=zprof(jvar,jdom,jchamp)
      enddo
      write(clnas,fmt='(a,i2.2)') 'G',jvar
         call lfaecrr(2,clnas,zsol,kdom)
    enddo
  else
    !
    ! Cas général: champ scientifique non sol.
    !
    itotal=0 ! indice du tableau 1d ztotal.
    do jdom=1,kdom
      do jlev=1,ilevchamp(jchamp)
        itotal=itotal+1
        ztotal(itotal)=zprof(jlev,jdom,jchamp)
      enddo
    enddo
       call lfaecrr(2,clna(1:ilna),ztotal,itotal)
  endif
enddo
end
