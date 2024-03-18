!OPTIONS NODOUBLE
program ddhmech
! --------------------------------------------------------------
! **** *DDHMECH* Modifie l'échéance d'un fichier de DDH.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*200 :: CLECH
CHARACTER*200 :: CLFE
CHARACTER*200 :: CLFS
CHARACTER*200 :: CLNA
CHARACTER*200 :: CLTYPE
INTEGER(KIND=4) :: IARG
INTEGER(KIND=4) :: IARGCP
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: ILNA
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: IULE
INTEGER(KIND=4) :: IULS
INTEGER(KIND=4) :: JLONG
REAL(KIND=8) :: zeche(1)
REAL(KIND=8) :: zechs(1)
REAL(KIND=8) :: ZRAPP

#include"ddhpar.h"
INTEGER(KIND=4) idatef(11) ! documentation sur les dates.
INTEGER(KIND=4) idocfi(17) ! documentation sur les dimensions des champs diag.
REAL(KIND=8) zprod(jpprod) ! tableau recevant un champ de DDH.
REAL(KIND=8) zechvar(1)
character*200 clechvar
logical llechvar
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 3 .and. iarg /= 4) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Modification de l''échéance d''un fichier de DDH.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: ddhmech ddh_ent ech [echvar] ddh_sor'
  write(*,'(a)') 'avec'
  write(*,'(a)') '	ddh_ent fichier DDH d''entrée'
  write(*,'(a)') '	ech échéance-flux du fichier de sortie (jours)'
  write(*,'(a)') '	echvar échéance-variables du fichier de sortie (jours) (argument facultatif)'
  write(*,'(a)') '	ddh_sor fichier DDH de sortie'
  write(*,'(a)') ' '
  stop
elseif(iarg == 3) then
  !
  !-------------------------------------------------
  ! Nombre d'arguments OK.
  !-------------------------------------------------
  !
  call getargp(1,clfe)
  call getargp(2,clech)
  read(clech,fmt=*) zechs(1)
  zechs(1)=86400.*zechs(1)
  llechvar=.false.
  call getargp(3,clfs)
elseif(iarg == 4) then
  !
  !-------------------------------------------------
  ! Nombre d'arguments OK.
  !-------------------------------------------------
  !
  call getargp(1,clfe)
  !
  call getargp(2,clech)
  read(clech,fmt=*) zechs(1)
  zechs(1)=86400.*zechs(1)
  !
  call getargp(3,clechvar)
  read(clechvar,fmt=*) zechvar(1)
  zechvar(1)=86400.*zechvar(1)
  llechvar=.true.
  !
  call getargp(4,clfs)
else
  write(*,fmt=*) 
  write(*,fmt=*) 'ddhmech/ERREUR interne iarg: !...'
  write(*,fmt=*) iarg
  call exit(1)
endif
!
! Ouverture des fichiers.
!
iule=72
iuls=73
call lfaouv(iule,clfe,'R')
call lfaouv(iuls,clfs,'W')
call lfapreci(iuls,jpprecint)
call lfaprecr(iuls,jpprecree)
call lfalecr(iule,'ECHEANCE',1,zeche,ilong,ierr)
!
!-------------------------------------------------
! Ratio à appliquer à tous les champs extensifs dans le temps.
!-------------------------------------------------
!
zrapp=zechs(1)/zeche(1)
!
!-------------------------------------------------
! On rebobine le fichier DDH d'entrée pour le lire
! séquentiellement.
!-------------------------------------------------
!
call lfarew(iule)
100	continue
!
! Renseignements sur l'article suivant du fichier.
!
clna=' '
call lfacas(iule,clna,cltype,ilong,ierr)
ilna=len_trim(clna)
if(ierr == 0) then
  !
  ! Copie de l'article clnom du fichier iule
  ! au fichier iuls.
  !
  if(clna(1:ilna) == 'DATE') then
    !
    !-------------------------------------------------
    ! Date.
    !-------------------------------------------------
    !
    call lfaleci(iule,clna,11,idatef,ilong,ierr)
    idatef(7)=nint(zrapp*float(idatef(7)))
    idatef(10)=nint(zrapp*float(idatef(10)))
    call lfaecri(iuls,clna,idatef,ilong)
  elseif(clna(1:ilna) == 'DOCFICHIER') then
    !
    !-------------------------------------------------
    ! Dimensions.
    !-------------------------------------------------
    !
    call lfaleci(iule,clna,17,idocfi,ilong,ierr)
    idocfi(5)=nint(zrapp*float(idocfi(5)))
    call lfaecri(iuls,clna,idocfi,ilong)
  elseif(llechvar .and. clna(1:ilna) == 'ECHEANCEDV') then
  elseif(clna(1:ilna) == 'ECHEANCE') then
    !
    !-------------------------------------------------
    ! Echéance.
    !-------------------------------------------------
    !
    call lfalecr(iule,clna,1,zeche,ilong,ierr)
    call lfaecrr(iuls,'ECHEANCE',zechs,1)
    if(llechvar) call lfaecrr(iuls,'ECHEANCEDV',zechvar,1)
  elseif(clna(1:1) == 'T'.or.clna(1:1) == 'P'.or.clna(1:1) == 'F'.or.clna(1:1) == 'G') then
    !
    !-------------------------------------------------
    ! Grandeur extensive dans le temps.
    ! On lui applique le ratio.
    !-------------------------------------------------
    !
    call lfalecr(iule,clna,jpprod,zprod,ilong,ierr)
    do jlong=1,ilong
      zprod(jlong)=zprod(jlong)*zrapp
    enddo
    call lfaecrr(iuls,clna,zprod,ilong)
  else
    !
    !-------------------------------------------------
    ! Autres articles de DDH. Simple recopie.
    !-------------------------------------------------
    !
    call lfacop(iule,clna,clna,iuls)
  endif
  goto 100
endif
!
! Fermeture des fichiers.
!
call lfafer(iule)
call lfafer(iuls)
end
