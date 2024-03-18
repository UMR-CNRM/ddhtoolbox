#ifdef VPP
@OPTIONS NODOUBLE
#endif
program ddhif
! --------------------------------------------------------------
! **** *ddhif* Création d'un fichier de DDH à partir de deux: les variables initiales de l'un, les finales de l'autre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
CHARACTER*200 :: CLECH
CHARACTER*200 :: CLFE1
CHARACTER*200 :: CLFE2
CHARACTER*200 :: CLFS
CHARACTER*200 :: CLNA
INTEGER(KIND=4) :: IDOM
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: ILEV
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: IPROD
INTEGER(KIND=4) :: IULE1
INTEGER(KIND=4) :: IULE2
INTEGER(KIND=4) :: IULS
INTEGER(KIND=4) :: JDOM
REAL(KIND=8) :: zech(1)
REAL(KIND=8) :: zech_scal
character(len=2) :: cltype
character(len=200) :: clnum
logical :: llcp

#include"ddhpar.h"
INTEGER(KIND=4) idocfi(17)
!
! Saisie de la ligne de commande.
!
call getargp(1,clfe1)
call getargp(2,clfe2)
call getargp(3,clech)
call getargp(4,clfs)
if(clfs == ' ') then
  print*,' '
  print*,'Création d''un fichier de DDH à partir de deux:'
  print*,'les variables initiales de l''un, les finales de l''autre.'
  print*,' '
  print*,'Utilisation: ddhif f1 f2 ech fres'
  print*,' '
  print*,'fres aura pour variables initiales celles de f1.'
  print*,'                         finales celles initiales de f2.'
  print*,'               échéance variables et flux ech (à fournir en jours).'
  stop
endif
read(clech,fmt='(f10.0)') zech(1)
zech(1)=86400.*zech(1) ! on convertit les jours en secondes.
!
! Ouverture des fichiers.
!
iule1=72
iule2=74
iuls=73
call lfaouv(iule1,clfe1,'R')
call lfaouv(iule2,clfe2,'R')
call lfaouv(iuls,clfs,'W')
call lfapreci(iuls,jpprecint)
call lfaprecr(iuls,jpprecree)
!
! Autodocumentation.
!
call lfacop(iule1,'INDICE EXPERIENCE',' ',iuls)
call lfacop(iule1,'DATE',' ',iuls)
call lfacop(iule1,'DOCFICHIER',' ',iuls)
call lfaecrr(iuls,'ECHEANCE',zech,1)
call lfaecrr(iuls,'ECHEANCEDV',zech,1)
call lfaleci(iule1,'DOCFICHIER',17,idocfi,ilong,ierr)
idom=idocfi(15)
ilev=idocfi(6)
do jdom=1,idom
  if(jdom < 1000) then
    write(clna,fmt='(a,i3.3)') 'DOCD',jdom
  else
    write(clnum,fmt=*) jdom
    write(clna,fmt='(2a)') 'DOCD',trim(adjustl(clnum))
  endif
  call lfacop(iule1,clna,' ',iuls)
enddo
!
! Variables initiales.
!
call lfacop(iule1,'VPP0',' ',iuls)
call lfacop(iule1,'VQV0',' ',iuls)
call lfacop(iule1,'VUU0',' ',iuls)
call lfacop(iule1,'VVV0',' ',iuls)
call lfacop(iule1,'VKK0',' ',iuls)
call lfacop(iule1,'VCT0',' ',iuls)
call lfacop(iule1,'VEP0',' ',iuls)
call lfacop(iule1,'VHR0',' ',iuls)
call lfacop(iule1,'VQL0',' ',iuls)
call lfacop(iule1,'VQN0',' ',iuls)
call lfacop(iule1,'VNT0',' ',iuls)
call lfacop(iule1,'VOM0',' ',iuls)

call lfacas(iule1,'VCP0',cltype,ilong,ierr)
if(ierr == 0) then
  llcp=.true.
  call lfacop(iule1,'VCP0',' ',iuls)
  call lfacop(iule1,'VCZ0',' ',iuls)
else
  llcp=.false.
endif
!
! Variables finales.
!
call lfacop(iule2,'VPP0','VPP1',iuls)
call lfacop(iule2,'VQV0','VQV1',iuls)
call lfacop(iule2,'VUU0','VUU1',iuls)
call lfacop(iule2,'VVV0','VVV1',iuls)
call lfacop(iule2,'VKK0','VKK1',iuls)
call lfacop(iule2,'VCT0','VCT1',iuls)
call lfacop(iule2,'VEP0','VEP1',iuls)
call lfacop(iule2,'VHR0','VHR1',iuls)
call lfacop(iule2,'VQL0','VQL1',iuls)
call lfacop(iule2,'VQN0','VQN1',iuls)
call lfacop(iule2,'VNT0','VNT1',iuls)
call lfacop(iule2,'VOM0','VOM1',iuls)

if(llcp) then
  call lfacop(iule2,'VCZ0','VCZ1',iuls)
  call lfacop(iule2,'VCP0','VCP1',iuls)
endif
!
! Masse cumulée.
!
iprod=idom*ilev
zech_scal=zech(1)
call copmc(iule1,iule2,iuls,iprod,zech_scal)
!
! Fermeture des fichiers.
!
call lfafer(iule1)
call lfafer(iule2)
call lfafer(iuls)
end
subroutine copmc(kule1,kule2,kuls,kprod,pech)
! --------------------------------------------------------------
! **** *copmc* Copie de l'article 'PPP' créé à partir des deux fichiers d'entrée.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   97-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: IERR
INTEGER(KIND=4) :: ILONG
INTEGER(KIND=4) :: JPROD
INTEGER(KIND=4) :: KPROD
INTEGER(KIND=4) :: KULE1
INTEGER(KIND=4) :: KULE2
INTEGER(KIND=4) :: KULS
REAL(KIND=8) :: PECH

REAL(KIND=8) zvpp01(kprod)
REAL(KIND=8) zvpp02(kprod)
REAL(KIND=8) zppp(kprod)
!
! Lecture de VPP0 du 1er fichier.
!
call lfalecr(kule1,'VPP0',kprod,zvpp01,ilong,ierr)
!
! Lecture de VPP0 du 2e fichier.
!
call lfalecr(kule2,'VPP0',kprod,zvpp02,ilong,ierr)
!
! Calcul de la moyenne des deux fois l'échéance.
!
do jprod=1,kprod
  zppp(jprod)=0.5*(zvpp01(jprod)+zvpp02(jprod))*pech
enddo
!
! Ecriture sur le fichier de sortie.
!
call lfaecrr(kuls,'PPP',zppp,kprod)
end

