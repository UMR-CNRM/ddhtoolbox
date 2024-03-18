subroutine choisit_unite(karg,cduni)
! --------------------------------------------------------------
! **** ** Choix d'unite de temps (h ou jours).
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  karg: numéro de l'argument de la ligne de commande contenant le dernier fichier à tracer.
! En sortie:
!  cduni: unité: 'h' ou 'd'.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,j,k)
character*(*) :: cduni
integer(kind=4) :: karg
integer(kind=4) :: iule
character*2 :: cltype
real(kind=8) :: zstati
!
!-------------------------------------------------
! Nom du fichier.
!-------------------------------------------------
!
call getargp(karg,clarg)
!
!-------------------------------------------------
! Ouverture du fichier LFA.
!-------------------------------------------------
!/zsta
iule=68
call lfaouv(iule,clarg,'R')
!
!-------------------------------------------------
! Lecture de la durée du run.
!-------------------------------------------------
!
call lfacas(iule,'RSTATI',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(iule,'RSTATI',1,zstati,ilong,ierr)
else
  write(*,fmt=*) 
  write(*,fmt=*) 'mevol/WARNING: RSTATI not provided in the file. Taken arbitrarily to zero.'
  write(*,fmt=*) 
  zstati=0.
endif
!
!-------------------------------------------------
! Choix de l'unité.
!-------------------------------------------------
!
if(zstati < 259200.) then
  cduni='h'
else
  cduni='d'
endif
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iule)
end
