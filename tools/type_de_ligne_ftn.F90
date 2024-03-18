subroutine typlf(cdc,ktype)
! --------------------------------------------------------------------------
! **** *TYPLF* Type de ligne fortran.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   94-12, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! cdc chaine-ligne fortran.
! En sortie:
! ktype = 0 si ligne vide.
! 1 si ligne de commentaire.
! 2 si ligne executable.
! 4 si ligne de directive au preprocesseur cpp.
! --------------------------------------------------------------------------
character*(*) cdc
character*1 clc
if(cdc == ' ') then
  ktype=0
elseif(cdc(1:1) == '#'.or.cdc(1:4) == 'CDIR'.or.cdc(1:4) == 'cdir') then
  !
  ! Ligne de directive au preprocesseur cpp ou au compilateur.
  !
  ktype=4
else
  do jc=1,len(cdc)
    clc=cdc(jc:jc)
    if(clc == ' ') then
    elseif(clc == '	') then
    elseif(clc == '!') then
      !
      ! Ligne de commentaire.F90.
      !
      ktype=1
      goto 100
    else
      !
      ! Ligne executable f77 ou.F90.
      !
      ktype=2
      goto 100
    endif
  enddo
  !
  ! Si on est parvenu ici, c'est que la ligne ne comportait
  ! que des blancs et des tabulations.
  !
  ktype=1
endif
  100 continue
end
