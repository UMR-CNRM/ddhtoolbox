subroutine pridocf(kdocfi,kdatef,cdnamx,pech,pechdv,kulsor)
! --------------------------------------------------------------------------
! **** *PRIDOCF* Impression du contenu de l'article de doc. DOCFICHIER.
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
!   92-06-05, J.M. Piriou: echeance en secondes lue sur article 'ECHEANCE
!   92-12-04, J.M. Piriou: adaptation au calcul sur CRAY.
! 2008-07-17, J.M. Piriou: translate some outputs to English.
!
! --------------------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: ILNAMX
INTEGER(KIND=4) :: KULSOR
REAL(KIND=8) :: PECH
REAL(KIND=8) :: PECHDV
REAL(KIND=8) :: ZECH
REAL(KIND=8) :: ZECHDV

character*(*) cdnamx
INTEGER(KIND=4) kdocfi(17)
INTEGER(KIND=4) kdatef(11)
ilnamx=len_trim(cdnamx)
write(kulsor,'(3a,i4.4,4(a,i2.2),a,$)') &
& ' Run ',cdnamx(1:ilnamx),', base ',kdatef(1) &
& ,'-',kdatef(2),'-',kdatef(3),' ',kdatef(4),':',kdatef(5),','
!
!-------------------------------------------------
! Echéance totale.
!-------------------------------------------------
!
if(pech < 3600.) then
  !
  ! On fournit l'échéance en minutes jusqu'à une heure.
  !
  zech=pech/60.
  write(kulsor,'(a,f7.2,a,$)')' cum. time range  ',zech,' mn'
elseif(pech < 259200.) then
  !
  ! On fournit l'échéance en heures jusqu'à 3 jours.
  !
  zech=pech/3600.
  write(kulsor,'(a,f7.2,a,$)')' cum. time range  ',zech,' h'
else
  !
  ! On fournit l'échéance en jours au-delà.
  !
  zech=pech/86400.
  write(kulsor,'(a,f7.2,a,$)')' cum. time range  ',zech,' days'
endif
!
!-------------------------------------------------
! Echéance variables.
!-------------------------------------------------
!
if(pechdv /= pech) then
  if(pechdv < 3600.) then
    !
    ! On fournit l'échéance en minutes jusqu'à une heure.
    !
    zechdv=pechdv/60.
    write(kulsor,'(a,f7.2,a)')', time range for var. ',zechdv,' mn'
  elseif(pechdv < 259200.) then
    !
    ! On fournit l'échéance en heures jusqu'à 3 jours.
    !
    zechdv=pechdv/3600.
    write(kulsor,'(a,f7.2,a)')', time range for var. ',zechdv,' h'
  else
    !
    ! On fournit l'échéance en jours au-delà.
    !
    zechdv=pechdv/86400.
    write(kulsor,'(a,f7.2,a)')', time range for var. ',zechdv,' days'
  endif
else
  write(kulsor,'(a)') '.'
endif
!
!-------------------------------------------------
! Type de domaines.
!-------------------------------------------------
!
if(kdocfi(1) == 1) then
  write(kulsor,'(a,$)')' LIMITED AREA DOMAINS'
elseif(kdocfi(1) == 5) then
  write(kulsor,'(a,$)')' GLOBAL DOMAIN'
elseif(kdocfi(1) == 6) then
  write(kulsor,'(a,$)')' ZONAL DOMAINS'
else
  write(kulsor,'(a,$)')' PRIDOCF/ERROR: unknown domain type!...'
endif
write(kulsor,'(i7,a,$)') kdocfi(5),' time steps,'
write(kulsor,'(2(i6,a))') kdocfi(6),' levels, ',kdocfi(15),' domains.'
end
