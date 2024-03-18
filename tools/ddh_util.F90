!OPTIONS NODOUBLE
subroutine get_budget_package(kul,cdbp)
! --------------------------------------------------------------------------
! **** Get the budget package (BP) version of current DDH file, from its autodocumentation.
! --------------------------------------------------------------------------
! -------
! Author:       2008-07-10, J.M. Piriou.
! -------
! Modifications:   
! -------
! Input:
!	kul: logical unit of DDH file.
! -------
! Output:
!	cdbp: budget package of DDH file.
! --------------------------------------------------------------------------
implicit none
!
!-------------------------------------------------
! Dummy variables.
!-------------------------------------------------
!
INTEGER(KIND=4), intent(in) :: kul
CHARACTER(len=*), intent(out) :: cdbp
!
!-------------------------------------------------
! Local variables.
!-------------------------------------------------
!
INTEGER(KIND=4) :: ilong
INTEGER(KIND=4) :: ierr_FQTPRECICOL
INTEGER(KIND=4) :: ierr_FQVPRECICOL
INTEGER(KIND=4) :: ierr_FQRPLS
INTEGER(KIND=4) :: ierr_FCRSEDIM
INTEGER(KIND=4) :: ierr_FQRPL
INTEGER(KIND=4) :: ierr_FQVEVPL
REAL(KIND=8) :: ZCOEFDDHI
CHARACTER(len=200) :: cltype
!
!-------------------------------------------------
! .
!-------------------------------------------------
!
call lfacas(kul,'FQTPRECICOL',cltype,ilong,ierr_FQTPRECICOL)
call lfacas(kul,'FQRPLS',cltype,ilong,ierr_FQRPLS)
call lfacas(kul,'FCRSEDIM',cltype,ilong,ierr_FCRSEDIM)
call lfacas(kul,'FQVPRECICOL',cltype,ilong,ierr_FQVPRECICOL)
call lfacas(kul,'FQRPL',cltype,ilong,ierr_FQRPL)
call lfacas(kul,'FQVEVPL',cltype,ilong,ierr_FQVEVPL)
if(ierr_FQTPRECICOL == 0 .and. ierr_FQVEVPL == 0) then
  !
  !-------------------------------------------------
  ! The article FQTPRECICOL exists in the DDH file.
  !-------------------------------------------------
  !
  cdbp='BP_GMAP_2008_07'
elseif(ierr_FQTPRECICOL == 0 .and. ierr_FQVEVPL /= 0) then
  !
  !-------------------------------------------------
  ! The article FQTPRECICOL exists in the DDH file, but FQVEVPL does not exist.
  ! Case of IFS runs 2018.
  !-------------------------------------------------
  !
  cdbp='BP_IFS_2018'
elseif(ierr_FQRPLS == 0) then 
  cdbp='BP_ALARO_2008_07'
elseif(ierr_FCRSEDIM == 0) then 
  cdbp='BP_PCMT_2012_01'
elseif(ierr_FQVPRECICOL == 0) then 
  cdbp='BP_oper2012_lflexdia'
elseif(ierr_FQRPL == 0) then 
  cdbp='BP_oper2014_lflexdia'
else
  write(*,fmt=*) 
  write(*,fmt=*) 'DDH/get_budget_package/ERROR: budget package not recognized!...'
  write(*,fmt=*) 
  call exit(1)
endif
end
