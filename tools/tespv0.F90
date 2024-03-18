subroutine tespv0(ptab,klong,ldprev0)
! --------------------------------------------------------------
! **** ** TESte la Présence de Valeurs nulles dans un tableau.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2021-03-25, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   ptab(klon) : tableau à tester.
! En sortie:
!   ldprev0 : vrai si au moins une valeur nulle dansle tableau, faux sinon.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)

real(kind=8), intent(in) :: ptab(klong)
integer(kind=4), intent(in) :: klong
logical, intent(out) :: ldprev0
ldprev0=.false.
do jlong=1,klong
  if(ptab(jlong) == 0.) ldprev0=.true.
enddo
end
