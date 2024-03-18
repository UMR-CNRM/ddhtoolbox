subroutine cooz(kule,klev,paphi,paphif)
! --------------------------------------------------------------
! **** ** Fourniture d'une coordonnée Y géopotentiel, lue sur fichier M1D LFA.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!  kule: unité logique du fichier LFA. Ce fichier doit déjà être ouvert.
!  klev: nombre de niveaux de variables sur la verticale.
! En sortie:
!  paphi: géopotentiel niveaux flux.
!  paphif: géopotentiel niveau variables.
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
real(kind=8) :: ztmp(klev+1)
real(kind=8) :: zvah(klev+1)
real(kind=8) :: zvbh(klev+1)
real(kind=8) :: paphi(klev+1)
real(kind=8) :: paphif(klev)
real(kind=8) :: zt(klev)
real(kind=8) :: zorog(1)
ilev1=klev+1
!
!-------------------------------------------------
! Coordonnée hauteur.
! On va regarder quelle est la coordoonée
! adaptée au champ à tracer disponible
! dans le fichier; on va choisir
! entre les articles PAPHI, PAPHIF, VAH
! et VBH.
!-------------------------------------------------
!
call lfacas(kule,'PAPHIF',cltype,ilong,ierr)
if(ierr == 0) then
  !
  !-------------------------------------------------
  ! L'article PAPHIF existe.
  !-------------------------------------------------
  !
  call lfalecr(kule,'PAPHIF',klev,paphif,ilong,ierr)
  call lfalecr(kule,'PAPHI',ilev1,paphi,ilong,ierr)
else
  !
  !-------------------------------------------------
  ! L'article PAPHI n'existe pas.
  ! Il faut donc le déduire de la pression et
  ! de la température.
  !-------------------------------------------------
  !
  !
  ! Calcul de p(a,b,vp00).
  !
  idim=klev+1
  call lfalecr(kule,'VAH',idim,zvah,ilong,ierr)
  call lfalecr(kule,'VBH',idim,zvbh,ilong,ierr)
  ilevflux=ilong
  ilevvar=ilevflux-1
  zpspt0=101325.
  call calpaprs(ilong,zvah,zvbh,zpspt0,ztmp)
  !
  ! Lecture de la température.
  !
  call lfalecr(kule,'PTT0',klev,zt,ilt,ierr)
  !
  ! Calcul de phi(p,t).
  !
  call phidept(ilevvar,ztmp,zt,paphi,paphif)
  !
  !-------------------------------------------------
  ! Altitude: on ajoute l'altitude de la surface.
  !-------------------------------------------------
  !
  call lfacas(kule,'POROG',cltype,ilong_orog,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'POROG',1,zorog,ilong_orog,ierr)
  else
    zorog(1)=0.
  endif
  paphi=paphi+zorog(1)
  paphif=paphif+zorog(1)
endif
end
