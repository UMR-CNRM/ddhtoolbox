program ddhreu
! --------------------------------------------------------------------------
! **** *ddhreu*  Réunion de 2 fichiers de DDH.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! Methode:
! --------
! Externes: /
! ---------
! Auteur:         2018-09, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
#include"implicit_r8i4.h"
real(kind=8), allocatable :: zval1(:)
real(kind=8), allocatable :: zval2(:)
real(kind=8), allocatable :: zval3(:)
integer(kind=4), allocatable :: ival1(:)
integer(kind=4), allocatable :: ival2(:)
integer(kind=4), allocatable :: ival3(:)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg /= 3) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'OBJET: Réunion de 2 fichiers de DDH.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'UTILISATION: ddhreu F1 F2 FRES'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'METHODE: pour tous les articles VPP0, VCT1, FQVPRECICVOL, etc, réunit pour chaque article les données de l''un et de l''autre.'
  write(*,'(9a)') '  Si dans F1 l''article VCT1 a par exemple une longueur 137 et dans F2 une longueur 1000, cet article aura une longueur 1137 dans FRES.'
  write(*,'(9a)') '  Pour les articles "INDICE EXPERIENCE", "DATE", "DOCFICHIER", "ECHEANCE", "DOCD" l''article de F1 est simplement copié dans FRES.'
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getarg(1,clf1)
call getarg(2,clf2)
call getarg(3,clfres)
!
!-------------------------------------------------
! Ouverture du fichier LFA.
!-------------------------------------------------
!
iullfa1=70 ; call lfaouv(iullfa1,clf1,'R')
iullfa2=80 ; call lfaouv(iullfa2,clf2,'R')
iullfa3=90 ; call lfaouv(iullfa3,clfres,'W')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
ierr=0
do while(ierr == 0)
  clna=' '
  call lfacas(iullfa1,clna,cltype,ilong1,ierr)
  if(ierr == 0) then
    !
    !-------------------------------------------------
    ! On n'est pas en fin de fichier.
    ! on va soit lire l'article courant, soit avancer à l'article suivant.
    !-------------------------------------------------
    !
    if(trim(clna) == 'INDICE EXPERIENCE' .or. trim(clna) == 'DATE' .or. trim(clna) == 'DOCFICHIER' .or. trim(clna) == 'ECHEANCE') then
      call lfacop(iullfa1,clna,clna,iullfa3)
    elseif(clna(1:4) == 'DOCD') then
      call lfacop(iullfa1,clna,clna,iullfa3)
      read(clna(5:7),fmt=*) idom
      idom=idom+1
      write(cldocd,fmt='(a,i3.3)') 'DOCD',idom
      call lfacas(iullfa2,cldocd,cltypegol,ilonggol,ierrgol)
      if(ierrgol == 0) then
        call lfacop(iullfa2,cldocd,cldocd,iullfa3)
      endif
    else
      if(cltype(1:1) == 'R') then
        !
        !-------------------------------------------------
        ! Tableau de réels.
        !-------------------------------------------------
        !
        allocate(zval1(ilong1))
        call lfacas(iullfa2,clna,cltype2,ilong2,ierr)
        allocate(zval2(ilong2))
        ilong3=ilong1+ilong2
        allocate(zval3(ilong3))
        call lfalecr(iullfa1,clna,ilong1,zval1,ilong,ierr)
        call lfalecr(iullfa2,clna,ilong1,zval2,ilong,ierr)
        do jval=1,ilong3
          if(jval <= ilong1) then
            zval3(jval)=zval1(jval)
          else
            zval3(jval)=zval2(jval-ilong1)
          endif
        enddo
        call lfaecrr(iullfa3,clna,zval3,ilong3)
        deallocate(zval1)
        deallocate(zval2)
        deallocate(zval3)
      else
      endif
    endif
  endif
enddo
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iullfa1)
call lfafer(iullfa2)
call lfafer(iullfa3)
end
