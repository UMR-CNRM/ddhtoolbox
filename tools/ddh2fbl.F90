!OPTIONS NODOUBLE
program ddh2fbl
! --------------------------------------------------------------------------
! **** *DDH2FBL*  
! --------------------------------------------------------------------------
! Sujet:   
! Reads a DDH file, and writes the ddhb directive file, consistent
! with the articles names from the DDH file.
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
! Auteur:         2008-12, J.M. Piriou.
! -------
! Modifications:    
! --------------------------------------------------------------------------
implicit none
CHARACTER(len=200) :: clfddh,clna,cltype,clddhb_bps,cldirsor,clexe,cloutdir
INTEGER(KIND=4) :: ierr,jarg,iullfa1,ilong,ichamp,ipos,system
INTEGER(KIND=4), parameter :: jpchamp=60 ! nb maxi de champs pronostiques dans un fichier DDH: CT, QV, etc.
INTEGER(KIND=4), parameter :: jppostes=100 ! nb maxi de postes pour le bilan d'une variable pronostique; ex: TCTRAYSO, TCTRAYTER, etc.
character(len=200) :: clpostes(jpchamp,jppostes), clchamp(jpchamp), clvar, clfsor
INTEGER(KIND=4) :: ipostes(jppostes),jchamp,jpostes,iarg,iargc, iul
LOGICAL :: llok
#include"ddhpar.h"
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
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Creates the ddhb directive files (.fbl) consistent with a given DDH file.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Usage: ddh2fbl FDDH [OUTPUT_DIR]'
  write(*,'(9a)') '	where'
  write(*,'(9a)') '		- FDDH is the input DDH file.'
  write(*,'(9a)') '		- OUTPUT_DIR is the output directory, receiving the FBL files. '&
        &'OUTPUT_DIR is the path relative to the $DDHB_BPS directory. Default value: tmp.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'The output FBL files will be written on the directory $DDHB_BPS/$OUTPUT_DIR.'
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getargp(1,clfddh)
call getargp(2,cloutdir)
!
!-------------------------------------------------
! Ouverture du fichier LFA.
!-------------------------------------------------
!
iullfa1=23
call lfaouv(iullfa1,clfddh,'R')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
ichamp=0
ipostes=0
call lfarew(iullfa1)
ierr=0
do while(ierr == 0)
  clna=' '
  call lfacas(iullfa1,clna,cltype,ilong,ierr)
  if(ierr == 0) then
    if(clna(1:1) == 'V' .or. clna(1:1) == 'F' .or. clna(1:1) == 'T') then
      clvar=clna(2:3)
      !
      !-------------------------------------------------
      ! Cet article est-il celui d'un champ déjà rencontré?
      !-------------------------------------------------
      !
      llok=.false.
      ipos=0
      do jchamp=1,ichamp
        if(trim(clvar) == clchamp(jchamp)) then
          llok=.true.
          ipos=jchamp
        endif
      enddo
      if(.not.llok) then
        !
        !-------------------------------------------------
        ! Ce champ n'a pas encore été rencontré.
        !-------------------------------------------------
        !
        ichamp=ichamp+1
        clchamp(ichamp)=clvar
        ipos=ichamp
      endif
      if(clna(1:1) == 'T' .or. clna(1:1) == 'F') then
        !
        !-------------------------------------------------
        ! L'article courant est celui d'un poste de bilan d'une variable pronostique.
        ! Cett variable pronostique est celle de numéro ipos.
        !-------------------------------------------------
        !
        ipostes(ipos)=ipostes(ipos)+1
        clpostes(ipos,ipostes(ipos))=clna
      endif
    endif
    !
    !-------------------------------------------------
    ! On n'est pas en fin de fichier.
    ! On avance à l'article suivant.
    !-------------------------------------------------
    !
    call lfaavan(iullfa1)
  endif
enddo
!
!-------------------------------------------------
! Fermeture du fichier LFA.
!-------------------------------------------------
!
call lfafer(iullfa1)
!
!-------------------------------------------------
! Ecriture des fichiers de directives.
!-------------------------------------------------
!
do jchamp=1,ichamp
  !if(ipostes(jchamp) >= 1) then
  if(.true.) then
    !
    !-------------------------------------------------
    ! Il y a au moins un poste de bilan pour cette variable pronostique.
    !-------------------------------------------------
    !
    !
    !-------------------------------------------------
    ! Nom du fichier de directives de cette variable pronostique.
    !-------------------------------------------------
    !
    !clfsor=trim(clfddh)//'.var.'//trim(clchamp(jchamp))//'.fbl'
    CALL GETENV('DDHB_BPS',clddhb_bps)
    if(clddhb_bps /= ' ') then
      !
      !-------------------------------------------------
      ! On va créer un répertoire temporaire sur le répertoire $DDHB_BPS.
      !-------------------------------------------------
      !
      if(cloutdir == ' ' ) cloutdir='tmp'
      cldirsor=trim(clddhb_bps)//'/'//trim(cloutdir)
      clexe='mkdir -p '//trim(cldirsor)
      ierr=system(clexe)
      !
      !-------------------------------------------------
      ! Les fichiers de directives FBL vont être écrits sur ce répertoire.
      !-------------------------------------------------
      !
      clfsor=trim(cldirsor)//'/'//trim(clchamp(jchamp))//'.fbl'
    else
      clfsor=trim(clchamp(jchamp))//'.fbl'
    endif
    write(*,fmt=*) trim(clfsor)
    !
    !-------------------------------------------------
    ! Ecriture du fichier de directives.
    !-------------------------------------------------
    !
    iul=40 ; open(iul,file=clfsor,form='formatted')
    write(iul,fmt='(9a)') 'BUDGET LIST ', trim(clchamp(jchamp))
    write(iul,fmt='(9a)') 'MAIN ', trim(clchamp(jchamp))
    write(iul,fmt='(9a)') trim(clchamp(jchamp))
    do jpostes=1,ipostes(jchamp)
      write(iul,fmt='(9a)') '#-------------------------------------'
      write(iul,fmt='(9a)') 'BEGIN BLOCK ',trim(clpostes(jchamp,jpostes))
      write(iul,fmt='(9a)') trim(clpostes(jchamp,jpostes))
    enddo
    close(iul)
  endif
enddo
end
