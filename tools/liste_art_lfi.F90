subroutine etalae(kul,cdcha,kdcha,kncha)
! --------------------------------------------------------------------------
! **** *ETALAE*  Etablissement de la liste des articles du LFI d'entrée.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 96-01
! --------------------------------------------------------------------------
! En entree:
! fichier LFI
! En sortie:
! kncha contient le nombre d'articles de ce fichier.
! cdcha(1, ..., kncha) contient le nom de ces articles.
! --------------------------------------------------------------------------
implicit none
integer(kind=4) kdcha,kul,kncha,irep,ierr,ilong,iposex
character*16 clnoma
character*(*) cdcha(kdcha)
!
! On positionne le pointeur en début de fichier.
!
call lfipos(irep,kul)
kncha=0
ierr=0
  100 continue
!
! Lecture de l'article suivant.
!
call lficas(irep,kul,clnoma,ilong,iposex,.true.)
if(ilong == 0) goto 200 ! on est en fin de fichier.
kncha=kncha+1
if(kncha > kdcha) then
  ierr=1
else
  cdcha(kncha)=clnoma
endif
goto 100
  200 continue
if(ierr == 1) then
  write(*,*) 'ERREUR etalae: kdcha plus petit'
  write(*,*) 'que le nombre de champs du fichier!...'
  write(*,*) kncha, kdcha
  call exit(1)
endif
end
