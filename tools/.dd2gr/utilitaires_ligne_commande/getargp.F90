function iargcp()
! --------------------------------------------------------------
! **** *iargcp* Get the number of command line arguments.
! --------------------------------------------------------------
! Sujet:
! Cette routine est creee pour contourner une bug du F90 Lahey-Fujitsu,
! qui compte les arguments destinés non au FORTRAN mais au système d'exploitation.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
character*200 clarg
!
!-------------------------------------------------
! Décalage de 1 de la numérotation sur machines HP.
!-------------------------------------------------
!
iarg=1
#ifdef HPPA
iarg=iarg+1
#endif
call getarg(iarg,clarg)
!
!-------------------------------------------------
! Cas du FORTRAN Lahey-Fujitsu pour LINUX: 
! si la ligne de commande comporte en son début
! "-Wl,", le premier argument est ici à oublier
! car c'est une option d'exécution destinée
! au système et non au logiciel FORTRAN.
!-------------------------------------------------
!
if(clarg(1:4) == '-Wl,') then
  iargcp=iargc()-1
else
  iargcp=iargc()
endif
end
subroutine getargp(karg,cdarg)
! --------------------------------------------------------------
! **** *getargp* GET Command Line Arguments.
! --------------------------------------------------------------
! Sujet:
! Cette routine est creee pour contourner une bug du.F90 HP,
! qui decale de un par rapport au standard l'indice de l'argument
! a getarg!...
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   98-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
character*(*) cdarg
character*200 clarg
!-------------------------------------------------
! Décalage de 1 de la numérotation sur machines HP.
!-------------------------------------------------
!
#ifdef HPPA
iarg=karg+1
#else
iarg=karg
#endif
!
!-------------------------------------------------
! Cas du FORTRAN Lahey-Fujitsu pour LINUX: 
! si la ligne de commande comporte en son début
! "-Wl,", le premier argument est ici à oublier
! car c'est une option d'exécution destinée
! au système et non au logiciel FORTRAN.
!-------------------------------------------------
!
call getarg(1,cdarg)
clarg=cdarg
if(clarg(1:4) == '-Wl,') then
  iarg=iarg+1
endif
!
!-------------------------------------------------
! Appel à la fonction système GETARG.
!-------------------------------------------------
!
call getarg(iarg,cdarg)
!write(*,fmt=*) 'Debug: getargp = ',karg,iarg,cdarg
end
