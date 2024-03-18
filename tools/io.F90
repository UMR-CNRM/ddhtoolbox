subroutine io_lec(cdf,ktail,cdcar)
! --------------------------------------------------------------
! **** ** Lecture des ktail premiers octets d'un fichier sur une chaine de caractères.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdf nom du fichier.
! En sortie:
!	ktail taille en octets.
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

INTEGER(KIND=4) :: KTAIL,IUL,io_freelu
character*(*) cdf
character*1 cdcar(ktail)
iul=io_freelu()
!
!-------------------------------------------------
! Lecture comme fichier à accès direct.
!-------------------------------------------------
!
open(iul,file=cdf,access='direct',recl=ktail)
read(iul,rec=1) cdcar
close(iul)
end
subroutine io_ecr(cdf,ktail,cdcar)
! --------------------------------------------------------------
! **** ** Ecriture des ktail octets de cdcar sur un fichier.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdf nom du fichier.
!	ktail taille en octets.
!	cdcar caractères à écrire.
! En sortie:
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

INTEGER(KIND=4) :: KTAIL,IUL,io_freelu,ierr
character*(*) cdf
character*1 cdcar(ktail)
character*147 clexe
integer(kind=4) system
iul=io_freelu()
!
!-------------------------------------------------
! On lance l'ordre UNIX "rm -f".
!-------------------------------------------------
!
write(clexe,fmt='(3a)') 'rm -f ',cdf(1:len_trim(cdf))
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
!
!-------------------------------------------------
! Ecriture comme fichier à accès direct.
!-------------------------------------------------
!
open(iul,file=cdf,access='direct',recl=ktail)
write(iul,rec=1) cdcar
close(iul)
end
subroutine io_ligne(kligne,ktail,cdcar,kcar1,kcar2,cdlig)
! --------------------------------------------------------------
! **** ** Position et valeur de la ligne kligne dans un tableau de caractères.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kligne numéro de la ligne désirée dans le tableau de caractères.
!	cdcar(ktail) les ktail caractères du tableau d'entrée.
! En sortie:
!	la ligne kligne va du caractère cdcar(kcar1) au caractère cdcar(kcar2).
!	cdlig valeur de cette ligne.
!	Si la ligne n'existe pas dans le fichier, la réponse est kcar1=kcar2=0.
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

INTEGER(KIND=4) :: KTAIL,KLIGNE,KCAR1,KCAR2,ILIGNE,JCAR,JCAR2
character*1 cdcar(ktail)
character*(*) cdlig
iligne=1
kcar1=1
do jcar=1,ktail
  if(ichar(cdcar(jcar)) == 10) then
  !
  !-------------------------------------------------
  ! Fin de ligne.
  !-------------------------------------------------
  !
    if(iligne == kligne) then
      kcar2=jcar-1
      cdlig=' '
      do jcar2=1,min(len(cdlig),kcar2-kcar1+1)
        cdlig(jcar2:jcar2)=cdcar(kcar1+jcar2-1)
      enddo
      return
    endif
    kcar1=jcar+1
    iligne=iligne+1
  endif
enddo
!
!-------------------------------------------------
! Si on est arrivé ici, c'est qu'il n'y a pas de ligne kligne.
!-------------------------------------------------
!
kcar1=0
kcar2=0
cdlig=' '
end
subroutine io_taille(cdf,ktail)
! --------------------------------------------------------------
! **** ** Taille d'un fichier en octets.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdf nom du fichier.
! En sortie:
!	ktail taille en octets.
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

INTEGER(KIND=4) :: KTAIL,IERR,IUL,io_freelu
character*(*) cdf
character*147 clexe
integer(kind=4) system
!
!-------------------------------------------------
! On lance l'ordre UNIX "rm -f".
!-------------------------------------------------
!
write(clexe,fmt='(3a)') 'rm -f .io.wcc'
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
iul=io_freelu()
!
!-------------------------------------------------
! On lance l'ordre UNIX "wc -c".
!-------------------------------------------------
!
write(clexe,fmt='(3a)') 'wc -c ',cdf(1:len_trim(cdf)),' > .io.wcc'
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
iul=io_freelu()
!
!-------------------------------------------------
! On lit le résultat.
!-------------------------------------------------
!
open(iul,file='.io.wcc',form='formatted')
read(iul,fmt=*) ktail
close(iul)
!
!-------------------------------------------------
! On nettoie le fichier résultat.
!-------------------------------------------------
!
write(clexe,fmt='(a)') 'rm .io.wcc'
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
end
function io_freelu()
! --------------------------------------------------------------
! **** ** Obtention d'une unité logique de fichier libre.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

INTEGER(KIND=4) :: JUNIT,IO_FREELU
logical*4 llopen
do junit=25,200
  inquire(unit=junit,opened=llopen)
  if(.not.llopen) then
    io_freelu=junit
    exit
  endif
enddo
end
subroutine io_plus_vieux(cdf1,cdf2,ldvieux)
! --------------------------------------------------------------
! **** ** Indique si f1 est plus vieux que f2.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdf1 nom du fichier 1.
!	cdf2 nom du fichier 2.
! En sortie:
!	ldvieux vrai si f1 est plus vieux que f2.
! --------------------------------------------------------------

!#include "tsmbkind.h"

IMPLICIT NONE

character*(*) cdf1,cdf2
character*147 clexe
character*200 clres
integer(kind=4) system
logical ldvieux
INTEGER(KIND=4) :: iul,io_freelu,ierr
!
!-------------------------------------------------
! On crée un shell de test comparatif d'ancienneté.
!-------------------------------------------------
!
iul=io_freelu()
open(iul,file='.io.test',form='formatted')
write(iul,fmt=*) 'if test ',cdf1(1:len_trim(cdf1)),' -ot ',cdf2(1:len_trim(cdf2))
write(iul,fmt=*) 'then'
write(iul,fmt=*) '	echo "oui"'
write(iul,fmt=*) 'else'
write(iul,fmt=*) '	echo "non"'
write(iul,fmt=*) 'fi'
close(iul)
!
!-------------------------------------------------
! On lance ce shell.
!-------------------------------------------------
!
write(clexe,fmt='(100a)') 'sh .io.test > .io.resultat'
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
!
!-------------------------------------------------
! On lit le résultat.
!-------------------------------------------------
!
iul=io_freelu()
open(iul,file='.io.resultat',form='formatted')
read(iul,fmt='(a)') clres
close(iul)
!
!-------------------------------------------------
! On nettoie les fichiers.
!-------------------------------------------------
!
write(clexe,fmt='(a)') 'rm .io.test .io.resultat'
ierr=system(clexe)
if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
!
!-------------------------------------------------
! Test final.
!-------------------------------------------------
!
if(clres(1:3) == 'oui') then
  ldvieux=.true.
else
  ldvieux=.false.
endif
end
