subroutine img_2ppm(cdfic,cdficppm)
! --------------------------------------------------------------
! **** *img_2ppm*
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-02, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdfic nom d'un fichier image.
! En sortie:
!	cdfic nom du fichier PPM équivalent.
!		si le fichier d'entrée était un PPM, cdifcppm=cdfic.
!		sinon, cdficppm est égal à cdfic".ppm" et convert a été appelé pour créer ce fichier.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
character*(*) cdfic,cdficppm
character*2 cl2
character*1500 clexe
logical llexist,llrecent
integer(kind=4) :: system,ierr
!
!-------------------------------------------------
! Le fichier existe-t-il?
!-------------------------------------------------
!
inquire(file=cdfic,exist=llexist)
if(.not.llexist) then
  write(*,fmt=*) 'img_lec/ERREUR: le fichier image ',cdfic(1:len_trim(cdfic)),' n''existe pas!...'
  call exit(1)
endif
!
!-------------------------------------------------
! Lecture des 2 premiers octets de cdfic.
!-------------------------------------------------
!
open(42,file=cdfic,form='formatted')
read(42,fmt='(a)',advance='no') cl2
close(42)
if(cl2(1:2) == 'P6') then
  !
  ! -------------------------------------------------
  ! Le fichier est un PPM.
  ! -------------------------------------------------
  !
  cdficppm=cdfic
else
  !
  ! -------------------------------------------------
  ! Le fichier n'est pas un PPM.
  ! -------------------------------------------------
  !
  cdficppm='.'//cdfic(1:len_trim(cdfic))//'.tmp.ppm'
  !
  ! -------------------------------------------------
  ! Le fichier PPM associé existe-t-il?
  ! -------------------------------------------------
  !
  inquire(file=cdficppm,exist=llexist)
  if(llexist) then
    !
    ! -------------------------------------------------
    ! Le fichier PPM associé existe.
    ! Est-il plus récent que le fichier de base?
    ! -------------------------------------------------
    !
    call io_plus_vieux(cdfic,cdficppm,llrecent)
  else
    llrecent=.false.
  endif
  if(.not.llexist .or. (llexist .and. .not.llrecent)) then
    !
    ! -------------------------------------------------
    ! On lance l'ordre UNIX "convert".
    ! -------------------------------------------------
    !
    write(clexe,fmt='(9a)') 'convert ',cdfic(1:len_trim(cdfic)),' ',cdficppm(1:len_trim(cdficppm))
    !write(*,fmt=*) clexe(1:len_trim(clexe))
    ierr=system(clexe)
    if(ierr /= 0) write(*,fmt=*) clexe(1:len_trim(clexe)),': system error number ',ierr
  endif
endif
end
