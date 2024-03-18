function cl_nettoie_blancs(cdc)
!
!-------------------------------------------------
! Fonction supprimant d'une chaîne les blancs consécutifs, et les blancs
! connexes au caractère ".
!-------------------------------------------------
!
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
#include"implicit_r8i4.h"

character(len=*) cdc
!
!-------------------------------------------------
! Elimination des doubles blancs.
!-------------------------------------------------
!
clc=cdc(1:1)
ic=1
do jc=2,len_trim(cdc)
  !
  !-------------------------------------------------
  ! On saute tous les blancs inutiles.
  !-------------------------------------------------
  !
  if(cdc(jc:jc) == ' ' .and. cdc(jc-1:jc-1) == ' ') cycle
  !
  !-------------------------------------------------
  ! Le caractère courant est valide. On le copie sur le chaîne de sortie.
  !-------------------------------------------------
  !
  ic=ic+1
  clc(ic:ic)=cdc(jc:jc)
enddo
!
!-------------------------------------------------
! Elimination des blancs connexes à un ", à l'intérieur d'une chaîne (i.e. entre deux ").
!-------------------------------------------------
!
clc2=clc(1:1)
ic=1
iopen=0 ! 1 si on est à l'intérieur d'une chaîne, i.e. si on a rencontré un nombre impair de ", 0 sinon.
do jc=2,len_trim(clc)-1
  !
  !-------------------------------------------------
  ! On saute tous les blancs inutiles.
  !-------------------------------------------------
  !
  if(clc(jc:jc) == ' ' .and. clc(jc-1:jc-1) == '"' .and. iopen == 1) cycle
  if(clc(jc:jc) == ' ' .and. clc(jc+1:jc+1) == '"' .and. iopen == 1) cycle
  !
  !-------------------------------------------------
  ! On entre ou sort d'une chaîne de caractères.
  !-------------------------------------------------
  !
  if(clc(jc:jc) == '"') iopen=1-iopen
  !
  !-------------------------------------------------
  ! Le caractère courant est valide. On le copie sur le chaîne de sortie.
  !-------------------------------------------------
  !
  ic=ic+1
  clc2(ic:ic)=clc(jc:jc)
enddo
!
!-------------------------------------------------
! Copie du dernier caractère, toujours valide.
!-------------------------------------------------
!
ic=ic+1
clc2(ic:ic)=clc(len_trim(clc):len_trim(clc))
!
!-------------------------------------------------
! Ecriture de la chaîne de sortie de la fonction.
!-------------------------------------------------
!
cl_nettoie_blancs=clc2
end
