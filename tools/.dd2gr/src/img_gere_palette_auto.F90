subroutine img_gere_palette_auto(cdpal)
! --------------------------------------------------------------
! **** *img_gere_palette_auto* Ecriture dans une chaîne de palette.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! 	rcmin, rcmax: extrêmes de la valeur du champ.
! En entrée/sortie:
!	cdpal; exemple:
!		* si en entrée cdpal='AUTO  7.5' c'est que l'utilisateur a fourni l'écartement d'isolignage.
!		  En sortie cd pal='AUTO  7.5   10.  308. ' où 10. et 308. sont les valeurs de rcmin et rcmax.
!		* si en entrée cdpal='AUTO' c'est que l'utilisateur n'a pas fourni l'écartement d'isolignage.
!		  On en calcule un, puis copie de même les extrêmes rcmin et rcmax.
!		  En sortie cd pal='AUTO  14.9   10.  308. ' où 10. et 308. sont les valeurs de rcmin et rcmax.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
character*(*) cdpal
if(cdpal(1:4) == 'AUTO') then
  cliso=cdpal(5:)
  if(rneglig /= rindef) then
    zec_iso=rneglig
  elseif(cliso /= ' ') then
    !
    !-------------------------------------------------
    ! L'utilisateur impose l'écartement d'isoligne.
    !-------------------------------------------------
    !
    read(cliso,fmt=*) zec_iso
  else
    !
    !-------------------------------------------------
    ! L'utilisateur n'impose pas l'écartement d'isoligne.
    ! On le calcule par défaut.
    !-------------------------------------------------
    !
    !zec_iso=(rcmax-rcmin)/30.
    zec_iso=(rcmax-rcmin)/50.
    !
    ! Arrondi à un chiffre significatif.
    call arrr(zec_iso,1,zec_iso2)
    zec_iso=zec_iso2
  endif
  write(cltmp,fmt=*) zec_iso,rcmin,rcmax
  cdpal='AUTO '//trim(cltmp)
endif
end
