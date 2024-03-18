program lisce
implicit none
CHARACTER*200 :: CLC2
INTEGER(KIND=4) :: IAFICL
INTEGER(KIND=4) :: ILART
INTEGER(KIND=4) :: ILC2
INTEGER(KIND=4) :: JAFICL

#include"ddhpar.h"
character *240 claficl(jpnomct)
character *240 clart
!
! Chargement de la liste de conversion.
!
call lisc(jpnomct,claficl,iaficl)
print*,iaficl,' articles fournis par lisc.'
open(12,file='lc_TOUS',form='formatted')
open(13,file='lc_PART_var',form='formatted')
open(14,file='lc_PART_dyn',form='formatted')
open(15,file='lc_PART_phy',form='formatted')
open(16,file='lc_PART_addit_ddh',form='formatted')
open(17,file='lc_PART_ddht_ddhi',form='formatted')
open(18,file='lc_BILAN',form='formatted')
open(21,file='lc_PRINC_COMPL',form='formatted')
do jaficl=1,iaficl
  clart=claficl(jaficl)
  ilart=len_trim(clart)
  clc2=clart(05:17)//' # '//clart(44:114)
  ilc2=len_trim(clc2)
  if(clart(1:1) /= 'X') then
    write(12,fmt='(a)') clc2(1:ilc2)
    if(clart(1:1) == 'D') then
      ! Champ dynamique.
      write(14,fmt='(a)') clc2(1:ilc2)
    elseif(clart(1:1) == 'P') then
      ! Champ physique.
      write(15,fmt='(a)') clc2(1:ilc2)
    elseif(clart(1:1) == 'V') then
      ! Champ de variable.
      write(13,fmt='(a)') clc2(1:ilc2)
    elseif(clart(1:1) == 'A') then
      ! Champ de diagnostic additionnel du code ARPEGE des DDH.
      write(16,fmt='(a)') clc2(1:ilc2)
    elseif(clart(1:1) == 'B') then
      ! Champ de diagnostic additionnel des utilitaires
      ! de trace (intd) ou de cumul (ddht).
      write(17,fmt='(a)') clc2(1:ilc2)
    else
      write(*,fmt=*) 'lisc_eclate/INFORMATION: le champ ',trim(clart),' ne peut être partitionné comme un champ physique, dynamique, ou variable.'
      write(*,fmt=*) '	premier caractère: ',clart(1:1)
    endif
    if(clart(1:1) == 'D' &
& 		.or.clart(1:1) == 'P' &
& 		.or.clart(1:1) == 'V' &
& 		.or.(clart(1:1) == 'B'.and.clart(5:5) == 'V'.and.clart(8:8) == 'F')) then
      ! Champ dynamique, physique ou de variable
      ! ou champ de tendance moyenne final - initial.
      write(18,fmt='(a)') clc2(1:ilc2)
    endif
    if(clart(8:16) == 'PRINCIPAL'.or.clart(8:17) == 'COMPLEMENT' &
& 		.or.(clart(5:5) == 'V'.and.(clart(8:8) == 'M'.or.clart(8:8) == 'F'))) then
      write(21,fmt='(a)') clc2(1:ilc2)
    endif
  endif
enddo
end
#include"lisc.F90"
