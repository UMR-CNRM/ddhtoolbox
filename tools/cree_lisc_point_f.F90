program f
implicit none
INTEGER(KIND=4), parameter :: jpx=330 ! nb max de lignes de 'liste_conversion'.
CHARACTER*200 :: CLC
CHARACTER*200 :: CLFE1
CHARACTER*200 :: CLFE2
CHARACTER*200 :: CLFS1
CHARACTER*200 :: CLSOR
INTEGER(KIND=4) :: ILISC
INTEGER(KIND=4) :: ILSOR
INTEGER(KIND=4) :: IOK
INTEGER(KIND=4) :: IULFE1
INTEGER(KIND=4) :: IULFE2
INTEGER(KIND=4) :: IULFS1
INTEGER(KIND=4) :: IX
INTEGER(KIND=4) :: IY
INTEGER(KIND=4) :: JL
INTEGER(KIND=4) :: JLC
INTEGER(KIND=4) :: JLO
character*240 cllc(jpx)
character*30  cllo(jpx)
!
! Lecture de la liste de conversion (LC).
!
iulfe1=22
clfe1='liste_conversion'
open(iulfe1,file=clfe1,form='formatted')
ix=0
100	read(iulfe1,fmt='(a)',end=200) clc
if(clc(1:1) /= "#" .and. clc /= ' ') then
  !
  ! La ligne courante n'est pas une ligne de commentaire.
  !
  ix=ix+1
  cllc(ix)=clc
endif
goto 100
200	continue
close(iulfe1)
print*,ix,' articles lus sur ',clfe1
!
! Lecture de la liste ordonnée (LO) (ordre du fichier).
! Tous les champs de LC qui sont dans LO seront
! recopiés dans le source lisc.F90 (SL) de sortie,
! dans l'ordre de LO.
! Tous les champs de LC qui ne sont pas dans LO seront
! recopiés dans le source lisc.F90 (SL) de sortie,
! en fin de liste, dans l'ordre de LC.
!
iulfe2=23
clfe2='liste_ordonnee'
open(iulfe2,file=clfe2,form='formatted')
!
! Ouverture du fichier de sortie.
!
iulfs1=24
clfs1='lisc.F90'
open(iulfs1,file=clfs1,form='formatted')
!
! On porte sur le source de sortie l'en-tête.
!
clsor='subroutine lisc(kdiml,cdlisc,klisc)'
ilsor=len_trim(clsor)
write(iulfs1,fmt='(a)') clsor(1:ilsor)
!
! On porte sur le source de sortie les déclarations.
!
clsor='character*(*) cdlisc(kdiml)'
ilsor=len_trim(clsor)
write(iulfs1,fmt='(a)') clsor(1:ilsor)
iy=0
ilisc=0
120	read(iulfe2,fmt='(a)',end=220) clc
iy=iy+1
cllo(iy)=clc
iok=0
do jl=1,ix
  if(clc(1:13) == cllc(jl)(5:17)) then
    !
    ! Le champ courant est dans LO et LC.
    ! On le copie sur SL.
    !
    ilisc=ilisc+1
    clsor=cllc(jl)
    ilsor=len_trim(clsor)
    write(iulfs1,fmt='(a,i3.3,a)') 'cdlisc(',ilisc,')= &'
    write(iulfs1,fmt='(3a)') '&''',clsor(1:46),''' &'
    write(iulfs1,fmt='(3a)') '&//''',clsor(47:92),''' &'
    write(iulfs1,fmt='(3a)') '&//''',clsor(93:139),''''
    iok=1
  endif
enddo
if(iok == 0) then
  print*,'ATTENTION: le champ ',clc(1:13),' est absent de la liste de conversion!...'
endif
goto 120
220	continue
close(iulfe2)
!
! Les champs présents dans LC mais absents de LO
! sont recopiés dans l'ordre de LC.
!
do jlc=1,ix
  iok=0
  do jlo=1,iy
    if(cllo(jlo)(1:13) == cllc(jlc)(5:17)) then
      !
      ! Le champ courant est dans LO et LC.
      !
      iok=1
    endif
  enddo
  if(iok == 0) then
    !
    ! le champ est présent dans LC mais absent de LO.
    ! On le recopie dans LC.
    !
    ilisc=ilisc+1
    clsor=cllc(jlc)
    ilsor=len_trim(clsor)
    !write(iulfs1,fmt='(a,i3.3,3a)') 'cdlisc(',ilisc,')=''',clsor(1:ilsor),''''
    write(iulfs1,fmt='(a,i3.3,a)') 'cdlisc(',ilisc,')= &'
    write(iulfs1,fmt='(3a)') '&''',clsor(1:46),''' &'
    write(iulfs1,fmt='(3a)') '&//''',clsor(47:92),''' &'
    write(iulfs1,fmt='(3a)') '&//''',clsor(93:139),''''
  endif
enddo
write(iulfs1,fmt=*) 'klisc=',ilisc
print*,iy,' articles lus sur ',clfe2
clsor='end'
ilsor=len_trim(clsor)
write(iulfs1,fmt='(a)') clsor(1:ilsor)
close(iulfs1)
end
