subroutine svg_gradu(cdprisec,cdunite,p1,p2,cdaxe)
! --------------------------------------------------------------
! **** ** Graduation d'un axe de date, par traçage de lignes-guides et légendage de l'axe.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   cdprisec='princ' ou 'sec' suivant qu'il faut tracer une ligne-guide principale ou secondaire.
!   cdunite='années', 'mois', 'jours', 'heures'
!   p1, p2: min et max de la date julienne.
!   cdaxe: 'X' ou 'Y'
! En sortie:
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
character(len=*) :: cdaxe,cdprisec,cdunite
lldebug=.false.
if(lldebug) then
  write(*,fmt=*) 'cdprisec=',trim(cdprisec)
  write(*,fmt=*) 'cdunite=',trim(cdunite)
  write(*,fmt=*) 'p1=',p1
  write(*,fmt=*) 'p2=',p2
  write(*,fmt=*) 'cdaxe=',trim(cdaxe)
endif
!
!-------------------------------------------------
! Passage julien > grégorien.
!-------------------------------------------------
!
call DJ_VERS_AMQHMS(p1,IAN1,IMO1,IQU1,IHE1,IMI1,ZS1)
call DJ_VERS_AMQHMS(p2,IAN2,IMO2,IQU2,IHE2,IMI2,ZS2)
if(trim(cdunite) == 'années') then
  !
  !-------------------------------------------------
  ! Tracé des années.
  !-------------------------------------------------
  !
  if(ian2 > ian1) then
    isens=1
  else
    isens=-1
  endif
  do jan=ian1,ian2,isens
    zgrego=real(jan*10000+0101)
    call AMQHMSREE_VERS_DJ(zgrego,zdj)
    write(cltxt,fmt=*) jan
    cltxt=adjustl(adjustr(cltxt))
    zdecal=-0.2 ! décalage de l'écriture du texte, versus la position nominale, exprimé en fraction de l'espace dévolu au texte.
    ! TRAce une Ligne-Guide individuelle en position réelle zdj,sur l'axe cdaxe, avec
    ! un trait principal ou secondaire (suivant la valeur de cdprisec), et comme nombre-légende
    ! le texte cltxt.
    ztaille=rgtaille_fonte*rfont_leg
    call svg_tralgi(zdj,cdaxe,cdprisec,cltxt,ztaille,zdecal)
  enddo
elseif(trim(cdunite) == 'mois') then
  !
  !-------------------------------------------------
  ! Tracé des mois.
  !-------------------------------------------------
  !
  imois=abs(imo2-imo1)+1+abs(ian2-ian1)*12 ! nombre de mois à traiter.
  if(p1 < p2) then
    ian=ian1
    imm=imo1
  else
    ian=ian2
    imm=imo2
  endif
  do jmois=1,imois
    if(modulo(jmois,imois/12+1) == 0) then
      zgrego=real(ian*10000+imm*100+01)
      call AMQHMSREE_VERS_DJ(zgrego,zdj)
      if(imm == 1) then
        cltxt='Jan'
      elseif(imm == 2) then
        cltxt='Fév'
      elseif(imm == 3) then
        cltxt='Mar'
      elseif(imm == 4) then
        cltxt='Avr'
      elseif(imm == 5) then
        cltxt='Mai'
      elseif(imm == 6) then
        cltxt='Jun'
      elseif(imm == 7) then
        cltxt='Jul'
      elseif(imm == 8) then
        cltxt='Aoû'
      elseif(imm == 9) then
        cltxt='Sep'
      elseif(imm == 10) then
        cltxt='Oct'
      elseif(imm == 11) then
        cltxt='Nov'
      elseif(imm == 12) then
        cltxt='Déc'
      else
        write(*,fmt=*)
        write(*,fmt=*) 'svg_gradu/ERREUR: mois erroné !...'
        write(*,fmt=*) imm
        call exit(1)
      endif
      write(cltxt,fmt='(2a,i4.4)') trim(cltxt),' ',ian
      cltxt=adjustl(adjustr(cltxt))
      ztaille_fonte=rlxsvg/120.
      zdecal=+0.16 ! décalage de l'écriture du texte, versus la position nominale, exprimé en fraction de l'espace dévolu au texte.
      ! TRAce une Ligne-Guide individuelle en position réelle zdj,sur l'axe cdaxe, avec
      ! un trait principal ou secondaire (suivant la valeur de cdprisec), et comme nombre-légende
      ! le texte cltxt.
      call svg_tralgi(zdj,cdaxe,cdprisec,cltxt,ztaille_fonte,zdecal)
    endif
    !
    !-------------------------------------------------
    ! On passe au mois suivant.
    !-------------------------------------------------
    !
    if(imm < 12) then
      imm=imm+1
    else
      imm=1
      ian=ian+1
    endif
  enddo
elseif(trim(cdunite) == 'jours') then
  !
  !-------------------------------------------------
  ! Tracé des jours.
  !-------------------------------------------------
  !
  iaaaammqq1=ian1*10000+imo1*100+iqu1
  iaaaammqq2=ian2*10000+imo2*100+iqu2
  if(iaaaammqq1 > iaaaammqq2) then
    itmp=iaaaammqq2
    iaaaammqq2=iaaaammqq1
    iaaaammqq1=itmp
  endif
  call ECARTDJ(iaaaammqq1,iaaaammqq2,iec) ! nombre de jours entre les 2 dates.
  do jec=0,iec,iec/20+1
    !
    !-------------------------------------------------
    ! Date jec jours après iaaaammqq1.
    !-------------------------------------------------
    !
    call daplusj(iaaaammqq1,jec,iaaaammqq)
    !
    !-------------------------------------------------
    ! Calcul de la date julienne associée.
    !-------------------------------------------------
    !
    zgrego=real(iaaaammqq)
    call AMQHMSREE_VERS_DJ(zgrego,zdj)
    !
    !-------------------------------------------------
    ! On écrit un texte égal au quantième de cette date.
    !-------------------------------------------------
    !
    iqq=modulo(iaaaammqq,100)
    write(cltxt,fmt=*) iqq
    cltxt=adjustl(adjustr(cltxt))
    !
    !-------------------------------------------------
    ! Ecriture du quantième dans le fichier SVG.
    !-------------------------------------------------
    !
    ztaille_fonte=rlxsvg/100.
    zdecal=-0.2 ! décalage de l'écriture du texte, versus la position nominale, exprimé en fraction de l'espace dévolu au texte.
    ! TRAce une Ligne-Guide individuelle en position réelle zdj,sur l'axe cdaxe, avec
    ! un trait principal ou secondaire (suivant la valeur de cdprisec), et comme nombre-légende
    ! le texte cltxt.
    call svg_tralgi(zdj,cdaxe,cdprisec,cltxt,ztaille_fonte,zdecal)
  enddo
elseif(trim(cdunite) == 'heures') then
  !
  !-------------------------------------------------
  ! Tracé des heures.
  !-------------------------------------------------
  !
  iaaaammqqhh1=ian1*1000000+imo1*10000+iqu1*100+ihe1
  iaaaammqqhh2=ian2*1000000+imo2*10000+iqu2*100+ihe2
  if(lldebug) then
    write(*,fmt=*) 'iaaaammqqhh1=',iaaaammqqhh1
    write(*,fmt=*) 'iaaaammqqhh2=',iaaaammqqhh2
  endif
  if(iaaaammqqhh1 > iaaaammqqhh2) then
    itmp=iaaaammqqhh2
    iaaaammqqhh2=iaaaammqqhh1
    iaaaammqqhh1=itmp
  endif
  call ECARTD(iaaaammqqhh1,iaaaammqqhh2,2,igre) ! nombre d'heures entre les 2 dates.
  if(lldebug) then
    write(*,fmt=*) 'igre=',igre
  endif
  if(igre < 60) then
    iec_h=1
  elseif(igre < 120) then
    iec_h=2
  elseif(igre < 180) then
    iec_h=3
  elseif(igre < 240) then
    iec_h=4
  elseif(igre < 360) then
    iec_h=6
  elseif(igre < 480) then
    iec_h=8
  else
    iec_h=12
  endif
  do jec=0,igre,iec_h
    !
    !-------------------------------------------------
    ! Date jec heures après iaaaammqq1.
    !-------------------------------------------------
    !
    call daplus(iaaaammqqhh1,2,jec,iaaaammqqhh)
    !
    !-------------------------------------------------
    ! Calcul de la date julienne associée.
    !-------------------------------------------------
    !
    zgrego=real(iaaaammqqhh/100)+real(modulo(iaaaammqqhh,100))/100. ! réel au format AAAAMMQQ.HHMMSS.
    call AMQHMSREE_VERS_DJ(zgrego,zdj)
    !
    !-------------------------------------------------
    ! On écrit un texte égal à la valeur de l'heure.
    !-------------------------------------------------
    !
    ihh=modulo(iaaaammqqhh,100)
    write(cltxt,fmt=*) ihh
    cltxt=adjustl(adjustr(cltxt))
    !
    !-------------------------------------------------
    ! Ecriture du quantième dans le fichier SVG.
    !-------------------------------------------------
    !
    ztaille_fonte=rlxsvg/150.
    zdecal=+0.16 ! décalage de l'écriture du texte, versus la position nominale, exprimé en fraction de l'espace dévolu au texte.
    ! TRAce une Ligne-Guide individuelle en position réelle zdj,sur l'axe cdaxe, avec
    ! un trait principal ou secondaire (suivant la valeur de cdprisec), et comme nombre-légende
    ! le texte cltxt.
    call svg_tralgi(zdj,cdaxe,cdprisec,cltxt,ztaille_fonte,zdecal)
  enddo
else
  write(*,fmt=*)
  write(*,fmt=*) 'svg_gradu/ERREUR: cas non encore prévu !...'
  write(*,fmt=*)
endif
end
