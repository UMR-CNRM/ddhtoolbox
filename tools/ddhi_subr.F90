subroutine cooxd(cdtyp,pdocdf,kdom,px)
! --------------------------------------------------------------------------
! **** *COOXD*  Coordonnee X associee a un domaine DDH donne.
! --------------------------------------------------------------------------
! Sujet:
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
! Auteur:           92-02, J.M. Piriou.
! -------
! --------------------------------------------------------------------------
! En entree:
! cdtyp    type de coordonnee horizontale desiree.
! pdocdf   tableau reel des documentations de domaines.
! kdom     numero du domaine courant.
! En sortie:
! px       coordonnee horizontale desiree.
! --------------------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: ILTYP
INTEGER(KIND=4) :: ITYPE
INTEGER(KIND=4) :: KDOM
REAL(KIND=8) :: PX
REAL(KIND=8) :: ZARG
REAL(KIND=8) :: ZCONRD
REAL(KIND=8) :: ZLAT1
REAL(KIND=8) :: ZLAT2
REAL(KIND=8) :: ZLAT3
REAL(KIND=8) :: ZLAT4
REAL(KIND=8) :: ZLON1
REAL(KIND=8) :: ZLON2
REAL(KIND=8) :: ZLON3
REAL(KIND=8) :: ZLON4
REAL(KIND=8) :: ZMODYX
REAL(KIND=8) :: ZX1
REAL(KIND=8) :: ZX2
REAL(KIND=8) :: ZY1
REAL(KIND=8) :: ZY2
REAL(KIND=8) :: zmanq

#include"ddhpar.h"
character*(*) cdtyp
REAL(KIND=8) pdocdf(jpdoc,jpdom) ! articles de documentation des domaines (tableau tota
zconrd=45./atan(1.)
itype=nint(pdocdf(11,kdom))
iltyp=len_trim(cdtyp)
if(cdtyp(1:iltyp) == 'LAT') then
  !
  ! L'utilisateur veut en X la latitude en degres.
  !
  if(itype == 3) then
    !
    ! Le domaine est un rectangle.
    !
    zlat2=zconrd*asin(min(1.,max(-1.,pdocdf(4,kdom))))
    zlat3=zconrd*asin(min(1.,max(-1.,pdocdf(8,kdom))))
    px=0.5*(zlat2+zlat3)
  elseif(itype == 2) then
    !
    ! Le domaine est un quadrilatère.
    !
    zlat1=zconrd*asin(min(1.,max(-1.,pdocdf(4,kdom))))
    zlat2=zconrd*asin(min(1.,max(-1.,pdocdf(6,kdom))))
    zlat3=zconrd*asin(min(1.,max(-1.,pdocdf(8,kdom))))
    zlat4=zconrd*asin(min(1.,max(-1.,pdocdf(10,kdom))))
    px=0.25*(zlat1+zlat2+zlat3+zlat4)
  else
    !
    ! Autres types de domaine.
    ! La latitude est tirée de doc(4).
    !
    zarg=min(1.,max(-1.,pdocdf(4,kdom)))
    px=zconrd*asin(zarg)
  endif
elseif(cdtyp(1:iltyp) == 'LON') then
  !
  ! L'utilisateur veut en X la longitude en degres.
  !
  if(itype == 3) then
    !
    ! Le domaine est un rectangle.
    ! On prend comme longitude la valeur moyenne.
    !
    px=zmodyx(zconrd*0.5*(pdocdf(3,kdom)+pdocdf(5,kdom)),-180.,180.)
  elseif(itype == 2) then
    !
    ! Le domaine est un quadrilatère.
    ! On prend comme longitude la valeur moyenne.
    !
    zlon1=zmodyx(zconrd*pdocdf(3,kdom),-180.,180.)
    zlon2=zmodyx(zconrd*pdocdf(5,kdom),-180.,180.)
    zlon3=zmodyx(zconrd*pdocdf(7,kdom),-180.,180.)
    zlon4=zmodyx(zconrd*pdocdf(9,kdom),-180.,180.)
    px=0.25*(zlon1+zlon2+zlon3+zlon4)
  elseif(itype == 6) then
    !
    ! Le domaine est une bande zonale.
    ! On prend comme longitude moyenne 0.
    !
    px=0.
  else
    !
    ! Autres types de domaine.
    ! La longitude est tirée de doc(3).
    !
    px=zmodyx(zconrd*pdocdf(3,kdom),-180.,180.)
  endif
elseif(cdtyp(1:iltyp) == 'DOM') then
  !
  ! L'utilisateur veut en X le numero du domaine.
  !
  px=real(kdom)
elseif(cdtyp(1:iltyp) == 'DIS') then
  !
  ! L'utilisateur veut en X l'abscisse curviligne en km depuis le domaine 1.
  !
  if(itype == 1.or.itype == 4) then
    zx1=pdocdf(3,kdom)
    zarg=min(1.,max(-1.,pdocdf(4,kdom)))
    zy1=asin(zarg)
    zx2=pdocdf(3,1)
    zarg=min(1.,max(-1.,pdocdf(4,1)))
    zy2=asin(zarg)
    px=6371.229*acos(cos(zy1)*cos(zy2)*cos(zx1-zx2) &
&     +sin(zy1)*sin(zy2))
  else
    print*,'COOXD/ERREUR: coordonnee DIS sur mauvaise geom DDH!...'
    print*,itype
    call exit(1)
  endif
else
  print*,'COOXD/ERREUR interne: cdtyp!...'
  print*,cdtyp
  call exit(1)
endif
zmanq=999.999
if(abs(pdocdf(1,kdom)-zmanq)/zmanq < 1.e-6) then
  !
  !-------------------------------------------------
  ! Valeur manquante.
  !-------------------------------------------------
  !
  px=zmanq
endif
end
subroutine stoperr(cdmes)
! --------------------------------------------------------------------------
! **** *STOPERR* Message d'erreur et arret de l'execution.
! --------------------------------------------------------------------------
! Auteur:  J.M.Piriou
! -------
! Modifications.
! --------------
! Original : 91-01
! --------------------------------------------------------------------------
implicit none
character*(*) cdmes
print*,'ERREUR:'
print*,trim(cdmes)
call exit(1)
end
subroutine sorfb(kul,p1,p2,cdc,pchamp)
! --------------------------------------------------------------------------
! **** *sorfb* Sortie formattee des bilans.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   95-01, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
CHARACTER*200 :: CLCHAMP
CHARACTER*200 :: CLFORMAT
INTEGER(KIND=4) :: ILC
INTEGER(KIND=4) :: ILCHAMP
INTEGER(KIND=4) :: KUL
REAL(KIND=8) :: P1
REAL(KIND=8) :: P2
REAL(KIND=8) :: PCHAMP

character*(*) cdc
ilc=len_trim(cdc)
ilc=max(ilc,14)
!
! --------------------------------------------------------------------------
!
! Determination format d'impression en fonction du champ traite.
!
if(cdc(2:3) == 'QL'.or.cdc(2:3) == 'QN'.or.cdc(2:3) == 'QT'.or.cdc(2:3) == 'QV') then
  clchamp='f20.7'
elseif(cdc(2:3) == 'CT') then
  clchamp='f12.2'
else
  clchamp='f20.7'
endif
ilchamp=len_trim(clchamp)
write(clformat,fmt='(3a)') '(a,2f10.4,1x,3a,1x,',clchamp(1:ilchamp),')'
!
! --------------------------------------------------------------------------
!
! Impression.
!
write(kul,fmt=clformat) 'ATM ',p1,p2,cdc(2:3),'-',cdc(1:ilc),pchamp
end
subroutine sorfdta(pcoo,kcoo,pchacf,kulsor)
! --------------------------------------------------------------------------
! **** *sorfdta* Sortie formattee sur les fichiers de données prêtes à tracer.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   96-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
CHARACTER*200 :: CLSOR
INTEGER(KIND=4) :: ILSOR
INTEGER(KIND=4) :: JCOO
INTEGER(KIND=4) :: KCOO
INTEGER(KIND=4) :: KULSOR
REAL(KIND=8) :: PCHACF
REAL(KIND=8) :: ZCHACF
REAL(KIND=8) :: ZVALA

REAL(KIND=8) pcoo(kcoo)
!!
!! Coordonnées.
!!
!do jcoo=1,kcoo
!  call reecar(pcoo(jcoo),-2,1,clsor,ilsor)
!  write(kulsor,fmt='(2a,$)') ' ',clsor(1:ilsor)
!enddo
!
! Valeur du champ.
!
zvala=abs(pchacf)
if(zvala < 1.e-10) then
  !
  ! Afin de ne pas faire planter les logiciels
  ! de tracé par des réels trop faibles en valeur
  ! absolue, on projette iceux sur 0.
  !
  zchacf=0.
else
  zchacf=pchacf
endif
!call reecar(zchacf,-2,1,clsor,ilsor)
!write(kulsor,fmt='(2a)') ' ',clsor(1:ilsor)
write(kulsor,fmt=*) (pcoo(jcoo), jcoo=1,kcoo),zchacf
end
subroutine legc(cdcoo,cdleg,klleg)
! --------------------------------------------------------------------------
! **** *legc* Légende en clair associée à une coordonnée.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   96-11, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
INTEGER(KIND=4) :: ILCOO
INTEGER(KIND=4) :: KLLEG

character*(*) cdcoo,cdleg
ilcoo=len_trim(cdcoo)
if(cdcoo(1:ilcoo) == 'VP') then
  cdleg='pressure (hPa)'
elseif(cdcoo(1:ilcoo) == 'VZ') then
  cdleg='z (km)'
elseif(cdcoo(1:ilcoo) == 'VN') then
  cdleg='levels'
elseif(cdcoo(1:ilcoo) == 'HLAT') then
  cdleg='latitude (deg)'
elseif(cdcoo(1:ilcoo) == 'HLON') then
  cdleg='longitude (deg)'
elseif(cdcoo(1:ilcoo) == 'HDOM') then
  cdleg='domains'
elseif(cdcoo(1:ilcoo) == 'HDIS') then
  cdleg='dist.'
elseif(cdcoo(1:ilcoo) == 'TJ') then
  cdleg='time (days)'
elseif(cdcoo(1:ilcoo) == 'TH') then
  cdleg='time (h)'
elseif(cdcoo(1:ilcoo) == 'TS') then
  cdleg='time (s)'
elseif(cdcoo(1:ilcoo) == 'NON') then
  cdleg=' '
elseif(cdcoo(1:2) == 'BA') then
  cdleg='years'
elseif(cdcoo(1:1) == 'B') then
  cdleg='days'
elseif(cdcoo(1:2) == 'JD') then
  cdleg='julian days'
elseif(cdcoo(1:3) == 'PTS') then
  cdleg='hours'
else
  print*,'DDHI/LEGC/ERREUR: unknown coordinate type!...'
  call exit(1)
endif
klleg=len_trim(cdleg)
end
subroutine ecrm(kulsor,kulm,lddebug,cdsol,psols,kdom,cdunite)
! --------------------------------------------------------------------------
! **** *ECRM*  Ecriture des min/max/moy/unite du champ courant.
! --------------------------------------------------------------------------
! Sujet:
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
! Auteur:           94-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
implicit none
CHARACTER*200 :: CLNOM
INTEGER(KIND=4) :: ILUNITE
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: KDOM
INTEGER(KIND=4) :: KULM
INTEGER(KIND=4) :: KULSOR
LOGICAL :: LDDEBUG

character*(*) cdunite
character*(*) cdsol
REAL(KIND=8) psols(kdom)
REAL(KIND=8) ztempo(4)
!
! Initialisation du tableau d'extremes, ecarts-types, etc...
!
ztempo(1)=1.e20
ztempo(2)=-1.e20
ztempo(3)=0.
ztempo(4)=0.
!
! Determination de ces extremes.
!
do jdom=1,kdom
  ztempo(1)=min(ztempo(1),psols(jdom))
  ztempo(2)=max(ztempo(2),psols(jdom))
  ztempo(3)=ztempo(3)+psols(jdom)
  ztempo(4)=ztempo(4)+psols(jdom)*psols(jdom)
enddo
ztempo(3)=ztempo(3)/real(kdom)
ztempo(4)=sqrt(max(0.,ztempo(4)/real(kdom) &
& -ztempo(3)*ztempo(3)))
!
! Ecriture des resultats sur lfa.
!
write(clnom,fmt='(2a)') 'SOL-LISC-',cdsol(1:index(cdsol,' ')-1)
call lfaecrc(kulsor,clnom,cdunite,1)
write(clnom,fmt='(2a)') 'SOL-MINM-',cdsol(1:index(cdsol,' ')-1)
call lfaecrr(kulsor,clnom,ztempo,4)
write(clnom,fmt='(2a)') 'SOL-TRAC-',cdsol(1:index(cdsol,' ')-1)
call lfaecrr(kulsor,clnom,psols,kdom)
!
! Ecriture des resultats sur fichier en clair.
!
if(lddebug) then
write(kulm,*) '-------------------------------------------------'
write(kulm,*) 'Champ sol ',cdsol(1:index(cdsol,' ')-1),':'
ilunite=len_trim(cdunite)
write(kulm,*) '  UNI:   ',cdunite(1:ilunite)
write(kulm,*) '  MIN:',ztempo(1)
write(kulm,*) '  MAX:',ztempo(2)
write(kulm,*) '  MOY:',ztempo(3)
write(kulm,*) '  ECT:',ztempo(4)
endif
end
subroutine unech(cdunite,pechlog,cdunech,klunech)
! --------------------------------------------------------------------------
! **** *UNECH* Combinaison des unité et échelle en une seule chaîne de caractères.
! --------------------------------------------------------------------------
! Sujet:
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
! Auteur:  97-01, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entrée:
! cdunite: unite.
! pechlog: échelle.
! En sortie:
! cdunech(1:klunech): unite et échelle.
! --------------------------------------------------------------------------
! Exemple:
! cdunite='PA'
! pechlog=0.01
! ================>
! cdunech='PA*0.01'
! klunech=7
! --------------------------------------------------------------------------
implicit none
CHARACTER*200 :: CLECH
INTEGER(KIND=4) :: ILECH
INTEGER(KIND=4) :: ILUNITE
INTEGER(KIND=4) :: KLUNECH
REAL(KIND=8) :: PECHLOG

character*(*) cdunite,cdunech
if(abs(pechlog-1.) > 0.001) then
  !
  ! Conversion du réel pechlog sur 2 chiffres significatifs
  ! en une chaîne de caractères.
  !
  call reecar(pechlog,-1,2,clech,ilech)
  ilunite=len_trim(cdunite)
  write(cdunech,fmt='(3a)') cdunite(1:ilunite),' éch. ',clech(1:ilech)
else
  cdunech=cdunite
endif
klunech=len_trim(cdunech)
end
subroutine ps_et_z500(pcoop1f,pcooz1v,pcoop1v &
& ,pcooz0v,pcoop0v &
& ,klev,kdom,pdocdf,cdfics)
! --------------------------------------------------------------------------
! **** *ps_et_z500* Impression sur output standard de ps et Z500.
! --------------------------------------------------------------------------
! Sujet:
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
! Auteur:  2001-04, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entrée:
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"ddhpar.h"
CHARACTER*200 :: CLFP9000M_FIN
CHARACTER*200 :: CLFP9000M_INI
INTEGER(KIND=4) :: IULP9000M_FIN
INTEGER(KIND=4) :: IULP9000M_INI
INTEGER(KIND=4) :: JDOM
INTEGER(KIND=4) :: JLEV
INTEGER(KIND=4) :: KDOM
INTEGER(KIND=4) :: KLEV
REAL(KIND=8) :: ZINDEF
REAL(KIND=8) :: ZP9000M
REAL(KIND=8) :: ZREF
REAL(KIND=8) :: ZX
REAL(KIND=8) :: ZZ500

REAL(KIND=8) pcooz0v(jplev,jpdom) ! coordonnee verticale z initiale niveau variables.
REAL(KIND=8) pcoop0v(jplev,jpdom) ! coordonnee verticale masse initiale niveau variables.

REAL(KIND=8) pcoop1f(jplev2,jpdom) ! coordonnee verticale masse finale niveau flux.
REAL(KIND=8) pcooz1v(jplev,jpdom) ! coordonnee verticale z finale niveau variables.
REAL(KIND=8) pcoop1v(jplev,jpdom) ! coordonnee verticale masse finale niveau variables.

REAL(KIND=8) pdocdf(jpdoc,jpdom) ! articles de documentation des domaines (tableau tota

character*(*) cdfics

zindef=3.85421521
iulp9000m_ini=40 ; clfp9000m_ini=cdfics(1:len_trim(cdfics))//'.p9000m_ini.dta'
iulp9000m_fin=41 ; clfp9000m_fin=cdfics(1:len_trim(cdfics))//'.p9000m_fin.dta'
open(iulp9000m_ini,file=clfp9000m_ini,form='formatted')
open(iulp9000m_fin,file=clfp9000m_fin,form='formatted')
do jdom=1,kdom
  !
  !-------------------------------------------------
  ! Z500: on l'initialise à une valeur indéfinie,
  ! pour controler ultérieurement s'il a bien été initialisé.
  !-------------------------------------------------
  !
  zz500=zindef
  zref=500. ! p ref en hPa.
  !
  ! Boucle de recherche de Z500.
  !
  do jlev=1,klev-1
    if(pcoop1v(jlev,jdom) <= zref .and. pcoop1v(jlev+1,jdom) > zref) then
      zz500=(pcooz1v(jlev+1,jdom)-pcooz1v(jlev,jdom)) &
      & /(pcoop1v(jlev+1,jdom)-pcoop1v(jlev,jdom)) &
      & *(zref-pcoop1v(jlev,jdom)) &
      & +pcooz1v(jlev,jdom)
    endif
  enddo
  !
  !-------------------------------------------------
  ! p à 9000 m INITIALE: on l'initialise à une valeur indéfinie,
  ! pour controler ultérieurement s'il a bien été initialisé.
  !-------------------------------------------------
  !
  zp9000m=zindef
  zref=9000./1000. ! hauteur en km.
  !
  ! Boucle de recherche de p à 9000 m.
  !
  do jlev=1,klev-1
    if(pcooz0v(jlev,jdom) >= zref .and. pcooz0v(jlev+1,jdom) < zref) then
      zp9000m=(pcoop0v(jlev+1,jdom)-pcoop0v(jlev,jdom)) &
      & /(pcooz0v(jlev+1,jdom)-pcooz0v(jlev,jdom)) &
      & *(zref-pcooz0v(jlev,jdom)) &
      & +pcoop0v(jlev,jdom)
    endif
  enddo
  !
  ! Ecriture sur fichier de p9000m(latitude).
  !
  call cooxd('LAT',pdocdf,jdom,zx)
  write(iulp9000m_ini,'(9g16.7)') zx,zp9000m
  !
  !-------------------------------------------------
  ! p à 9000 m FINALE: on l'initialise à une valeur indéfinie,
  ! pour controler ultérieurement s'il a bien été initialisé.
  !-------------------------------------------------
  !
  zp9000m=zindef
  zref=9000./1000. ! hauteur en km.
  !
  ! Boucle de recherche de p à 9000 m.
  !
  do jlev=1,klev-1
    if(pcooz1v(jlev,jdom) >= zref .and. pcooz1v(jlev+1,jdom) < zref) then
      zp9000m=(pcoop1v(jlev+1,jdom)-pcoop1v(jlev,jdom)) &
      & /(pcooz1v(jlev+1,jdom)-pcooz1v(jlev,jdom)) &
      & *(zref-pcooz1v(jlev,jdom)) &
      & +pcoop1v(jlev,jdom)
    endif
  enddo
  !
  ! Ecriture sur fichier de p9000m(latitude).
  !
  call cooxd('LAT',pdocdf,jdom,zx)
  write(iulp9000m_fin,'(9g16.7)') zx,zp9000m
  !
  !-------------------------------------------------
  ! Impression du résultat sur output standard.
  !-------------------------------------------------
  !
  if(jdom == 1) then
    if(zp9000m /= zindef) print*,'p à 9000 m finale domaine ',jdom,' = ',zp9000m,' hPa'
    print*,'ps         finale domaine ',jdom,' = ',pcoop1f(klev+1,jdom),' hPa'
    if(zz500 /= zindef) print*,'Z500       finale domaine ',jdom,' = ',1000.*zz500,' m'
  endif
enddo
end
