subroutine lega(pvalmin,pvalmax,pret,kprinc,pprinc,ksec,psec)
! --------------------------------------------------------------
! **** ** LEGende des Axes.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   pvalmin,pvalmax: min et max de l'axe.
!   pret: écart entre 2 tracés de réticule. Si cet écart est égal à rindef l'écart est estimé par la présente routine.
! En sortie:
!   kprinc lignes principales à tracer, de valeur pprinc(1, ..., kprinc).
!   ksec lignes secondaires à tracer, de valeur psec(1, ..., ksec).
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
integer(kind=4) ichoix(9)
real(kind=8) pprinc(jplignes)
real(kind=8) psec(jplignes)
!
!-------------------------------------------------
! On préfère lire des écarts de 1, puis un peu moins bien des écarts de 2, puis
! un peu moins bien des écarts de 5, puis de 4, etc.
! Le tableau ci-dessous sert à sélecter si l'écart sur lequel on tombe est plus
! lisible que le précédent.
!-------------------------------------------------
!
ichoix(1)=1
ichoix(2)=2
ichoix(3)=5
ichoix(4)=4
ichoix(5)=3
ichoix(6)=6
ichoix(7)=7
ichoix(8)=8
ichoix(9)=9
!
!-------------------------------------------------
! Etendue.
!-------------------------------------------------
!
zec=abs(pvalmax-pvalmin)
!
!-------------------------------------------------
! On va faire entre 4 et 8 lignes principales, puis chacune sera divisée en 2, 3
! ou 4 lignes secondaires suivant besoin.
!-------------------------------------------------
!
zcritere_maxi=0.
do jligp=8,4,-1
  zintlp=zec/real(jligp)
  !
  !-------------------------------------------------
  ! Arrondi à un seul chiffre significatif.
  !-------------------------------------------------
  !
  call arrr(zintlp,1,zintlp1)
  !
  !-------------------------------------------------
  ! imant: entier associé au premier chiffre significatif de la mantisse (ex: 6 pour 0.006).
  !-------------------------------------------------
  !
  zlis=zintlp1/10.**(nint(log(zintlp1)/log(10.)-0.5))
  if(zlis >= 10.) then
    zlis=zlis/10.
  elseif(zlis < 1.) then
    zlis=zlis*10.
  endif
  imant=nint(zlis)
  !
  !-------------------------------------------------
  ! Combien y a-t-il d'intervalles principaux de largeur zintlp1?
  !-------------------------------------------------
  !
  ilig=int(zec/zintlp1)
  !
  !-------------------------------------------------
  ! Pour choisir si on met des intervalles principaux tous les 1, 2, 3, etc, on applique un critère de choix: on préfère coter tous les 1 2 ou 5 (plus lisible) que tous les 7 par exemple, ce qu'on traduit ci-dessous en zcritere plus ou moins élevés.
  !-------------------------------------------------
  !
  !zcritere=1./real(ichoix(imant)) ! critère basé sur les chiffres d'écart préférés (ex: 1 plus lisible que 2, lui-même plus lisible que 5, etc).
  zcritere=real(ilig) ! critère: avoir le plus d'intervalles principaux possible.
  !
  !-------------------------------------------------
  ! Si zec/zintlp1 est quasiment un entier, cela veut dire que imant est tel que les graduations vont tomber juste, elles sont un diviseur de la largeur zec. On augmente la note de ce imant-là.
  !-------------------------------------------------
  !
  zfrac=zec/zintlp1-int(zec/zintlp1)
  if(zfrac < 0.001 .or. zfrac > 0.999) then
    zcritere=zcritere*4.
  endif
  !if(imant == 1 .or. imant == 2 .or. imant == 5) then
  !  zcritere=100.+real(ilig)
  !elseif(imant == 3 .or. imant == 4) then
  !  zcritere=50.+real(ilig)
  !else
  !  zcritere=20.+real(ilig)
  !endif
  if(pret /= rindef) then
    !
    !-------------------------------------------------
    ! L'utilisateur a forcé l'écart entre 2 tracés de réticule.
    !-------------------------------------------------
    !
    !
    !-------------------------------------------------
    ! Arrondi à un seul chiffre significatif.
    !-------------------------------------------------
    !
    call arrr(pret,1,zintret)
    !
    !-------------------------------------------------
    ! imant: entier associé au premier chiffre significatif de la mantisse (ex: 6 pour 0.006).
    !-------------------------------------------------
    !
    zlis=zintret/10.**(nint(log(zintret)/log(10.)-0.5))
    if(zlis >= 10.) then
      zlis=zlis/10.
    elseif(zlis < 1.) then
      zlis=zlis*10.
    endif
    imant_princ=nint(zlis)
    zecp=pret
    ilig=nint(zec/zecp)
  elseif(abs(pvalmax-pvalmin-2500.) < 1.) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 2500. on veut mettre des intervalles principaux tous les 500..
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=500.
    ilig=nint(2500./zecp)
  elseif(abs(pvalmax-pvalmin-0.02)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 0.02 on veut mettre des intervalles principaux tous les 0.005. Cas de l'article Piriou and Guérémy 2018.
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=0.005
    ilig=nint(0.02/zecp)
  elseif(abs(pvalmax-pvalmin-30.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 30. on veut mettre des intervalles principaux tous les 6.. Cas de l'article PCMT figure convective Q1 ou Q2 dans BOMEX.
    !-------------------------------------------------
    !
    imant_princ=6
    zecp=6.
    ilig=nint(30./zecp)
  elseif(abs(pvalmax-pvalmin-0.1)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 0.1 on veut mettre des intervalles principaux tous les 0.02.
    !-------------------------------------------------
    !
    imant_princ=2
    zecp=0.02
    ilig=nint(0.1/zecp)
  elseif(abs(pvalmax-pvalmin-0.03)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 0.03 on veut mettre des intervalles principaux tous les 0.01. Cas de l'article PCMT figure convective mass flux dans BOMEX.
    !-------------------------------------------------
    !
    imant_princ=1
    zecp=0.01
    ilig=nint(0.03/zecp)
  elseif(abs(pvalmax-pvalmin-4.5)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 4.5 on veut mettre des intervalles principaux tous les 0.5. Cas de l'article Bechtold et al. 2000 figure 6. b).
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=0.5
    ilig=nint(4.5/zecp)
  elseif(abs(pvalmax-pvalmin-3.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 3. on veut mettre des intervalles principaux tous les 0.5.
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=0.5
    ilig=nint(3./zecp)
  elseif(abs(pvalmax-pvalmin-2.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 2. on veut mettre des intervalles principaux tous les 0.5.
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=0.5
    ilig=nint(2./zecp)
!  elseif(abs(pvalmax-pvalmin-16.)/abs(pvalmax-pvalmin) < 0.05) then
!    !
!    !-------------------------------------------------
!    ! Si l'écart est de 16. on veut mettre des intervalles principaux tous les 4.. Cas de l'article Siebesma and Cuijpers 1995.
!    !-------------------------------------------------
!    !
!    imant_princ=4
!    zecp=4.
!    ilig=nint(16./zecp)
  elseif(abs(pvalmax-pvalmin-8.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 8. on veut mettre des intervalles principaux tous les 1..
    !-------------------------------------------------
    !
    imant_princ=1
    zecp=1.
    ilig=nint(8./zecp)
  elseif(abs(pvalmax-pvalmin-20.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 20. on veut mettre des intervalles principaux tous les 2.. Cas de l'article Siebesma and Cuijpers 1995.
    !-------------------------------------------------
    !
    imant_princ=2
    zecp=2.
    ilig=nint(20./zecp)
  elseif(abs(pvalmax-pvalmin-0.5)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 0.5 on veut mettre des intervalles principaux tous les 0.1.
    !-------------------------------------------------
    !
    imant_princ=5
    zecp=0.1
    ilig=nint(0.5/zecp)
  elseif(abs(pvalmax-pvalmin-30.)/abs(pvalmax-pvalmin) < 0.05) then
    !
    !-------------------------------------------------
    ! Si l'écart est de 30. on veut mettre des intervalles principaux tous les 2..
    !-------------------------------------------------
    !
    if(lgpubli) then
      imant_princ=5
      zecp=5.
      ilig=nint(30./zecp)
    else
      imant_princ=2
      zecp=2.
      ilig=nint(30./zecp)
    endif
  elseif(zcritere > zcritere_maxi) then
    !
    !-------------------------------------------------
    ! L'écart courant est plus lisible que le précédent.
    !-------------------------------------------------
    !
    zcritere_maxi=zcritere
    imant_princ=imant
    zecp=zintlp1 ! zecp: écart entre deux graduations principales.
    !
    !-------------------------------------------------
    ! On mémorise combien il y a d'intervalles principaux de largeur zintlp1.
    !-------------------------------------------------
    !
    if(lgdebu) write(*,fmt=*) 'lega: ',ilig,' intervalles principaux de largeur ',zecp,imant_princ
  endif
enddo
!
!-------------------------------------------------
! S'il n'y a que 3 ou 4 intervalles principaux on met des intervalles secondaires.
!-------------------------------------------------
!
if(ilig <= 8) then
  if(imant_princ == 1) then
    !
    !-------------------------------------------------
    ! La mantisse de l'écart principal est 1 (ex: écarts de 0.1, 1., 10.,
    ! 100., etc). On divise en deux cet écart principal.
    !-------------------------------------------------
    !
    iligs=2
  else
    !
    !-------------------------------------------------
    ! Dans les autres cas on divise l'écart principal en imant_princ intervalles
    ! secondaires. Ainsi chaque intervalle secondaire sera un écart dont la
    ! mantisse est 1. (ex: écarts de 0.1, 1., 10., 100., etc).
    !-------------------------------------------------
    !
    iligs=imant_princ
  endif
else
  iligs=1
endif
if(lgdebu) write(*,fmt=*) ' '
if(lgdebu) write(*,fmt=*) 'ilig,imant_princ= ',ilig,imant_princ
! zecs: écart entre deux graduations secondaires.
zecs=zecp/real(iligs)
if(lgdebu) write(*,fmt=*) 'lega: Chaque intervalle principal sera divisé en ',iligs,' intervalles secondaires.'
!
!-------------------------------------------------
! Maintenant que les choix sont faits (kprinc lignes principales, ksec lignes
! secondaires), on initialise la valeur réelle à pointer en face de chaque ligne.
!-------------------------------------------------
!

idepart=nint(pvalmin/zecp-0.5)
zdepart=real(idepart)*zecp
if(zdepart < pvalmin-0.0001*zecp) then
  zdepart=zdepart+zecp
  idepart=idepart+1
elseif(zdepart > pvalmin+0.9999*zecp) then
  zdepart=zdepart-zecp
  idepart=idepart-1
endif

ifin=nint(pvalmax/zecp-0.5)
zfin=real(ifin)*zecp
if(zfin > pvalmax+0.0001*zecp) then
  zfin=zfin-zecp
  ifin=ifin-1
elseif(zfin < pvalmax-0.9999*zecp) then
  zfin=zfin+zecp
  ifin=ifin+1
endif

kprinc=0
if(idepart > ifin) then
  isens=-1
else
  isens=1
endif
do jprinc=idepart,ifin,isens
  kprinc=kprinc+1
  if(kprinc > jplignes) then
    write(*,fmt=*)
    write(*,fmt=*) 'lega: dd2gr/ERREUR: recompiler avec une valeur plus grande de jplignes!...'
    write(*,fmt=*)
    call exit(1)
  endif
  pprinc(kprinc)=zdepart+real(jprinc-idepart)*zecp
  if(lgdebu) write(*,fmt=*) 'lega: ligne principale n° ',kprinc,' : cote ',pprinc(kprinc)
enddo
!
!-------------------------------------------------
! Lignes secondaires.
!-------------------------------------------------
!
if(lgdebu) write(*,fmt=*) ' '

idepart=nint(pvalmin/zecs-0.5)
zdepart=real(idepart)*zecs
if(zdepart < pvalmin) then
  zdepart=zdepart+zecs
  idepart=idepart+1
endif

ifin=nint(pvalmax/zecs-0.5)
zfin=real(ifin)*zecs
if(zfin > pvalmax) then
  zfin=zfin-zecs
  ifin=ifin-1
endif

ksec=0
if(idepart > ifin) then
  isens=-1
else
  isens=1
endif
do jsec=idepart,ifin
  ksec=ksec+1
  if(ksec > jplignes) then
    write(*,fmt=*)
    write(*,fmt=*) 'lega: dd2gr/ERREUR: recompiler avec une valeur plus grande de jplignes!...'
    write(*,fmt=*)
    call exit(1)
  endif
  psec(ksec)=zdepart+real(jsec-idepart)*zecs
  if(lgdebu) write(*,fmt=*) 'lega: ligne secondaire n° ',ksec,' : cote ',psec(ksec)
enddo
end
