subroutine hayashi_shallow_water
! --------------------------------------------------------------
! **** *hayashi_shallow_water*
! --------------------------------------------------------------
! Sujet:
!   Dans le cadre d'un diagramme Hayashi, tracé des courbes-linéaments de la dispersion en shallow-water.
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
!	écriture sur le fichier SVG, d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
integer(kind=4), parameter :: jpbris=800
real(kind=8) :: zfracx(jpbris)
real(kind=8) :: zfracy(jpbris)
write(*,fmt=*) ' '
write(*,fmt=*) '  Tracé des courbes de dispersion du diagramme nombre d''onde-fréquence... '
write(nulsvg,fmt='(9a)') ' '
write(nulsvg,fmt=*) '<!-- Tracé des courbes de dispersion du diagramme nombre d''onde-fréquence. -->'
llplusse=.false.
!
!-------------------------------------------------
! Couleur des courbes-linéaments.
!-------------------------------------------------
!
clcoul='brown'
clcoul='rgb(30,30,30)'
clcoul='blue'
clcoul='rgb(30,30,200)'
clcoul='rgb(30,30,100)'
clfonte='font-family="URW Gothic L"'
clfonte='font-family="Arial"'
clcoulpp=clcoul
clcoulcadre='white'
if(lgpubli) then
  zpub=1.6
else
  zpub=1.
endif
ztaif=0.75*rgtaille_fonte*rfont_axes*zpub
llcadre=.true.
!
!-------------------------------------------------
! Pointillé ou continu.
!-------------------------------------------------
!
clpoin='0'
!
!-------------------------------------------------
! Largeur de trait.
!-------------------------------------------------
!
zwidth=1.7e-3*rlxsvg*zpub
zseuil=0.
zk_cpc_x=rxmax+0.05*(rxmax-rxmin) ! nombre d'onde (en Cycles Par Circonférence terrestre) situé plus à droite que le graphique, pour que les courbes-linéaments soient tracées jusqu'aux bords du domaine de tracé.
if(trim(cgformat) == 'HAS') then
  !
  !-------------------------------------------------
  ! Diagramme symétrique.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Droite "12 m" des ondes de Kelvin.
  !-------------------------------------------------
  !
  ibris=2
  zh=12. ! épaisseur (m) en shallow water.
  zbide=15. ! nombre d'onde en cycles par circonférence terrestre. Sa valeur importe peu car la pente n'en dépend pas.
  zpente=zomega_cpd_kelvin(zbide,zh)/zbide
  zfracx(1)=(0.-rxmin)/(rxmax-rxmin)
  zfracy(1)=(0.-rymin)/(rymax-rymin)
  zfracx(2)=(zk_cpc_x-rxmin)/(rxmax-rxmin)
  zfracy(2)=(zk_cpc_x*zpente-rymin)/(rymax-rymin)
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Droite "12 m" des ondes de Kelvin. -->'
  call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
  !
  !-------------------------------------------------
  ! Droite "25 m" des ondes de Kelvin.
  !-------------------------------------------------
  !
  ibris=2
  zh=25. ! épaisseur (m) en shallow water.
  zbide=15. ! nombre d'onde en cycles par circonférence terrestre. Sa valeur importe peu car la pente n'en dépend pas.
  zpente=zomega_cpd_kelvin(zbide,zh)/zbide
  zfracx(1)=(0.-rxmin)/(rxmax-rxmin)
  zfracy(1)=(0.-rymin)/(rymax-rymin)
  zfracx(2)=(zk_cpc_x-rxmin)/(rxmax-rxmin)
  zfracy(2)=(zk_cpc_x*zpente-rymin)/(rymax-rymin)
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Droite "25 m" des ondes de Kelvin. -->'
  call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
  !
  !-------------------------------------------------
  ! Ecriture du texte "Kelvin".
  !-------------------------------------------------
  !
  cltxt='Kelvin'
  zxtxt=11.5
  zytxt=zpente*zxtxt
  call svg_texte(cltxt,clfonte,ztaif,clcoulpp,zxtxt,zytxt,llcadre,clcoulcadre)
  !
  !-------------------------------------------------
  ! Droite "50 m" des ondes de Kelvin.
  !-------------------------------------------------
  !
  ibris=2
  zh=50. ! épaisseur (m) en shallow water.
  zbide=15. ! nombre d'onde en cycles par circonférence terrestre. Sa valeur importe peu car la pente n'en dépend pas.
  zpente=zomega_cpd_kelvin(zbide,zh)/zbide
  zfracx(1)=(0.-rxmin)/(rxmax-rxmin)
  zfracy(1)=(0.-rymin)/(rymax-rymin)
  zfracx(2)=(zk_cpc_x-rxmin)/(rxmax-rxmin)
  zfracy(2)=(zk_cpc_x*zpente-rymin)/(rymax-rymin)
  write(nulsvg,fmt='(9a)') ' '
  write(nulsvg,fmt='(9a)') '<!-- Droite "50 m" des ondes de Kelvin. -->'
  call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
  !
  !-------------------------------------------------
  ! Courbes des ondes de Rossby équatoriales approchées (ER dans la littérature: Equatorial Rossby).
  !-------------------------------------------------
  !
  itrac=40
  do jh=1,3
    ! valeurs pertinentes de zh: entre 8m et 90m, ex: 12, 25, 50.
    if(jh == 1) then
      zh=50.
    elseif(jh == 2) then
      zh=25.
    else
      zh=12.
    endif
    do jn=1,1
      ibris=0
      do jtrac=1,itrac
        zfrac=real(jtrac-1)/real(itrac-1)
        zk=rxmin+(0.-rxmin)*zfrac
        ibris=ibris+1
        if(ibris > jpbris) then
          write(*,fmt=*)
          write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpbris !...'
          call exit(1)
        endif
        zfracx(ibris)=(zk-rxmin)/(rxmax-rxmin)
        zfracy(ibris)=(zomega_cpd_rossby(zk,jn,zh)-rymin)/(rymax-rymin)
      enddo
      write(nulsvg,fmt='(9a)') ' '
      write(nulsvg,fmt=*) '<!-- Courbe de l''onde équatoriale de Rossby, h=',zh,'. -->'
      call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Ecriture du texte "n=1 ER".
  !-------------------------------------------------
  !
  cltxt='n=1 ER'
  zxtxt=-8.
  zytxt=0.07
  call svg_texte(cltxt,clfonte,ztaif,clcoulpp,zxtxt,zytxt,llcadre,clcoulcadre)
  !
  !-------------------------------------------------
  ! Courbes des ondes d'inertie-gravité (IG dans la littérature)
  ! par la méthode de Newton appliquée à la fonction cubique zschrodinger.
  !-------------------------------------------------
  !
  itrac=80
  do jh=1,3
    ! valeurs pertinentes de zh: entre 8m et 90m, ex: 12, 25, 50.
    if(jh == 1) then
      zh=50.
    elseif(jh == 2) then
      zh=25.
    else
      zh=12.
    endif
    do jn=1,1
      ibris=0
      do jtrac=1,itrac
        zfrac=real(jtrac-1)/real(itrac-1)
        zk=rxmin+(rxmax-rxmin)*zfrac
        ibris=ibris+1
        if(ibris > jpbris) then
          write(*,fmt=*)
          write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpbris !...'
          call exit(1)
        endif
        zomega_cpd=0.4 ! ébauche de la boucle de Newton.
        zfracx(ibris)=(zk-rxmin)/(rxmax-rxmin)
        zfracy(ibris)=(znewton(zomega_cpd,zk,jn,zh)-rymin)/(rymax-rymin)
      enddo
      write(nulsvg,fmt='(9a)') ' '
      write(nulsvg,fmt=*) '<!-- Courbe de l''onde inertie-gravité, h=',zh,'. -->'
      call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Ecriture du texte "n=1 IG".
  !-------------------------------------------------
  !
  cltxt='n=1 IG'
  zxtxt=-2.5
  zytxt=0.447
  call svg_texte(cltxt,clfonte,ztaif,clcoulpp,zxtxt,zytxt,llcadre,clcoulcadre)
else ! trim(cgformat) /= 'HAS'
  !
  !-------------------------------------------------
  ! Diagramme asymétrique.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Courbes des ondes mixtes Rossby-gravité (MRG dans la littérature)
  ! par la méthode de Newton appliquée à la fonction cubique zschrodinger.
  !-------------------------------------------------
  !
  itrac=80
  do jh=1,3
    ! valeurs pertinentes de zh: entre 8m et 90m, ex: 12, 25, 50.
    if(jh == 1) then
      zh=50.
    elseif(jh == 2) then
      zh=25.
    else
      zh=12.
    endif
    do jn=0,0
      ibris=0
      do jtrac=1,itrac
        zfrac=real(jtrac-1)/real(itrac-1)
        zk=rxmin+(rxmax-rxmin)*zfrac
        ibris=ibris+1
        if(ibris > jpbris) then
          write(*,fmt=*)
          write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpbris !...'
          call exit(1)
        endif
        if(jtrac == 1) then
          zomega_cpd=0.10 ! ébauche de la boucle de Newton.
        else
          zomega_cpd=zsol ! ébauche de la boucle de Newton: la solution du zk précédent.
        endif
        zfracx(ibris)=(zk-rxmin)/(rxmax-rxmin)
        zsol=znewton(zomega_cpd,zk,jn,zh)
        zfracy(ibris)=(zsol-rymin)/(rymax-rymin)
      enddo
      write(nulsvg,fmt='(9a)') ' '
      write(nulsvg,fmt=*) '<!-- Courbe de l''onde mixte Rossby-gravité, h=',zh,'. -->'
      call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Ecriture du texte "n=0 MRG".
  !-------------------------------------------------
  !
  cltxt='n=0 MRG'
  zxtxt=-6.0
  zytxt=0.18
  call svg_texte(cltxt,clfonte,ztaif,clcoulpp,zxtxt,zytxt,llcadre,clcoulcadre)
  !
  !-------------------------------------------------
  ! Courbes des ondes d'inertie-gravité (IG dans la littérature)
  ! par la méthode de Newton appliquée à la fonction cubique zschrodinger.
  !-------------------------------------------------
  !
  itrac=140
  do jh=3,1,-1
    ! valeurs pertinentes de zh: entre 8m et 90m, ex: 12, 25, 50.
    if(jh == 1) then
      zh=50.
    elseif(jh == 2) then
      zh=25.
    else
      zh=12.
    endif
    do jn=2,2
      ibris=0
      do jtrac=1,itrac
        zfrac=real(jtrac-1)/real(itrac-1)
        zk=rxmin+(rxmax-rxmin)*zfrac
        ibris=ibris+1
        if(ibris > jpbris) then
          write(*,fmt=*)
          write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpbris !...'
          call exit(1)
        endif
        if(jtrac == 1) then
          zomega_cpd=0.58 ! ébauche de la boucle de Newton.
        else
          zomega_cpd=zsol ! ébauche de la boucle de Newton: la solution du zk précédent.
        endif
        zfracx(ibris)=(zk-rxmin)/(rxmax-rxmin)
        zsol=znewton(zomega_cpd,zk,jn,zh)
        zfracy(ibris)=(zsol-rymin)/(rymax-rymin)
      enddo
      write(nulsvg,fmt='(9a)') ' '
      write(nulsvg,fmt=*) '<!-- Courbe de l''onde inertie-gravité, h=',zh,'. -->'
      call svg_ligne_polygonale(zfracx,zfracy,ibris,rindef,clcoul,llplusse,clpoin,zwidth,zseuil)
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Ecriture du texte "n=0 MRG".
  !-------------------------------------------------
  !
  cltxt='n=2 IG'
  if(rymax < 0.55) then
    !
    !-------------------------------------------------
    ! Une seule courbe des 3 MRG apparaît dans le graphique.
    ! On met le cadre sur celle du bas.
    !-------------------------------------------------
    !
    zxtxt=-3.0
    zytxt=0.485
  else
    !
    !-------------------------------------------------
    ! Plusieurs courbes des 3 MRG apparaissent dans le graphique.
    ! On met le cadre sur celle du milieu.
    !-------------------------------------------------
    !
    zxtxt=-3.0
    zytxt=0.58
  endif
  call svg_texte(cltxt,clfonte,ztaif,clcoulpp,zxtxt,zytxt,llcadre,clcoulcadre)
endif
end
function zomega_cpd_kelvin(pk_cpc,ph)
! --------------------------------------------------------------
! **** **
! --------------------------------------------------------------
! Sujet:
!   Calcul de oméga (en cycles par jour, CPD) pour les ondes de Kelvin, en fonction de k (en cycles par circonférence terrestre, CPC).
! Arguments explicites:
! Arguments implicites:
! Methode:
!   MC Wheeler and Hahn Nguyen 2015.
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
real(kind=8) :: pk_cpc ! nombre d'onde en cycles par circonférence terrestre.
real(kind=8) :: ph ! épaisseur en shallow water.
real(kind=8) :: zomega_cpd_kelvin
zra=6371000. ! rayon terrestre.
zk=pk_cpc/zra ! k en m^-1.
zg=9.80665
zomega=zk*sqrt(zg*ph) ! oméga en s^-1.
zpi=4.*atan(1.)
zduree=86400. ! durée du jour en s.
zomega_cpd_kelvin=zomega/(2.*zpi)*zduree
end
function zomega_cpd_rossby(pk_cpc,kn,ph)
! --------------------------------------------------------------
! **** **
! --------------------------------------------------------------
! Sujet:
!   Calcul de oméga (en cycles par jour, CPD) pour les ondes de Rossby équatoriales, en fonction de k (en cycles par circonférence terrestre, CPC).
! Arguments explicites:
! Arguments implicites:
! Methode:
!   Equation [14] de MC Wheeler and Hahn Nguyen 2015.
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications
! --------------------------------------------------------------
! En entree:
!   pk_cpc: nombre d'onde en cycles par circonférence terrestre.
!   ph: épaisseur en shallow water.
!   kn: mode l'onde.
! En sortie:
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: pk_cpc
integer(kind=4), intent(in) :: kn
real(kind=8), intent(in) :: ph
real(kind=8) :: zomega_cpd_rossby
zra=6371000. ! rayon terrestre.
zpi=4.*atan(1.)
zlat=0.
zomega_terre=2.*zpi/86164. ; zbeta=2.*zomega_terre/zra*cos(zlat)
zk=pk_cpc/zra ! k en m^-1.
zg=9.80665
zomega=-zbeta*zk/(zk*zk+real(2*kn+1)*zbeta/sqrt(zg*ph)) ! oméga en s^-1.
zduree=86400. ! durée du jour en s.
zomega_cpd_rossby=zomega/(2.*zpi)*zduree
end
function zschrodinger(pomega_cpd,pk_cpc,kn,ph)
! --------------------------------------------------------------
! **** **
! --------------------------------------------------------------
! Sujet:
!   Fonction de Schrödinger des ondes équatoriales.
! Arguments explicites:
! Arguments implicites:
! Methode:
!   Equation [13] de MC Wheeler and Hahn Nguyen 2015.
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications
! --------------------------------------------------------------
! En entree:
!   pk_cpc: nombre d'onde en cycles par circonférence terrestre.
!   ph: épaisseur en shallow water.
!   kn: mode l'onde.
! En sortie:
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: pomega_cpd
real(kind=8), intent(in) :: pk_cpc
integer(kind=4), intent(in) :: kn
real(kind=8), intent(in) :: ph
real(kind=8) :: zschrodinger
zra=6371000. ! rayon terrestre.
zpi=4.*atan(1.)
zlat=0.
zomega_terre=2.*zpi/86164. ; zbeta=2.*zomega_terre/zra*cos(zlat)
zg=9.80665
zk=pk_cpc/zra ! k en m^-1.
zomega=pomega_cpd*2.*zpi/86400.
zschrodinger=sqrt(zg*ph)/zbeta*(zomega*zomega/zg/ph-zk*zk-zk/zomega*zbeta)-real(2*kn+1)
end
function znewton(pomega_cpd,pk_cpc,kn,ph)
! --------------------------------------------------------------
! **** **
! --------------------------------------------------------------
! Sujet:
!   Résolution en oméga de l'équation de Schrödinger, par la méthode de Newton, avec pour ébauche pomega_cpd.
! Arguments explicites:
! Arguments implicites:
! Methode:
!   Equation [13] de MC Wheeler and Hahn Nguyen 2015.
! Externes:
! Auteur:   2018-03, J.M. Piriou.
! Modifications
! --------------------------------------------------------------
! En entree:
!   pomega_cpd: fréquence en cycles par jour.
!   pk_cpc: nombre d'onde en cycles par circonférence terrestre.
!   ph: épaisseur en shallow water.
!   kn: mode l'onde.
! En sortie:
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
real(kind=8), intent(in) :: pomega_cpd
real(kind=8), intent(in) :: pk_cpc
integer(kind=4), intent(in) :: kn
real(kind=8), intent(in) :: ph
real(kind=8) :: znewton
zomega_cpd=pomega_cpd
zdom=0.001 ! écart de oméga en CPD (Cycles Per Day).
!
!-------------------------------------------------
! Boucle de Newton résolvant en zomega_cpd l'équation zschrodinger=0..
!-------------------------------------------------
!
do jit=1,8
  zf=zschrodinger(zomega_cpd,pk_cpc,kn,ph)
  zf2=zschrodinger(zomega_cpd+zdom,pk_cpc,kn,ph)
  zderi=(zf2-zf)/zdom
  zomega_cpd=zomega_cpd-zf/zderi
enddo
znewton=zomega_cpd
end
