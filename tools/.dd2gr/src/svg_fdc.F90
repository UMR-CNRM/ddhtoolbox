subroutine svg_fdc(kximage,kyimage,kxdecal,kydecal,kxpix,kypix)
! --------------------------------------------------------------
! **** *svg_fdc* Superposition d'un fond de carte (exemple: contour des continents) sur une image SVG.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2015-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kximage,kyimage: tailles X,Y de l'image de sortie.
!	kxdecal,kydecal: nombre de pixels en X et Y dont la zone de tracé est décalée au sein de l'image totale (i.e. celle avec ses titres et légendes).
!	kx,ky: dimension des données à tracer.
! En entrée/sortie:
!  on écrit sur le fichier SVG d'unité logique nulsvg.
! --------------------------------------------------------------
!
!
use parametres
#include"implicit_r8i4.h"
!
integer(kind=4) :: irvb_trait(3)
real(kind=8), allocatable :: zfracx(:),zfracy(:)
character(len=400) :: clmot(40)
!
!-------------------------------------------------
! Fichier fond de carte par défaut.
!-------------------------------------------------
!
call getenv('FDC',clfic_def)
call getenv('HOME',clhome)
if(clfic_def == ' ') then
  !
  !-------------------------------------------------
  ! On propose un fdc par défaut, dépendant du domaine LLV courant.
  !-------------------------------------------------
  !
  if(rxmin > -10. .and. rxmax < 10. .and. rymin > 41. .and. rymax < 53.) then
    !
    !-------------------------------------------------
    ! Domaine France.
    !-------------------------------------------------
    !
    clfic_def=trim(clhome)//'/.dd2gr/fdc/france_departements_07km'
  elseif(abs(rxmax-rxmin) < 40.) then
    clfic_def=trim(clhome)//'/.dd2gr/fdc/global_03km'
  elseif(abs(rxmax-rxmin) < 80.) then
    clfic_def=trim(clhome)//'/.dd2gr/fdc/global_10km'
  elseif(abs(rxmax-rxmin) < 120.) then
    clfic_def=trim(clhome)//'/.dd2gr/fdc/global_17km'
  else
    clfic_def=trim(clhome)//'/.dd2gr/fdc/global_29km'
  endif
endif
if(nfdc == 0) then
  !
  !-------------------------------------------------
  ! L'utilisateur n'a pas spécifié
  ! le nom du fichier-fond de carte. On met une valeur par défaut,
  ! lue sur la variable d'environnement FDC.
  !-------------------------------------------------
  !
  nfdc=1
  cgfdc(nfdc)='#FDC FIC='//trim(clfic_def)
elseif(cgfdc(1)(1:5) == '#FDC=') then
  !
  !-------------------------------------------------
  ! Ancienne syntaxe de spécification d'un fond de carte.
  ! On la traite ici pour compatibilité ascendante.
  !-------------------------------------------------
  !
  nfdc=1
  cgfdc(nfdc)='#FDC FIC='//trim(cgfdc(1)(6:))
endif
!
!-------------------------------------------------
! Boucle sur les fichiers fond de carte à tracer.
!-------------------------------------------------
!
do jfdc=1,nfdc
  zfdc_epais=1.5 ! épaisseur par défaut de la ligne fdc, en pixels.
  clcoul='black' ! couleur par défaut de la ligne fdc.
  clfic=clfic_def
  llplusse=.false.
  !
  !-------------------------------------------------
  ! La demande utilisateur est du type:
  ! #FDC FIC=tutu.dta [COUL=black] [EPAIS=1.5]
  ! On la casse en ses différents mots.
  !-------------------------------------------------
  !
  call casc(cgfdc(jfdc),1,clmot,ilmot)
  !
  !-------------------------------------------------
  ! Saisie des paramètres prescrits par l'utilisateur.
  !-------------------------------------------------
  !
  do jmot=1,ilmot
    clc=clmot(jmot)
    if(clc(1:index(clc,'=')-1) == 'FIC') then
      !
      !-------------------------------------------------
      ! Fichier de données.
      !-------------------------------------------------
      !
      clfic=clc(index(clc,'=')+1:)
      !
      ! On permet à l'utilisateur d'écrire "$HOME" dans une spécification "#FDC FIC=".
      ! En ce cas on interprète $HOME avec la valeur UNIX.
      !
      if(clfic(1:5) == '$HOME') clfic=trim(clhome)//clfic(6:len_trim(clfic))
    elseif(clc(1:index(clc,'=')-1) == 'COUL') then
      !
      !-------------------------------------------------
      ! Choix de la couleur.
      !-------------------------------------------------
      !
      clcoul=clc(index(clc,'=')+1:)
    elseif(clc(1:index(clc,'=')-1) == 'PLUS') then
      llplusse=.true.
      !
      !-------------------------------------------------
      ! Choix de la taille du +.
      !-------------------------------------------------
      !
      clplusse=clc(index(clc,'=')+1:)
      read(clplusse,fmt=*) rgplusse
    elseif(clc(1:index(clc,'=')-1) == 'EPAIS') then
      !
      !-------------------------------------------------
      ! Choix de l'épaisseur de trait.
      !-------------------------------------------------
      !
      clepais=clc(index(clc,'=')+1:)
      read(clepais,fmt=*) zmult
      zfdc_epais=zfdc_epais*zmult
    elseif(clc(1:4) == '#FDC') then
    else
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/svg_fdc/ERREUR: directive de fond carte non attendue !...'
      write(*,fmt=*) trim(clc)
      call exit(1)
    endif
  enddo
  !
  !-------------------------------------------------
  ! Lecture du fichier-fond de carte.
  !-------------------------------------------------
  !
  inquire(file=clfic,exist=llexist)
  if(llexist) then
    write(nulsvg,fmt='(9a)') ' '
    write(nulsvg,fmt='(9a)') '<!-- Tracé du fond de carte',trim(clfic),'. -->'
    !
    !-------------------------------------------------
    ! Ouverture du fichier d'entrée.
    !-------------------------------------------------
    !
    iule=22 ; open(iule,file=clfic,form='formatted')
    if(lgdebu) write(*,fmt=*) 'svg_fdc: lecture du fichier FDC ',trim(clfic)
    !
    !-------------------------------------------------
    ! Lecture séquentielle.
    !-------------------------------------------------
    !
    inomal=0
    do
      read(iule,fmt=*,iostat=ios) clc
      if(ios == -1) then
        !
        !-------------------------------------------------
        ! Fin de fichier.
        !-------------------------------------------------
        !
        exit
      elseif(ios == 0) then
        !
        !-------------------------------------------------
        ! Cas général.
        !-------------------------------------------------
        !
        inomal=inomal+1
      else
        !
        !-------------------------------------------------
        ! Cas non prévu.
        !-------------------------------------------------
        !
        write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
        call exit(1)
      endif
      !
      !-------------------------------------------------
      ! Traitement de la ligne courante.
      !-------------------------------------------------
      !
    enddo
    !
    !-------------------------------------------------
    ! A ce stade inomal contient le nombre de lignes du fichier de fond de carte.
    !-------------------------------------------------
    !
    allocate(zfracx(inomal))
    allocate(zfracy(inomal))
    !
    !-------------------------------------------------
    ! Lecture séquentielle.
    !-------------------------------------------------
    !
    rewind(iule)
    zsaut=999.999
    inew=0
    do jnomal=1,inomal
      read(iule,fmt=*) zfx,zfy
      if(abs(zfx-zsaut) > 1.e-4) then
        zfx= (zfx-rxmin)/(rxmax-rxmin)
        zfy= (zfy-rymin)/(rymax-rymin)
        if(zfx > -0.2 .and. zfx < 1.2 .and. zfy > -0.2 .and. zfy < 1.2) then
          inew=inew+1
          zfracx(inew)=zfx
          zfracy(inew)=zfy
        endif
      else
        inew=inew+1
        zfracx(inew)=zfx
        zfracy(inew)=zfy
      endif
    enddo
    !
    !-------------------------------------------------
    ! Fermeture du fichier d'entrée.
    !-------------------------------------------------
    !
    close(iule)
    !
    !-------------------------------------------------
    ! Tracé de la ligne polygonale.
    !-------------------------------------------------
    !
    clpoin=' '
    !
    !-------------------------------------------------
    ! Si le fond de carte comporte beaucoup de points dans la zone de tracé on
    ! échantille ces points, en ne traçant que les points distants d'au moins
    ! zseuil pixels.
    !-------------------------------------------------
    !
    zseuil=min(2.,real(int(1.82e-4*real(inew))))
    if(nint(zseuil) /= 0) then
      write(*,fmt=*) '  On trace deux points de fond de carte si distants d''au moins ',nint(zseuil),' pixels.'
    endif
    write(*,fmt='(a,i2,a,i2)') '   Tracé de fond de carte ',jfdc,' / ',nfdc
    write(*,fmt='(9a)') '     fichier   : ',trim(clfic)
    write(*,fmt='(9a)') '     couleur   : ',trim(clcoul)
    write(*,fmt='(a,f4.2)') '     épaisseur  en pixels : ',zfdc_epais
    call svg_ligne_polygonale(zfracx,zfracy,inew,zsaut,clcoul,llplusse,clpoin,zfdc_epais,zseuil)
  else ! llexist
    write(*,fmt=*) '  fichier fond de carte absent: ',trim(clfic)
  endif ! llexist
  if(allocated(zfracx)) deallocate(zfracx)
  if(allocated(zfracy)) deallocate(zfracy)
enddo
end
