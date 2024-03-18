subroutine img_fdc(cdfdc,cdcoord,kximage,kyimage,kxdecal,kydecal,kxpix,kypix,krvb)
! --------------------------------------------------------------
! **** *img_fdc* Superposition d'un fond de carte (exemple: contour des continents) sur une image.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2010-10, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdfdc: fond de carte à superposer à l'image:
!		cdfdc='non' si aucun fond de carte désiré,
!		sinon cdfdc='/home/piriou/ch/fdc/ref  255 0 0'
!		si on veut par exemple lire le fond de carte sur le fichier /home/piriou/ch/fdc/ref
!		et le tracer en rouge (triplet RVB 255 0 0).
!		Dans le fichier fond de carte le saut de plume est supposé donné
!		par une ligne sur laquelle sont écrits les deux réels 999.999 999.999.
!	cdcoord: coordonnées des axes X et Y:
!		cdcoord='non' si les coordonnées ne sont pas fournies,
!		cdcoord='1.5  28.  -78.   45.' si les coordonnées réelles
!			* du bas gauche du carré en bas à gauche sont X=1.5 Y=-78.
!			* du haut droite du carré en haut à droite sont X=28. Y=45.
!		en l'état actuel du logiciel ces coordonnées servent si l'on superpose
!		un fond de carte (variable cdfdc), ou que l'on demande le légendage des axes X et Y.
!	kximage,kyimage: tailles X,Y de l'image de sortie.
!	kxdecal,kydecal: nombre de pixels en X et Y dont la zone de tracé est décalée au sein de l'image totale (i.e. celle avec ses titres et légendes).
!	kx,ky: dimension des données à tracer.
! En entrée/sortie:
!  krvb: tableau 2D des valeurs RVB de l'image sur laquelle superposer le fond de carte.
! --------------------------------------------------------------
!
!
#include"implicit_r8i4.h"
!
character*(*) cdfdc,cdcoord
integer(kind=4) :: krvb(3,kximage,kyimage)
integer(kind=4) :: irvb_trait(3)
!
!-------------------------------------------------
! Lecture des coordonnées.
!-------------------------------------------------
!
if(trim(cdcoord) /= 'non') then
  read(cdcoord,fmt=*) zxmin,zxmax,zymin,zymax
else
  write(*,fmt=*) 
  write(*,fmt=*) 'img/ERREUR: tracé de fond de carte demandé, or les coordonnées en X et Y n''ont pas été fournies!...'
  write(*,fmt=*) 
  call exit(1)
endif
!
!-------------------------------------------------
! Lecture du nom du fichier-fond de carte.
!-------------------------------------------------
!
ipos=index(cdfdc,' ')
clfic=cdfdc(1:ipos-1)
!
!-------------------------------------------------
! Lecture des valeurs RVB de la couleur désirée du fond de carte sur l'image finale.
!-------------------------------------------------
!
read(cdfdc(ipos+1:),fmt=*) ir,iv,ib
!
!-------------------------------------------------
! Lecture du fichier-fond de carte.
!-------------------------------------------------
!
inquire(file=clfic,exist=llexist)
if(llexist) then
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  iule=22 ; open(iule,file=clfic,form='formatted')
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  zsaut=999.999
  zindef=2.0124547e12
  zfxprec=zindef
  zfyprec=zindef
  inomal=0
  llsaut=.true.
  do
    read(iule,fmt=*,iostat=ios) zx,zy
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
    if(abs((zx-zsaut)/zsaut) < 1.E-7) then
      !
      !-------------------------------------------------
      ! Saut de plume.
      !-------------------------------------------------
      !
      llsaut=.true.
    else
      !
      !-------------------------------------------------
      ! Pas de saut de plume: une donnée de (x,y) est fournie dans le fichier fond de carte.
      !-------------------------------------------------
      !
      if(llsaut) then
        !
        !-------------------------------------------------
        ! La donnée précédente était un saut de plume.
        ! Vérifions que le couple (x,y) est bien à l'intérieur du domaine
        ! de tracé.
        !-------------------------------------------------
        !
        zfx=(zx-zxmin)/(zxmax-zxmin)
        zfy=(zy-zymin)/(zymax-zymin)
        if(zfx < 0. .or. zfx > 1. .or. zfy < 0. .or. zfy > 1.) then
          !
          !-------------------------------------------------
          ! Le point est hors de la zone à tracer.
          ! Il est équivalent à un saut de plume.
          !-------------------------------------------------
          !
          llsaut=.true.
        else
          !
          !-------------------------------------------------
          ! Le point est dans la zone à tracer.
          ! On le sauvegarde pour le coup d'après.
          !-------------------------------------------------
          !
          llsaut=.false.
          zfxprec=zfx
          zfyprec=zfy
        endif
      else
        !
        !-------------------------------------------------
        ! La donnée précédente n'était pas un saut de plume.
        !-------------------------------------------------
        !
        zfx=(zx-zxmin)/(zxmax-zxmin)
        zfy=(zy-zymin)/(zymax-zymin)
        if(zfx < 0. .or. zfx > 1. .or. zfy < 0. .or. zfy > 1.) then
          !
          !-------------------------------------------------
          ! Le point est hors de la zone à tracer.
          ! Il est équivalent à un saut de plume.
          !-------------------------------------------------
          !
          llsaut=.true.
        else
          !
          !-------------------------------------------------
          ! Le point est dans la zone à tracer.
          ! Tracé du segment de droite allant de (zfxprec,zfyprec) à (zfx,zfy).
          !-------------------------------------------------
          !
          ixprec=max(1,min(kxpix,int(zfxprec*real(kxpix))+1))+kxdecal
          iyprec=max(1,min(kypix,kypix-int(zfyprec*real(kypix))+1))+kydecal
          ix=max(1,min(kxpix,int(zfx*real(kxpix))+1))+kxdecal
          iy=max(1,min(kypix,kypix-int(zfy*real(kypix))+1))+kydecal
          ipix_largeur=1 ! largeur du trait en pixels.
          irvb_trait(1)=ir
          irvb_trait(2)=iv
          irvb_trait(3)=ib
          call img_trait(ixprec,iyprec,ix,iy,ipix_largeur,irvb_trait,krvb,kximage,kyimage)
          llsaut=.false.
          zfxprec=zfx
          zfyprec=zfy
        endif
      endif
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(iule)
endif
end
