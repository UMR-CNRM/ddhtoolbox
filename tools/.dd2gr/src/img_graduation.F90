subroutine img_graduation(pval,kval,px,py,krvb_fond_texte,popac_fond_texte &
  &,krvb_pp_texte,popac_pp_texte,kfonte,kx,ky,krvb)
! --------------------------------------------------------------
! **** *img_graduation* Pointage des valeurs réelles d'une graduation.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-04, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!
! La graduation est donnée comme une ligne brisée de kval points.
! Chaque point est donné par 
! - sa localisation X: px(jval), jval=1,kval.
! - sa localisation Y: py(jval), jval=1,kval.
! - la valeur de la graduation en ce point: pval(jval), jval=1,kval.
! Donc si on veut définir une graduation régulière: kval=2.
! Pour une graduation irrégulière: kval > 2.
! ATTENTION: les valeurs, dans pval, doivent être croissantes!...
!
! krvb_fond_texte(3): couleur du fond du texte
! popac_fond_texte: opacité du fond du texte
! krvb_pp_texte(3): couleur du pp du texte
! popac_pp_texte: opacité du pp du texte
! kfonte: fonte du texte:
!		- si kfonte=1, fonte 9x15bold
! kx, ky: taille de l'image en E/S
!
! En entrée/sortie:
!
! krvb: triplets RVB de l'image sur laquelle on surimpose le texte.
!
! --------------------------------------------------------------
!
#include"implicit_r8i4.h"
!
integer(kind=4) :: krvb_fond_texte(3),krvb_pp_texte(3),inbcar,ityps(4)
integer(kind=4) :: kfonte,kx,ky,krvb(3,kx,ky),irvb_fond_gol_texte
integer(kind=4) :: ix_texte,iy_texte,jcoul,jx,jy,iloc(3)
integer(kind=4), allocatable :: irvb_texte(:,:,:)
real(kind=8) :: popac_fond_texte,popac_pp_texte
real(kind=8) :: pval(kval),px(kval),py(kval)
integer(kind=4), parameter :: jpclas=100
real(kind=8) :: zclas(jpclas)
!
!-------------------------------------------------
! Longueur de la ligne brisée en pixels.
!-------------------------------------------------
!
zlong=0
do jval=1,kval-1
  zlong=zlong+sqrt((px(jval+1)-px(jval))**2+(py(jval+1)-py(jval))**2)
enddo
zmin=real(minval(pval))
zmax=real(maxval(pval))
!
!-------------------------------------------------
! Nombre approx. de réels cotables: proportionnel à la longueur de la ligne brisée.
!-------------------------------------------------
!
inb=nint(zlong*12./600.) ! environ 12 cotations sur 600 pixels.
!
!-------------------------------------------------
! Ecartement entre deux cotes.
!-------------------------------------------------
!
zec=(zmax-zmin)/real(inb)
!
!-------------------------------------------------
! Arrondi à un chiffre significatif.
!-------------------------------------------------
!
call arrr(zec,1,zec2)
zec=zec2
!
!-------------------------------------------------
! Si l'écart est un nombre "usuel", on l'arrondit à l'entier le plus proche.
!-------------------------------------------------
!
if(zec > 0.5 .and. zec < 100000.) then
  !
  !-------------------------------------------------
  ! On va coter des entiers.
  !-------------------------------------------------
  !
  zec2=real(nint(zec))
  zec=zec2
  llentier=.true.
else
  !
  !-------------------------------------------------
  ! On va coter des réels.
  ! On en met deux fois moins (occupent plus de caractères).
  !-------------------------------------------------
  !
  llentier=.false.
  zec=zec*2.
  call arrr(zec,1,zec2)
  zec=zec2
endif
!
!-------------------------------------------------
! On crée iclas classes à coter.
!-------------------------------------------------
!
llp0=.false.
call cree_classes(zmin,zmax,zec,llp0,zeps,jpclas,iclas,zclas)
!
!-------------------------------------------------
! Ecriture du texte de cotation sur l'image.
!-------------------------------------------------
!
do jclas=1,iclas
  if(llentier) then
    !
    !-------------------------------------------------
    ! Cotation d'entiers.
    !-------------------------------------------------
    !
    write(cltexte,fmt=*) nint(zclas(jclas))
  else
    !
    !-------------------------------------------------
    ! Cotation de réels.
    !-------------------------------------------------
    !
    write(cltexte,fmt='(f10.2)') zclas(jclas)
  endif
  cltexte=adjustl(cltexte)
  !
  !-------------------------------------------------
  ! Ecriture de texte.
  !-------------------------------------------------
  !
  ! Localisation du texte.
  !
  llok=.false.
  do jval=1,kval-1
    if(zclas(jclas) >= pval(jval)  .and. zclas(jclas) <= pval(jval+1)) then
      !
      !-------------------------------------------------
      ! La valeur courante à coter est dans le segment
      ! courant de la ligne brisée d'entrée.
      !-------------------------------------------------
      !
      zxpos=px(jval)+(px(jval+1)-px(jval))*(zclas(jclas)-pval(jval))/(pval(jval+1)-pval(jval))
      ixpos=max(1,min(kx,nint(zxpos)))
      zypos=py(jval)+(py(jval+1)-py(jval))*(zclas(jclas)-pval(jval))/(pval(jval+1)-pval(jval))
      iypos=max(1,min(ky,nint(zypos)))
      llok=.true.
    endif
  enddo
  if(llok) then
    iloc(1)=10 ! centrage.
    iloc(2)=ixpos ! X.
    iloc(3)=iypos ! Y.
    call img_texte(cltexte,krvb_fond_texte,popac_fond_texte &
    &,krvb_pp_texte,popac_pp_texte,kfonte,iloc,kx,ky,krvb)
  endif
enddo
end
