subroutine img_legende_alphanumerique(kx,ky &
&,krvb_fond,krvb_pp,kn,krvb_palette,cdtexte,krvb_sor)
! --------------------------------------------------------------
! **** *img_legende_alphanumerique*
! --------------------------------------------------------------
! Sujet:
!	Création d'une image contenant 
!	une barre verticale comportant n rectangles colorés définis par leur (R,V,B), 
!	chacun associé à un texte alphanumérique.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2005-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!    kx: taille X de l'image de sortie en pixels.
!    ky: taille Y de l'image de sortie en pixels.
!    krvb_fond(3): (R,V,B) du fond de la légende.
!    krvb_pp(3): (R,V,B) du premier plan des textes à écrire.
!    kn: nombre de textes à écrire.
!    krvb_palette(kn,3): (R,V,B) de chaque palette colorée associée au texte.
!    cdtexte(kn): texte à associer à chaque couleur de palette.
! En sortie:
!    krvb_sor(kx,ky,3): image-légende créée.
! --------------------------------------------------------------
! Exemple: 
! --------------------------------------------------------------
!
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
integer(kind=4), intent(in) :: krvb_fond(3),krvb_pp(3)
integer(kind=4), intent(in) :: krvb_palette(3,kn)
character*(*), intent(in) :: cdtexte(kn)
integer(kind=4), intent(out) :: krvb_sor(3,kx,ky)
integer(kind=4) :: iloc(3)
!
!-------------------------------------------------
! Initialisation du fond.
!-------------------------------------------------
!
do jy=1,ky
  do jx=1,kx
    do jc=1,3
      krvb_sor(jc,jx,jy)=krvb_fond(jc)
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Taille X et Y de chaque pavé-couleur associé à chaque item de la légende.
!-------------------------------------------------
!
ipave_x=max(30,kx/6)
ipave_y=max(15,nint(real(ky)*0.8))/kn
!
!-------------------------------------------------
! Coordonnées haut-gauche de la légende dans l'image de sortie.
!-------------------------------------------------
!
icoin_x=ipave_x/2
icoin_y=(ky-kn*ipave_y)/2
!
!-------------------------------------------------
! Ecriture de la légende et couleur de chaque item.
!-------------------------------------------------
!
do jn=1,kn
  !
  !-------------------------------------------------
  ! Pavé coloré.
  !-------------------------------------------------
  !
  do jy=1,ipave_y
    do jx=1,ipave_x
      isor_x=icoin_x+jx-1
      isor_y=icoin_y+(jn-1)*ipave_y+jy-1
      if(isor_x <= kx .and. isor_y <= ky) then
        do jc=1,3
          	krvb_sor(jc,isor_x,isor_y)=krvb_palette(jc,jn)
        enddo
      else
        write(*,fmt=*) 'img_legende_alphanumerique/ATTENTION: '
        write(*,fmt=*) isor_x,kx,isor_y,ky
      endif
    enddo
  enddo
  !
  !-------------------------------------------------
  ! Texte associé.
  !-------------------------------------------------
  !
  zopac_fond=1.
  zopac_pp=1.
  ifonte=1
  !
  !-------------------------------------------------
  ! Détermination de la taille de l'image-texte.
  ! Elle est fonction du nombre de caractères
  ! et de la fonte utilisée.
  !-------------------------------------------------
  !
  if(ifonte == 1) then
    call img_fonte_9x15bold_taille(cdtexte(jn),ix_texte,iy_texte)
  else
    print*,'img_legende_alphanumerique/ERREUR: numéro de fonte inconnu: ',ifonte
    call exit(1)
  endif
  iloc(1)=0
  iloc(2)=icoin_x+ipave_x+8
  iloc(3)=icoin_y+(jn-1)*ipave_y+(ipave_y-iy_texte)/2
  call img_texte(cdtexte(jn),krvb_fond,zopac_fond &
  &,krvb_pp,zopac_pp,ifonte,iloc,kx,ky,krvb_sor)
enddo
end
