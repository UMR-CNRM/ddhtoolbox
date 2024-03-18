subroutine svg_legende_discrete(kn,krvb_palette,pseuil,cdunite)
! --------------------------------------------------------------
! **** *svg_legende_discrete*
! --------------------------------------------------------------
! Sujet:
!  Création d'une image contenant 
!  une barre verticale comportant n rectangles colorés définis par leur (R,V,B), 
!  chacun associé à un seuil donné par un nombre réel.
!
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2014-10, J.M. Piriou.
! Modifications:
!           2017-03, J.M. Piriou: débogage: zxrect (faute de frappe) devient zx_rect.
! --------------------------------------------------------------
! En entree:
!    kn: nombre de seuils fournis; c'est aussi le nombre de palettes colorées - 1.
!    krvb_palette(kn+1,3): (R,V,B) de chaque palette colorée associée au texte.
!    pseuil(kn): seuils déterminant la frontière entre chaque classe.
!    cdunite: unité du champ dont on trace la légende (i.e. unité physique des seuils).
! En sortie:
!    écriture d'un fichier SVG, d'unité logique nulsvg.
! --------------------------------------------------------------
! Exemple: 
! --------------------------------------------------------------
!
use parametres
#include"implicit_r8i4.h"
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
integer(kind=4), intent(in) :: krvb_palette(3,kn+1)
real(kind=8), intent(in) :: pseuil(kn)
character*(*), intent(in) :: cdunite
integer(kind=4) :: iloc(3)
if(lgpubli) then
  !
  !-------------------------------------------------
  ! On augmente la taille des caractères
  ! pour publication en A4 bi-colonne.
  !-------------------------------------------------
  !
  zpub=1.6
else
  zpub=1.
endif
!
!-------------------------------------------------
! Initialisation du fond.
!-------------------------------------------------
!
ipaves=kn+1
!
!-------------------------------------------------
! Taille X et Y de chaque pavé-couleur associé à chaque item de la légende.
!-------------------------------------------------
!
zx_rect=0.12*rx_legcour
zy_rect=(rlysvg-ryt-ry_legx)/real(ipaves+2)
!
!-------------------------------------------------
! Coordonnées bas-gauche de la légende dans l'image de sortie.
!-------------------------------------------------
!
zxcoin=rxt+rlxt+0.20*rx_legcour
!zycoin=ryt+0.95*rlyt
zycoin=ryt+rlyt-0.5*(rlyt-real(ipaves)*zy_rect)
!
!-------------------------------------------------
! Abscisse de l'unité (ex: K ou m/s, etc).
!-------------------------------------------------
!
!ixtxt_unite=zxcoin+0.5*zx_rect
!
!-------------------------------------------------
! Ecriture de la légende et couleur de chaque item.
!-------------------------------------------------
!
write(nulsvg,fmt='(9a)') '<!-- Légende de la palette colorée: pavés colorés. -->'
do jn=1,ipaves
  !
  !-------------------------------------------------
  ! Définition du pavé coloré en SVG.
  !-------------------------------------------------
  !
  zycoin_loc=zycoin-real(jn)*zy_rect
  if(lgdebu) write(*,fmt=*) 'svg_legende_discrete: rectangle n° ',jn,' en ' &
  & ,zxcoin,zycoin_loc,' couleur ',krvb_palette(1,jn),krvb_palette(2,jn),krvb_palette(3,jn)

  zwidth_bord_pave=3.00e-4*rlxsvg
  write(clrect,fmt='(4(a,f8.2),3(a,i3.3),a,f8.2,100a)') '<rect x="',zxcoin,'" y="',zycoin_loc,'" width="',zx_rect &
  &,'" height="',zy_rect,'" style="fill:rgb(',krvb_palette(1,jn),',',krvb_palette(2,jn),',',krvb_palette(3,jn),'); stroke: #000000; stroke-width: ',zwidth_bord_pave,';" />'
  clrect=cl_nettoie_blancs(clrect)
  write(nulsvg,fmt='(a)') trim(clrect)
enddo
do jn=1,kn
  !
  !-------------------------------------------------
  ! Texte contenant la valeur réelle, frontière entre 2 classes.
  !-------------------------------------------------
  !
  if(trim(cglegf) == trim(cgindef)) then
    !
    !-------------------------------------------------
    ! L'utilisateur n'a pas spécifié le format FORTRAN dans lequel il veut que
    ! les réels de la légende soient écrits. On fait cela de façon dynamique.
    !-------------------------------------------------
    !
    call reecar(pseuil(jn),-1,3,cltxt,iltexte)
  else
    !
    !-------------------------------------------------
    ! L'utilisateur a spécifié le format FORTRAN dans lequel il veut que
    ! les réels de la légende soient écrits. 
    !-------------------------------------------------
    !
    clform='('//trim(cglegf)//')'
    write(cltxt,fmt=clform) pseuil(jn)
  endif
  !
  !-------------------------------------------------
  ! On élimine le "." si c'est le dernier caractère de la chaîne: le réel est un entier !
  !-------------------------------------------------
  !
  if(len_trim(cltxt) >= 3) then
    if(cltxt(len_trim(cltxt)-2:len_trim(cltxt)) == '.00') cltxt(len_trim(cltxt)-2:len_trim(cltxt))='   '
  endif
  if(len_trim(cltxt) >= 2) then
    if(cltxt(len_trim(cltxt)-1:len_trim(cltxt)) == '.0') cltxt(len_trim(cltxt)-1:len_trim(cltxt))='  '
  endif
  if(cltxt(len_trim(cltxt):len_trim(cltxt)) == '.') cltxt(len_trim(cltxt):len_trim(cltxt))=' '
  !
  !-------------------------------------------------
  ! On élimine les "0" non significatifs à droite de la virgule.
  ! Exemple: "0.500" devient "0.5".
  !-------------------------------------------------
  !
  call ote_zeros_non_sign(cltxt,cltxt2)
  cltxt=cltxt2
  !
  !-------------------------------------------------
  ! Calage à gauche.
  !-------------------------------------------------
  !
  cltxt=adjustl(cltxt)
  !
  !-------------------------------------------------
  ! Position du texte sur l'image SVG.
  !-------------------------------------------------
  !
  !ixtxt=nint(zxcoin+zx_rect+0.25*(rx_legcour-zx_rect-(zxcoin-rxt-rlxt)))
  ixtxt=nint(zxcoin+zx_rect+45.)
  ixtxt_unite=ixtxt
  iytxt=nint(zycoin-real(jn)*zy_rect+0.5*rgtaille_fonte*rfont_leg)
  ztaille=rgtaille_fonte*rfont_leg*zpub
  !
  ! Diminution de taille des caractères lorsqu'il y a beaucoup de pavés.
  !
  ztaille=ztaille/real(ipaves)*real(min(40,ipaves))
  
  write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
  & ,ixtxt,'" y="' &
  & ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaille &
  & ,'" text-anchor="middle" fill="black" >' &
  & ,trim(cltxt),'</text>'
  
  clsvg_texte=cl_nettoie_blancs(clsvg_txt)
  !
  !-------------------------------------------------
  ! On écrit le nombre réel correspondant à la classe, pour une classe sur
  ! imodulo.
  !-------------------------------------------------
  !
  !imodulo=2
  imodulo=1
  if(modulo(jn+1,imodulo) == 0) then
    write(nulsvg,fmt='(9a)') '<!-- Ecriture de la valeur réelle associée à la palette colorée. -->'
    write(nulsvg,fmt='(a)') trim(clsvg_texte)
  endif
enddo
!
!-------------------------------------------------
! Ecriture de l'unité.
!-------------------------------------------------
!
!ixtxt=rxt+rlxt+0.5*rx_legcour
ixtxt=ixtxt_unite
!iytxt=ryt+rlyt
zfrac=0.7
iytxt=zfrac*(ryt+rlyt)+(1.-zfrac)*(ryt+rlyt-zy_rect)
clleg=cgunite
if(trim(clleg) == 'none') clleg=' '


ztaif=rgtaille_fonte*rfont_unite*zpub
write(clsvg_txt,fmt='(a,i5,a,i5,3a,g16.7,3a)') '<text x="' &
& ,ixtxt,'" y="' &
& ,iytxt,'" ',trim(cgfonte_nombres),' font-size="',ztaif &
& ,'" text-anchor="middle" fill="black" >' &
& ,trim(clleg),'</text>'

clsvg_texte=cl_nettoie_blancs(clsvg_txt)
write(nulsvg,fmt='(9a)') '<!-- Ecriture de l''unité. -->'
write(nulsvg,fmt='(a)') trim(clsvg_texte)
end
