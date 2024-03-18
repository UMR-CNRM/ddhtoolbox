program dd2gr
! --------------------------------------------------------------
! **** ** Tracé de champs (x, y, valeur) donnés sur une grille (x, y) régulière ou irrégulière.
! Methode:
! Externes:
! Auteur:   2010-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entrée: 
!  - fichier ASCII de données X Y V, un triplet sur chaque ligne,
!  - fichier de directives aux tracé (fichier ".doc") indiquant le nom du fichier ASCII de données,
!  la taille de l'image de sortie désirée, les modes d'interpolation et extrapolation, etc.
!  Le nom de ce fichier de directives est donné sur la ligne de commande à dd2gr.
! En sortie:
!  - un fichier image.
!  Le nom de ce fichier image est donné sur la ligne de commande à dd2gr.
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
character(len=400) :: clmot(40)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
lgc2d=.true.
lgdebu=.false.
lgxy_isotropes=.false.
cg_sens_palette='direct'
nfic_isol=0
lgpubli=.false.
lgleg=.true.
rgpointage=9999.
rgfacp=5.
lgpoil=.false.
lgliss=.false.
rgliss=0.
lgreticule=.true.
nrvb_negl(:)=255
lginversex=.false.
lginversey=.false.
cgtyppal='LIN'
ntexl=0
cgimpf=' '
rgplusse=1.
ncoulper=0
ntraiper=0
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargc() ! nombre d'arguments.
if(iarg /= 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'OBJET: tracé au format vectoriel SVG de données réelles.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'UTILISATION: dd2gr FDOC FSVG'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'EXEMPLE: dd2gr VNT1.doc trac.svg'
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
call getarg(1,cgfdoc)
call getarg(2,cgfpix)
llsvg=cgfpix(len_trim(cgfpix)-2:len_trim(cgfpix)) == 'svg'
write(*,fmt=*) '-------------------------------------------------------------'
write(*,fmt=*) 'dd2gr:'
!
!-------------------------------------------------
! Indefs.
!-------------------------------------------------
!
nrvb_indef(1)=220 ; nrvb_indef(2)=191 ; nrvb_indef(3)=191 ! couleur de valeur manquante.
rindef=999.999
nindef=-999
cgindef=' '
!
cgformat=cgindef
cgdate=cgindef
cginterpole=cgindef
cgtitre=cgindef
cgcoult='black'
cglang='FRA'
allocate (cgfdta(jpcourbes)) ; cgfdta(:)=cgindef
allocate (cgchamp(jpcourbes)) ; cgchamp(:)=cgindef
allocate (cgcoul(0:jpcourbes)) ; cgcoul(:)=cgindef
allocate (lgplusse(0:jpcourbes)) ; lgplusse(:)=.false.
allocate (cgpoin(0:jpcourbes)) ; cgpoin(:)=cgindef
allocate (cglarg(jpcourbes)) ; cglarg(:)=cgindef
ncourbes=0
nchamp=0
cgunite=cgindef
cgorigine=cgindef
cglegx=cgindef
cgextdyn=cgindef
cglegy=cgindef
cgregx=cgindef
cgcoul_vec=cgindef
cgtypax='LIN'
cgtypay='LIN'
cgregy=cgindef
cgfdc(:)=cgindef
nfdc=0
lgfdc=.false.
cgpal=cgindef
cglabf=cgindef
cglegf=cgindef
cglabt=cgindef
cglaba='0.'
cgvmin=cgindef
cgvmax=cgindef
cgimage=cgindef
lgimaf=.false.
cglegende=cgindef
nximage=nindef
nyimage=nindef
cgextrapolation=cgindef
cglegendexy=cgindef
cgextrmeth='CONCENTRIQUE'
rgxminl=rindef
rgxmaxl=rindef
rgyminl=rindef
rgymaxl=rindef
rneglig=rindef
npalspec=0
cgtrace_axes='oui'
lgminmax=.true.
rech_vec=rindef
nsaut_vec=1
lgpointage=.false.
rfont=1.
rfont_axes=1.
rfont_leg=1.
rfont_unite=1.
rfont_titre=1.
rgxret=rindef
rgyret=rindef
!
! A la UNIRAS (plus de contraste, couleurs saturées).
cgpaldef_neg='AQUABLUE' ; cgpaldef_pos='BLANC-JAUNE-ROUGE-ROSE' ; cgpaldef_cst='BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE'
!
! A la Keraunos (couleurs moins saturées, moins de contraste).
cgpaldef_neg='AQUABLUE' ; cgpaldef_pos='BLANC-JAUNE-ROUGE-ROSE' ; cgpaldef_cst='BLANC-BLEU-VERT-ROUGE'
!
!-------------------------------------------------
! S'il existe, on lit $HOME/.dd2gr.doc.
!-------------------------------------------------
!
call getenv('HOME',clhome)
clfdoc=trim(clhome)//'/.dd2gr.doc'
inquire(file=clfdoc,exist=llex)
if(llex) call lit_directives(clfdoc)
!
!-------------------------------------------------
! Ouverture du fichier de directives.
!-------------------------------------------------
!
inquire(file=cgfdoc,exist=llex)
if(llex) then
  call lit_directives(cgfdoc)
else
  write(*,fmt=*) 
  write(*,fmt=*) 'dd2gr/ERREUR: fichier DOC inexistant!...'
  write(*,fmt=*) trim(cgfdoc)
  call exit(1)
endif
!
!-------------------------------------------------
! S'il existe, on lit c.doc.
!-------------------------------------------------
!
clfdoc='c.doc'
inquire(file=clfdoc,exist=llex)
if(llex) call lit_directives(clfdoc)
!
!-------------------------------------------------
! Si l'utilisateur demande des échelles log, on ajoute cette info dans la
! légende des axes.
!-------------------------------------------------
!
if(.not.llsvg) then
  if(trim(cgtypax) == 'LOG') cglegx=trim(cglegx)//' (log10)'
  if(trim(cgtypay) == 'LOG') cglegy=trim(cglegy)//' (log10)'
  if(trim(cgtypax) == '-LOG') cglegx=trim(cglegx)//' (-log10)'
  if(trim(cgtypay) == '-LOG') cglegy=trim(cglegy)//' (-log10)'
endif
!
!-------------------------------------------------
! Taille de l'image de sortie.
!-------------------------------------------------
!
if(cgimage == cgindef .and. cgformat(1:3) == 'LLV') then
  !
  ! L'utilisateur n'a pas spécifié de taille en pixels pour l'image de sortie.
  ! Cas de tracé de champs géographiques Lon-Lat-Valeur.
  !
  nximage=1500 ; nyimage=850
elseif(cgimage == cgindef) then
  !
  ! L'utilisateur n'a pas spécifié de taille en pixels pour l'image de sortie.
  !
  !nximage=936 ; nyimage=594
  nximage=1400 ; nyimage=890
else
  !
  ! L'utilisateur a  spécifié une taille en pixels pour l'image de sortie..
  !
  read(cgimage,fmt=*) nximage,nyimage
endif
write(*,fmt='(100a)') ' '
write(*,fmt=*) '  Taille de l''image de sortie en pixels : ',nximage,' x ',nyimage
!
!-------------------------------------------------
! Taille de la légende sur l'image de sortie, dans le cas des tracés de courbes.
!-------------------------------------------------
!
if(cglegende == cgindef) then
  nxlegende=nximage/4
  nylegende=max(60,nyimage-45)
else
  read(cglegende,fmt=*) nxlegende,nylegende
endif
if(lgdebu) write(*,fmt='(2(a,i5))') '  nxlegende=',nxlegende,' nylegende=',nylegende
if(trim(cgextrapolation) == 'non') then
  !
  !-------------------------------------------------
  ! Si l'utilisateur a porté "#EXTRAPOLE=non" dans le fichier DOC
  ! c'est qu'il veut un simple pointage des valeurs irrégulières d'entrée
  ! sur la grille régulière de sortie.
  ! On affichera avec une couleur "valeur manquante" les points
  ! de la grille de sortie qui n'ont pas été affectés
  ! par une valeur irrégulière d'entrée.
  !-------------------------------------------------
  !
  lgextrapolation=.false.
else
  !
  !-------------------------------------------------
  ! Si l'utilisateur n'a pas porté "#EXTRAPOLE=non" dans le fichier DOC
  ! c'est qu'il veut en sortie une grille régulière entièrement
  ! remplie par des valeurs, quitte à rechercher la valeur
  ! du point de grille le plus proche.
  !-------------------------------------------------
  !
  lgextrapolation=.true.
endif
!
!-------------------------------------------------
! Test de cohérence.
!-------------------------------------------------
!
if(lgliss .and. .not.lgextrapolation) then
  write(*,fmt=*)
  write(*,fmt=*) 'dd2gr/ERREUR: l''option lissage (#LISSAGE) et l''option "sans extrapolation (#EXTRAPOLE=non) sont incompatibles". Supprimer l''une des deux de votre fichier ".doc".'
  write(*,fmt=*)
  call exit(1)
endif
!
!-------------------------------------------------
! Palette par défaut.
!-------------------------------------------------
!
if(cgpal == cgindef) then
  !cgpal='ARC-EN-CIEL'
  !cgpal='BLANC-BLEU-VERT-JAUNE-ROUGE-ROSE'
  cgpal='AUTO'
endif
!
!-------------------------------------------------
! On combine plusieurs infos dans le titre.
!-------------------------------------------------
!
if(.not.llsvg) then
  if(cgtitre /= cgindef) then
    if(cgunite /= cgindef) cgtitre=trim(cgtitre)//' ('//trim(cgunite)//')'
    if(cgorigine /= cgindef) cgtitre=trim(cgtitre)//' , '//trim(cgorigine)
    if(cgdate /= cgindef) cgtitre=trim(cgtitre)//' , '//trim(cgdate)
  endif
endif
!
!-------------------------------------------------
! Gestion des défauts afférents au fond de carte.
!-------------------------------------------------
!
if(cgformat(1:2) == 'LL') then
  !
  !-------------------------------------------------
  ! Dans le format LLV ou LLUVC on lit par défaut un fond de carte.
  !-------------------------------------------------
  !
  lgfdc=.true.
endif
if(cgfdta(1) == cgindef) then
  !
  !-------------------------------------------------
  ! Le nom du fichier n'a pas été fourni dans le DOC.
  ! On l'initialise à partir du nom du fichier DOC.
  !-------------------------------------------------
  !
  if(len_trim(cgfdoc) >=5) then
    if(cgfdoc(len_trim(cgfdoc)-3:len_trim(cgfdoc)) == '.doc') then
      !
      !-------------------------------------------------
      ! Le fichier DOC a pour suffixe ".doc".
      !-------------------------------------------------
      !
      cgfdta(1)=cgfdoc(1:len_trim(cgfdoc)-4)//'.dta'
    else
      !
      !-------------------------------------------------
      ! Le fichier DOC n'a pas de suffixe reconnu.
      !-------------------------------------------------
      !
      cgfdta(1)=trim(cgfdoc)//'.dta'
    endif
  endif
endif
!
!-------------------------------------------------
! Appel des tracés par type de donnée.
!-------------------------------------------------
!
if(trim(cgformat) == 'XYV' .or. trim(cgformat) == 'HAS' .or. trim(cgformat) == 'HAA' .or. trim(cgformat) == 'LLV') then
  if(cgregx == cgindef) then
    !
    !-------------------------------------------------
    ! La directive REGX et REGY n'a pas été portée dans le fichier DOC.
    ! C'est que les données sont irrégulières.
    !-------------------------------------------------
    !
    call xyv_irreg
  else
    !
    !-------------------------------------------------
    ! La directive REGX et REGY été portée dans le fichier DOC.
    ! C'est que les données régulières, et ne comportent qu'une colonne.
    !-------------------------------------------------
    !
    call xyv_reg
  endif
elseif(trim(cgformat) == 'XYCT') then
    !
    !-------------------------------------------------
    ! Format XYCT: XY Couleur Texte.
    ! En entrée on a un fichier ASCII dont chaque ligne est du type
    ! 5.245 -7.52315 255,0,0 Maison
    ! ce qui écrira la chaîne "Maison" en rouge au lieu de coordonnées (5.245 , -7.52315).
    !-------------------------------------------------
    !
    call xyct
elseif(trim(cgformat) == 'X' .or. trim(cgformat) == 'Y' .or. trim(cgformat) == 'XV' .or. trim(cgformat) == 'YV' .or. trim(cgformat) == 'XVI' .or. trim(cgformat) == 'YVI') then
  call courbes
elseif(trim(cgformat) == 'XYUVC' .or. trim(cgformat) == 'LLUVC') then
  !
  !-------------------------------------------------
  ! Tracé de vecteurs, et d'un champ coloré.
  !-------------------------------------------------
  !
  call xyuvc_irreg(nximage,nyimage)
else
  write(*,fmt=*) 
  write(*,fmt=*) 'dd2gr/ERREUR: format non reconnu!...'
  write(*,fmt=*) trim(cgformat)
  call exit(1)
endif
end
