MODULE parametres
#include"implicit_r8i4.h"
SAVE
integer(kind=4) :: ncourbes,nchamp,nxlegende,nylegende
character(len=400) cgformat,cgdate,cginterpole,cgtitre,cgunite &
&,cgorigine,cglegx,cglegy,cglegende,cglang
character(len=400) :: cgcoult ! couleur du titre.
integer(kind=4), parameter :: jpcourbes=100
character(len=400) :: cgtexl(100) ! texte libre de l'utilisateur.
integer(kind=4) :: ntexl ! nb de textes écrits par l'utilisateur.
!
!-------------------------------------------------
! Définition des courbes.
!-------------------------------------------------
!
character(len=400), allocatable :: cgfdta(:) ! nom du fichier de données de cette courbe.
character(len=400), allocatable :: cgchamp(:) ! nom en clair du champ associé à cette courbe.
character(len=400), allocatable :: cgcoul(:) ! couleur de cette courbe.
character(len=400), allocatable :: cgpoin(:) ! courbe pointillée ou continue.
character(len=400), allocatable :: cglarg(:) ! largeur de trait de cette courbe.
logical, allocatable :: lgplusse(:) ! vrai si on doit "plusser" cette courbe, i.e. écrire un + à chaque début de segment.
real(kind=8) :: rgplusse ! facteur multiplicatif de la taille du "+" versus la taille standard.
integer(kind=4) :: ncoulper ! périodicité de couleur de courbe.
integer(kind=4) :: ntraiper ! périodicité de type de trait de la courbe.
!
!-------------------------------------------------
! Divers.
!-------------------------------------------------
!
character(len=400) cgfdoc,cgfpix,cgindef,cgregx,cgregy,cglegendexy
character(len=400) :: cglabf ! LABel Format: format FORTRAN d'écriture des réels dans les labels (option pointage sous XYV ou LLV).
character(len=400) :: cglabt ! LABel Taille.
character(len=400) :: cglaba ! LABel Angle.
character(len=400) :: cglegf ! LEGende Format: format FORTRAN d'écriture des réels de la légende de la palette colorée.
integer(kind=4), parameter :: jpfdc=10
character(len=4000) cgfdc(jpfdc)
logical :: lgfdc
integer(kind=4) :: nfdc
character(len=400) cgpal,cgvmin,cgvmax,cgimage,cgextrapolation,cgextrmeth,cgtypax,cgtypay
logical :: lgimaf ! vrai si le champ "#IMAGE" a été forcé par l'utilisateur.
real(kind=8) :: rindef,rneglig
real(kind=8) :: rgxret,rgyret ! écart X (resp. Y ) entre 2 tracés de réticule.
integer(kind=4) :: nximage,nyimage,nindef,nrvb_indef(3)
integer(kind=4) :: nrvb_negl(3) ! Couleur des valeurs négligeables.
logical :: lgextrapolation
logical :: lgdebu
logical :: lgc2d ! vrai si lors d'un tracé de champ 2D il faut coloriser le champ. Ce logique est faux si on se souhaite que des isolignes.

logical :: lgxy_isotropes ! vrai si on veut que le rapport d'aspect Y/X en pixels soit celui des données réelles.
integer(kind=4), parameter :: jppalspec=600
character(len=400) cgpalspec(jppalspec) ! palette spécifiée comme une suite de triplets RVB.

character(len=400) cgpaldef_neg ! palette par défaut pour les valeurs négatives.
character(len=400) cgpaldef_pos ! palette par défaut pour les valeurs positives.
character(len=400) cgpaldef_cst ! palette par défaut pour les valeurs de signe constant.
character(len=400) :: cgtyppal ! Type de palette de couleur: 'LIN' (linéaire) ou 'LOG' (logarithmique).

character(len=400) cgextdyn
integer(kind=4) :: npalspec
integer(kind=4) :: nulsvg ! unité logique du fichier SVG de sortie.
!
!-------------------------------------------------
! Mode pointage de cercles.
!-------------------------------------------------
!
logical :: lgpointage ! vrai si on pointe des cercles, au lieu de remplir de la couleur (dans le cas XYV et LLV), ou au lieu de tracer des segments de droite (dans le cas XYI).
!
!-------------------------------------------------
! Min et max en X et Y, lorsqu'ils sont imposés par l'utilisateur.
!-------------------------------------------------
!
real(kind=8) :: rgxminl,rgxmaxl,rgyminl,rgymaxl
!
!-------------------------------------------------
! Min et max en X et Y, dans l'espace réel (celui des Kelvin, m/s, etc).
!-------------------------------------------------
!
real(kind=8) :: rxmin
real(kind=8) :: rxmax
real(kind=8) :: rymin
real(kind=8) :: rymax
!
!-------------------------------------------------
! Pointage de lieux: si dans les données V de LLV (3èmùe colonne) l'utilisateur a mis la valeur rgpointage, alors on pointe
! en ce lieu un disque de couleur donnée (noire dans la version actuelle de dd2gr).
!-------------------------------------------------
!
real(kind=8) :: rgpointage ! valeur utilisateur spécifiant qu'une donnée est là simplement pour mettre un disque noir en ce lieu.
real(kind=8) :: rgfacp ! FACteur multiplicatif du rayon du point noir dans l'option rgpointage.
logical :: lgpoil ! POIntage de Lieux.
!
!-------------------------------------------------
! Min et max du champ, dans le cas de champs 2D.
!-------------------------------------------------
!
real(kind=8) :: rcmin
real(kind=8) :: rcmax
real(kind=8) :: rcmoy
real(kind=8) :: rcect
real(kind=8) :: rcrcm
real(kind=8) :: rcmin_reel ! sert simplement à l'affichage du sous-titre.
real(kind=8) :: rcmax_reel ! sert simplement à l'affichage du sous-titre.
real(kind=8) :: rech_vec ! échelle de tracé des modules de vecteurs (cas XYUVC ou LLUVC).
integer(kind=4) :: nsaut_vec ! on ne trace qu'un vecteur sur nsaut_vec.
character(len=400) :: cgcoul_vec ! couleur des tracés de vecteurs.
!
!-------------------------------------------------
! Sens de la palette: 'direct' ou 'inverse'.
!-------------------------------------------------
!
character(len=400) :: cg_sens_palette
!
!-------------------------------------------------
! Fontes.
!-------------------------------------------------
!
!character(len=400) :: cgfonte_texte='font-family="New Century Schoolbook" font-weight="bold"' ! fonte pour le texte (titres, etc).
!character(len=400) :: cgfonte_texte='font-family="URW Gothic L" font-weight="bold"' ! fonte pour le texte (titres, etc).
!character(len=400) :: cgfonte_nombres='font-family="URW Gothic L" font-weight="bold"' ! fonte pour les nombres.
character(len=400) :: cgfonte_texte='font-family="Arial" font-weight="bold"' ! fonte pour le texte (titres, etc).
character(len=400) :: cgfonte_nombres='font-family="Arial" font-weight="bold"' ! fonte pour les nombres.
real(kind=8) :: rgtaille_fonte ! taille des caractères en pixels.
real(kind=8) :: rfont ! coefficient multiplicateur appliqué à rgtaille_fonte, pour TOUTES les écritures de texte.
real(kind=8) :: rfont_axes ! coefficient multiplicateur appliqué à rgtaille_fonte, pour la taille de légendage des axes.
real(kind=8) :: rfont_leg ! coefficient multiplicateur appliqué à rgtaille_fonte, pour la taille des caractères de la légende (légende de la palette de couleurs, légende de noms de courbes).
real(kind=8) :: rfont_unite ! coefficient multiplicateur appliqué à rgtaille_fonte, pour la taille de l'unité des champs 2D (ex: K/jour).
real(kind=8) :: rfont_titre ! coefficient multiplicateur appliqué à rgtaille_fonte, pour la taille des titres et sous-titres.
character(len=400) :: cgtrace_axes ! 'oui' si tracé et cotation des axes.
logical :: lgminmax ! vrai si on affiche les min, max, moy, ect du champ.
!
!-------------------------------------------------
! Coin haut gauche de la palette de couleurs colorée, servant de légende des couleurs.
!-------------------------------------------------
!
real(kind=8) :: rxpal
real(kind=8) :: rypal
!
!-------------------------------------------------
! Etendue en X et Y de la palette de couleurs colorée, servant de légende des couleurs.
!-------------------------------------------------
!
real(kind=8) :: rlxpal
real(kind=8) :: rlypal
!
!-------------------------------------------------
! Paramètres pour les tracés de courbes 1D.
!-------------------------------------------------
!
integer(kind=4), parameter :: jplignes=200
real(kind=8) :: rlxsvg ! nombre de pixels en X de la zone de tracé complète.
real(kind=8) :: rlysvg ! nombre de pixels en Y de la zone de tracé complète.
real(kind=8) :: rxt ! Largeur de l'espace à gauche pour le légendage de l'axe Y.
real(kind=8) :: ry_legx ! Hauteur de l'espace du bas pour le légendage de l'axe X.
real(kind=8) :: rx_legcour ! Largeur de de l'espace à droite pour le légendage des couleurs des courbes.
real(kind=8) :: ryt ! Hauteur de la zone réservée au titre.
real(kind=8) :: rlxt ! Au sein de l'espace total, taille en pixels de la zone de tracé des courbes en X.
real(kind=8) :: rlyt ! Au sein de l'espace total, taille en pixels de la zone de tracé des courbes en Y.
logical :: lgpubli ! vrai si design pour publication: pas de titre (car il est géré dans une caption, hors graphique), pas de réticule sur le graphique, palettes de couleur pertinentes à la fois en couleur et noir et blanc, etc.
logical :: lgleg ! vrai si la légende (des courbes 1D, de la palette colorée) doit être affichée.

!
!-------------------------------------------------
! Isolignage.
!-------------------------------------------------
!
integer(kind=4), parameter :: jpisol=8 ! nb max de champs isolignables.
character(len=400) :: cgfic_isol(jpisol) ! nom des fichiers contenant des données à isoligner.
integer(kind=4) :: nfic_isol ! nb de fichiers contenant des données à isoligner, demandé par l'utilisateur.
!
!-------------------------------------------------
! Lissage après interpolation.
!-------------------------------------------------
!
logical :: lgliss ! vrai si lissage après interpolation.
real(kind=8) :: rgliss ! taux de lissage: si 1. lissage sur tous les points (mise à plat du champ), si 0. aucun lissage, si 0.05 lissage sur 5% des points, etc.

!
!-------------------------------------------------
! Réticule: lignes principales et secondaires en X et Y.
!-------------------------------------------------
!
logical :: lgreticule

logical :: lginversex ! vrai si on veut inverser le sens de l'axe X.
logical :: lginversey ! vrai si on veut inverser le sens de l'axe Y.


!-------------------------------------------------
! Impacts de foudre.
! Cette chaîne de caractères contient 3 champs: nom du fichier d'impacts, date de début de tracé, date de fin de tracé. Les dates sont AAAAMMJJHH.
!-------------------------------------------------
character(len=400) cgimpf ! IMPacts de Foudre. 
end
