subroutine lit_directives(cdfic)
! --------------------------------------------------------------
! **** ** Lectre d'un fichier de type ".doc".
! Methode:
! Externes:
! Auteur:   2010-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entrée: 
! En sortie:
! --------------------------------------------------------------
use parametres
#include"implicit_r8i4.h"
character(len=*) :: cdfic
character(len=400) :: clmot(40)
write(*,fmt=*) '  Lecture du fichier de directives : ',trim(cdfic)
iule=22 ; open(iule,file=cdfic,form='formatted')
cltitre_add=' ' ! titre additionnel.
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
indta=0
do
  read(iule,fmt='(a)',iostat=ios) clc
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
    indta=indta+1
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
  ! Affichage informatif de la directive lue.
  !-------------------------------------------------
  !
  if(clc(1:1) == '#') then
    write(*,fmt=*) '      ',trim(clc)
  endif
  !
  !-------------------------------------------------
  ! Tout ce qui est à droite d'un '!' est du commentaire.
  !-------------------------------------------------
  !
  iposc=index(clc,'!')
  if(iposc >= 2) then
    !
    ! La ligne comporte un '!' en position 2 ou au-delà.
    ! On enlève tous les caractères à droite de ce '!'.
    !
    clc=clc(1:iposc-1)
  endif
  !
  !-------------------------------------------------
  ! Traitement de la ligne courante.
  !-------------------------------------------------
  !
  if(clc == ' ') then
    ! Ligne vide. Rien à faire.
  elseif(clc(1:1) /= '#') then
    ! Ligne de commentaire. Rien à faire.
  elseif(clc(1:index(clc,'=')-1) == '#FORMAT') then
    cgformat=clc(index(clc,'=')+1:)
    if(trim(cgformat) == 'LLV') lgxy_isotropes=.true.
  elseif(clc(1:index(clc,'=')-1) == '#TEXTE') then
    ntexl=ntexl+1
    cgtexl(ntexl)=clc
  elseif(clc(1:index(clc,'=')-1) == '#DATE') then
    cgdate=clc(index(clc,'=')+1:)
    call ote_et_commercial(cgdate,cgdate)
  elseif(clc(1:index(clc,'=')-1) == '#MC_COUL_PER') then
    read(clc(index(clc,'=')+1:),fmt=*) ncoulper
  elseif(clc(1:index(clc,'=')-1) == '#MC_TRAI_PER') then
    read(clc(index(clc,'=')+1:),fmt=*) ntraiper
  elseif(clc(1:index(clc,'=')-1) == '#GRILLE') then
    if(cgdate == ' ') then
      cgdate=clc(index(clc,'=')+1:)
    else
      cgdate=trim(cgdate)//', '//clc(index(clc,'=')+1:)
    endif
    call ote_et_commercial(cgdate,cgdate)
  elseif(trim(clc) == '#INVERSE_X') then
    lginversex=.true.
  elseif(trim(clc) == '#INVERSE_Y') then
    lginversey=.true.
  elseif(clc(1:index(clc,'=')-1) == '#IMPACTS') then
    cgimpf=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#INTERPOLE') then
    !
    !-------------------------------------------------
    ! Définition de la grille sur laquelle les valeurs
    ! irrégulières vont être interpolées.
    ! Exemple: "#INTERPOLE= 400 650"
    !-------------------------------------------------
    !
    cginterpole=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#METHODE_EXTRAPOLATION') then
    !
    !-------------------------------------------------
    ! Choix de la méthode pour extrapoler, i.e. la méthode
    ! pour chercher, lorsqu'un point de grille est en valeur manquante,
    ! sa valeur à partir des points voisins.
    ! Exemple: #METHODE_EXTRAPOLATION=CONCENTRIQUE
    ! ou #METHODE_EXTRAPOLATION=Y_PUIS_X
    !-------------------------------------------------
    !
    cgextrmeth=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#TITRE') then
    cgtitre=clc(index(clc,'=')+1:)
    call ote_et_commercial(cgtitre,cgtitre)
  elseif(clc(1:index(clc,'=')-1) == '#TITRE_ADD') then
    cltitre_add=clc(index(clc,'=')+1:)
    call ote_et_commercial(cltitre_add,cltitre_add)
  elseif(clc(1:index(clc,'=')-1) == '#TITRE_COUL') then
    cgcoult=clc(index(clc,'=')+1:)
    call ote_et_commercial(cgcoult,cgcoult)
  elseif(clc(1:index(clc,'=')-1) == '#FICHIER') then
    ncourbes=ncourbes+1
    if(ncourbes > jpcourbes) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpcourbes !...'
      write(*,fmt=*) jpcourbes
      call exit(1)
    endif
    if(trim(cgformat) == 'XYV' .or. trim(cgformat) == 'HAS' .or. trim(cgformat) == 'HAA' .or. trim(cgformat) == 'LLV' &
      & .or. trim(cgformat) == 'LLUVC' .or. trim(cgformat) == 'XYUVC') then
      !
      !-------------------------------------------------
      ! Un seul fichier de données possible.
      ! Si l'utilisateur en a indiqué plusieurs c'est le dernier indiqué qui
      ! sera lu. On force donc ici ncourbes à ne pas s'incrémenter.
      !-------------------------------------------------
      !
      ncourbes=1
    endif
    cgfdta(ncourbes)=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#CHAMP' .or. clc(1:index(clc,'=')-1) == '#CHAMP_ISOL') then
    nchamp=nchamp+1
    if(nchamp > jpcourbes) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: recompiler avec une valeur plus grande de jpcourbes !...'
      write(*,fmt=*) jpcourbes
      call exit(1)
    endif
    cgchamp(nchamp)=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#RETICULE') then
    !
    !-------------------------------------------------
    ! Réticule: lignes principales et secondaires en X et Y.
    !-------------------------------------------------
    !
    clreticule=clc(index(clc,'=')+1:)
    if(trim(clreticule) == 'oui') then
      lgreticule=.true.
    else
      lgreticule=.false.
    endif
  elseif(clc(1:index(clc,'=')-1) == '#COUL') then
    !
    !-------------------------------------------------
    ! Couleur de la courbe, sous forme d'un nom (ex: #COUL=black), ou d'un RVB
    ! (ex: #COUL=rgb(248,128,23) ).
    !-------------------------------------------------
    !
    if(trim(cgformat) == 'XYI' .and. nchamp == 0) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: la ligne #COUL doit être positionnée après la ligne #CHAMP !...'
      write(*,fmt=*)
      call exit(1)
    endif
    cgcoul(nchamp)=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#PLUS') then
    !
    !-------------------------------------------------
    ! On doit "plusser" cette courbe, i.e. écrire un + à chaque début de segment.
    !-------------------------------------------------
    !
    lgplusse(nchamp)=.true.
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille du "+".
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rgplusse
  elseif(clc(1:index(clc,'=')-1) == '#LABEL_FORMAT') then
    !
    !-------------------------------------------------
    ! Format FORTRAN d'écriture des labels, dans le cas du pointage irrégulier.
    !-------------------------------------------------
    !
    cglabf=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDE_FORMAT') then
    !
    !-------------------------------------------------
    ! Format FORTRAN d'écriture des nombres réels dans la légende de la palette
    ! colorée.
    !-------------------------------------------------
    !
    cglegf=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#LABEL_TAILLE') then
    !
    !-------------------------------------------------
    ! Taille d'écriture des labels, dans le cas du pointage irrégulier.
    !-------------------------------------------------
    !
    cglabt=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#LABEL_ANGLE') then
    !
    !-------------------------------------------------
    ! Angle d'écriture des labels, dans le cas du pointage irrégulier.
    !-------------------------------------------------
    !
    cglaba=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#FONTE_TAILLE') then
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille des caractères.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rfont
  elseif(clc(1:index(clc,'=')-1) == '#XRET') then
    !
    !-------------------------------------------------
    ! Ecart en X entre 2 réticules.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rgxret
  elseif(clc(1:index(clc,'=')-1) == '#YRET') then
    !
    !-------------------------------------------------
    ! Ecart en Y entre 2 réticules.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rgyret
  elseif(clc(1:index(clc,'=')-1) == '#FONTE_TAILLE_AXES') then
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille des caractères.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rfont_axes
  elseif(clc(1:index(clc,'=')-1) == '#FONTE_TAILLE_LEGENDE') then
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille des caractères.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rfont_leg
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDE') then
    !
    !-------------------------------------------------
    ! Affichage de la légende des courbes et de la palette.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    if(trim(cldroit) == 'oui') then
      lgleg=.true.
    else
      lgleg=.false.
    endif
  elseif(clc(1:index(clc,'=')-1) == '#PUBLI') then
    !
    !-------------------------------------------------
    ! Graphique pour publication ou développement.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    if(trim(cldroit) == 'oui') then
      lgpubli=.true.
      lgreticule=.true.
    endif
  elseif(clc(1:index(clc,'=')-1) == '#FONTE_TAILLE_UNITE') then
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille des caractères.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rfont_unite
  elseif(clc(1:index(clc,'=')-1) == '#RVB_INDEF') then
    !
    !-------------------------------------------------
    ! Couleur RVB des valeurs manquantes.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) nrvb_indef(1),nrvb_indef(2),nrvb_indef(3)
  elseif(clc(1:index(clc,'=')-1) == '#RVB_NEGL') then
    !
    !-------------------------------------------------
    ! Couleur RVB des valeurs négligeables.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) nrvb_negl(1),nrvb_negl(2),nrvb_negl(3)
  elseif(clc(1:index(clc,'=')-1) == '#LISSAGE') then
    !
    !-------------------------------------------------
    ! Lissage sur une fraction rgliss des points après interpolation.
    !-------------------------------------------------
    !
    lgliss=.true.
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rgliss
  elseif(clc(1:index(clc,'=')-1) == '#FONTE_TAILLE_TITRE') then
    !
    !-------------------------------------------------
    ! Coef. multiplicateur à appliquer à la taille des caractères.
    !-------------------------------------------------
    !
    cldroit=clc(index(clc,'=')+1:)
    read(cldroit,fmt=*) rfont_titre
  elseif(clc(1:5) == '#ISOL') then
    nfic_isol=nfic_isol+1
    if(nfic_isol > jpisol) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: plus de champs à isoligner que jpisol !...'
      write(*,fmt=*) 'jpisol=',jpisol
      write(*,fmt=*) 'Recompiler avec jpisol plus grand.'
      write(*,fmt=*)
      call exit(1)
    endif
    cgfic_isol(nfic_isol)=clc
    !
    !-------------------------------------------------
    ! La demande utilisateur est du type:
    ! #ISO FIC=tutu.dta [EC=0.5] [EC1=1.5] [EC2=3.5] [COT=1] [COUL=black] [TAIL=1.] [SEUL=oui]
    ! On la casse en ses différents mots.
    !-------------------------------------------------
    !
    call casc(cgfic_isol(nfic_isol),1,clmot,ilmot)
    do jmot=1,ilmot
      if(clmot(jmot)(1:8) == 'SEUL=oui') then
        lgc2d=.false.
      endif
    enddo
  elseif(clc(1:index(clc,'=')-1) == '#RAYON_POINTAGE_LIEU') then
    read(clc(index(clc,'=')+1:),fmt=*) rgfacp
  elseif(clc(1:index(clc,'=')-1) == '#POIN') then
    !
    !-------------------------------------------------
    ! Courbe continue: #POIN=non, ou ligne #POIN absente
    ! Courbe en tireté court: #POIN=5,5
    ! Courbe en tireté long : #POIN=11,11
    ! Courbe mixte: #POIN=10,3,2,3
    ! Rayon du disque dans le cas de pointage irrégulier: #POIN=1.
    !-------------------------------------------------
    !
    if(trim(cgformat) == 'XYI' .and. nchamp == 0) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: la ligne #POIN doit être positionnée après la ligne #CHAMP !...'
      write(*,fmt=*)
      call exit(1)
    endif
    cgpoin(nchamp)=clc(index(clc,'=')+1:)
    if(nchamp == 0) lgpointage=.true.
  elseif(clc(1:index(clc,'=')-1) == '#LARG') then
    !
    !-------------------------------------------------
    ! Largeur de trait de la courbe courante, exprimée en fraction de la largeur
    ! standard. Par ex, pour avoir une courbe deux fois plus fine que les
    ! autres: #LARG=0.5 . 
    !-------------------------------------------------
    !
    if(nchamp == 0) then
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/ERREUR: la ligne #LARG doit être positionnée après la ligne #CHAMP !...'
      write(*,fmt=*)
      call exit(1)
    endif
    cglarg(nchamp)=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#UNITE') then
    cgunite=clc(index(clc,'=')+1:)
    call ote_et_commercial(cgunite,cgunite)
  elseif(clc(1:index(clc,'=')-1) == '#TYPE_AXE_X') then
    cgtypax=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#TYPE_AXE_Y') then
    cgtypay=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#ORIGINE') then
    cgorigine=clc(index(clc,'=')+1:)
    call ote_et_commercial(cgorigine,cgorigine)
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDE_X') then
    cglegx=clc(index(clc,'=')+1:)
    call ote_et_commercial(cglegx,cglegx)
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDE_Y') then
    cglegy=clc(index(clc,'=')+1:)
    call ote_et_commercial(cglegy,cglegy)
  elseif(clc(1:index(clc,'=')-1) == '#REGX') then
    cgregx=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#REGY') then
    cgregy=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#NEGLIG') then
    !
    !-------------------------------------------------
    ! Si l'utilisateur écrit "#NEGLIG=x" c'est que le réel x
    ! doit être utilisé comme valeur considérée comme négligeable,
    ! i.e. toute valeur du champ inférieure en valeur absolue
    ! à x est tracée avec la couleur du négligeable.
    !-------------------------------------------------
    !
    clneglig=clc(index(clc,'=')+1:)
    read(clneglig,fmt=*) rneglig
  elseif(clc(1:index(clc,'=')-1) == '#COUL_VECTEURS') then
    cgcoul_vec=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#ECHELLE_VECTEURS') then
    clech_vec=clc(index(clc,'=')+1:)
    read(clech_vec,fmt=*) rech_vec
  elseif(clc(1:index(clc,'=')-1) == '#SAUT_VECTEURS') then
    clsaut_vec=clc(index(clc,'=')+1:)
    read(clsaut_vec,fmt=*) nsaut_vec
  elseif(clc(1:4) == '#FDC') then
    lgfdc=.true.
    nfdc=nfdc+1
    cgfdc(nfdc)=clc
  elseif(clc(1:index(clc,'=')-1) == '#LANGAGE') then
    cglang=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#TYPE_PALETTE') then
    cgtyppal=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#PALETTE') then
    cgpal=clc(index(clc,'=')+1:)
    npalspec=0
  elseif(clc(1:index(clc,'=')-1) == '#PAL_SPECIF') then
    npalspec=npalspec+1
    cgpalspec(npalspec)=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#SENS_PALETTE') then
    cg_sens_palette=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#TRACE_AXES') then
    cgtrace_axes=clc(index(clc,'=')+1:)
  elseif(trim(clc) == '#NOMINMAX') then
    lgminmax=.false.
  elseif(clc(1:index(clc,'=')-1) == '#VMIN' .or. clc(1:index(clc,'=')-1) == '#VN') then
    cgvmin=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#VMAX' .or. clc(1:index(clc,'=')-1) == '#VX') then
    cgvmax=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#EXTDYN') then
    !
    !-------------------------------------------------
    ! L'utilisateur a fourni par ex "#EXTDYN=3.7", ce qui veut dire qu'il
    ! souhaite des EXTrêmes de tracé DYNamiques, calculés comme moyenne + 3.7
    ! fois l'écart-type.
    !-------------------------------------------------
    !
    cgextdyn=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#PAL_DEF_CST') then
    !
    !-------------------------------------------------
    ! Palette par défaut pour les champs de signe de constant.
    !-------------------------------------------------
    !
    cgpaldef_cst=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#PAL_DEF_NEG') then
    !
    !-------------------------------------------------
    ! Palette par défaut pour les valeurs négatives.
    !-------------------------------------------------
    !
    cgpaldef_neg=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#PAL_DEF_POS') then
    !
    !-------------------------------------------------
    ! Palette par défaut pour les valeurs positives.
    !-------------------------------------------------
    !
    cgpaldef_pos=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#IMAGE') then
    !
    !-------------------------------------------------
    ! Résolution en pixels de l'image finale de sortie.
    ! Exemple: #IMAGE=650 400
    ! pour créer une image de 650 pixels en X, 400 pixels en Y.
    !-------------------------------------------------
    !
    cgimage=clc(index(clc,'=')+1:)
    lgimaf=.true.
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDE_PIX') then
    !
    !-------------------------------------------------
    ! Taille en pixels de la légende de palette de couleurs.
    ! Exemple: #LEGENDE_PIX= 300 500
    !-------------------------------------------------
    !
    cglegende=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#EXTRAPOLE') then
    !
    !-------------------------------------------------
    ! Si #EXTRAPOLE=non dd2gr fonctionne en mode "pointage" seulement,
    ! et affecte une couleur de type manquante aux pavés
    ! de la grille régulière de sortie ne recevant aucun
    ! point de la grille irrégulière d'entrée.
    !
    ! Sinon dd2gr fonctionne en mode extrapolation, de façon 
    ! à affecter une valeur à tout point de la grille régulière de sortie.
    !-------------------------------------------------
    !
    cgextrapolation=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#LEGENDEXY') then
    cglegendexy=clc(index(clc,'=')+1:)
  elseif(clc(1:index(clc,'=')-1) == '#X_MIN_LIMIT' .or. clc(1:index(clc,'=')-1) == '#XN') then
    cllec=clc(index(clc,'=')+1:)
    read(cllec,fmt=*) rgxminl
  elseif(clc(1:index(clc,'=')-1) == '#X_MAX_LIMIT' .or. clc(1:index(clc,'=')-1) == '#XX') then
    cllec=clc(index(clc,'=')+1:)
    read(cllec,fmt=*) rgxmaxl
  elseif(clc(1:index(clc,'=')-1) == '#Y_MIN_LIMIT' .or. clc(1:index(clc,'=')-1) == '#YN') then
    cllec=clc(index(clc,'=')+1:)
    read(cllec,fmt=*) rgyminl
  elseif(clc(1:index(clc,'=')-1) == '#Y_MAX_LIMIT' .or. clc(1:index(clc,'=')-1) == '#YX') then
    cllec=clc(index(clc,'=')+1:)
    read(cllec,fmt=*) rgymaxl
  elseif(trim(clc) == '#XY_ISOTROPES=oui') then
    lgxy_isotropes=.true.
  elseif(trim(clc) == '#XY_ISOTROPES=non') then
    lgxy_isotropes=.false.
  else
    write(*,fmt=*) '  dd2gr/INFO: ligne ignorée : ',trim(clc)
  endif
enddo
if(cltitre_add /= ' ') then
  
  !-------------------------------------------------
  ! On ajoute le titre additionnel au titre.
  !-------------------------------------------------
  cgtitre=trim(cgtitre)//', '//trim(cltitre_add)
endif
close(iule)
end
