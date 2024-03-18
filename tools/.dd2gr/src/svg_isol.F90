subroutine svg_isol
! --------------------------------------------------------------
! **** *svg_isol* Isolignage d'un champ.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2017-09, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
!
use parametres
use lfagen
#include"implicit_r8i4.h"
character(len=400) :: clmot(40)
real(kind=8), dimension(:), pointer :: zx,zxloc
real(kind=8), dimension(:), pointer :: zy,zyloc
real(kind=8), dimension(:), pointer :: zu,zuloc
real(kind=8), dimension(:), pointer :: zv,zvloc
real(kind=8), dimension(:), pointer :: zc,zcloc
real(kind=8), allocatable :: ztab(:,:)
real(kind=8), allocatable :: ztab2(:,:)
integer(kind=4) :: iisolcn(3) ! Couleur de l'isoligne de valeur minimale.
integer(kind=4) :: iisolcx(3) ! Couleur de l'isoligne de valeur maximale.
!-------------------------------------------------
! Initialisation.
!-------------------------------------------------
cldash='; stroke-dasharray: '//'6,6 ' ! tireté court.
cldash='; stroke-dasharray: '//'0,7 ' ! pointillé.
cldash='; stroke-dasharray: '//'0 ' ! continu.
llrr=.false. ! vrai si isolignes irrégulières de type RR.
!
do jfic_isol=1,nfic_isol
  !
  !-------------------------------------------------
  ! On a nfic_isol champs à isoligner, chacun lu sur un fichier d'entrée différent.
  !-------------------------------------------------
  !
  write(*,fmt=*) ' '
  write(*,fmt=*) '  ---------------------------------------------'
  write(*,fmt=*) ' '
  write(*,fmt=*) '  Tracé d''isolignes demandé : ',trim(cgfic_isol(jfic_isol))
  !
  !-------------------------------------------------
  ! La demande utilisateur est du type:
  ! #ISO FIC=tutu.dta [EC=0.5] [EC1=1.5] [EC2=3.5] [COT=1] [COUL=black] [TAIL=1.]
  ! On la casse en ses différents mots.
  !-------------------------------------------------
  !
  call casc(cgfic_isol(jfic_isol),1,clmot,ilmot)
  !
  !-------------------------------------------------
  ! Initialisation par défaut.
  !-------------------------------------------------
  !
  clindef='akeabsent'
  clfic=clindef
  zindef=-6.12145512
  zec=zindef
  zec1=zindef
  zec2=zindef
  ziso1=zindef
  ziso2=zindef
  iindef=-23151134
  clcoul="black"
  ztail=1.
  zepais=1.
  !
  !-------------------------------------------------
  ! Saisie des paramètres prescrits par l'utilisateur.
  !-------------------------------------------------
  !
  llexpl=.false. ! vrai si l'utilisateur a précisé explicitement le champ 'FIC='.
  do jmot=1,ilmot
    clc=clmot(jmot)
    if(clc(1:index(clc,'=')-1) == 'FIC') then
      llexpl=.true.
      !
      !-------------------------------------------------
      ! Fichier de données.
      !-------------------------------------------------
      !
      clfic=clc(index(clc,'=')+1:)
    elseif(clc(1:index(clc,'=')-1) == 'EC') then
      !
      !-------------------------------------------------
      ! Ecartement d'isolignes.
      !-------------------------------------------------
      !
      clec=clc(index(clc,'=')+1:)
      if(trim(clec) == 'RR') then
        llrr=.true.
      else
        read(clec,fmt=*) zec
      endif
    elseif(clc(1:index(clc,'=')-1) == 'EC1') then
      !
      !-------------------------------------------------
      ! Plus petite isoligne à tracer.
      !-------------------------------------------------
      !
      clec1=clc(index(clc,'=')+1:)
      read(clec1,fmt=*) zec1
    elseif(clc(1:index(clc,'=')-1) == 'EC2') then
      !
      !-------------------------------------------------
      ! Plus grande isoligne à tracer.
      !-------------------------------------------------
      !
      clec2=clc(index(clc,'=')+1:)
      read(clec2,fmt=*) zec2
    elseif(clc(1:index(clc,'=')-1) == 'ISO1') then
      !
      !-------------------------------------------------
      ! Plus petite isoligne tracée.
      !-------------------------------------------------
      !
      clec1=clc(index(clc,'=')+1:)
      read(clec1,fmt=*) ziso1
    elseif(clc(1:index(clc,'=')-1) == 'ISO2') then
      !
      !-------------------------------------------------
      ! Plus grande isoligne tracée.
      !-------------------------------------------------
      !
      clec2=clc(index(clc,'=')+1:)
      read(clec2,fmt=*) ziso2
    elseif(clc(1:index(clc,'=')-1) == 'COUL') then
      !
      !-------------------------------------------------
      ! Choix de la couleur.
      !-------------------------------------------------
      !
      clcoul=clc(index(clc,'=')+1:)
    elseif(clc(1:index(clc,'=')-1) == 'EPAIS') then
      !
      !-------------------------------------------------
      ! Choix de l'épaisseur de trait des isolignes.
      !-------------------------------------------------
      !
      clepais=clc(index(clc,'=')+1:)
      read(clepais,fmt=*) zepais
    elseif(clc(1:index(clc,'=')-1) == 'TAIL') then
      !
      !-------------------------------------------------
      ! Choix de la taille des caractères de label.
      !-------------------------------------------------
      !
      cltail=clc(index(clc,'=')+1:)
      read(cltail,fmt=*) ztail
    elseif(clc(1:5) == '#ISOL') then
      !
      !-------------------------------------------------
      ! En-tête de début de ligne. Rien à faire.
      !-------------------------------------------------
      !
    elseif(clc(1:5) == 'SEUL=') then
      !
      !-------------------------------------------------
      ! Rien de particulier à faire. Cet argument a été utilisé en amont dans le code.
      !-------------------------------------------------
      !
    else
      !
      !-------------------------------------------------
      ! Cas d'entrée erronée de l'utilisateur.
      !-------------------------------------------------
      !
      write(*,fmt=*)
      write(*,fmt=*) 'dd2gr/sv_isol/ERREUR: erreur de syntaxe dans la demande utilisateur ',trim(cgfic_isol(jfic_isol)),'!...'
      write(*,fmt=*) 'La demande non reconnue est ',trim(clc)
      write(*,fmt=*)
      call exit(1)
    endif
  enddo
  if(clfic == clindef) then
    !
    !-------------------------------------------------
    ! Cas particulier où l'utilisateur a juste saisi '#ISOL'.
    ! Il veut isoligner le fichier qui a été colorisé.
    !-------------------------------------------------
    !
    clfic=cgfdta(1)
  endif
  iule=22
  !
  ! Le fichier est-il LFA?
  !
  call lfatest(iule,clfic,lllfa)
  if(lllfa) then
    !
    !-------------------------------------------------
    ! Fichier LFA.
    !-------------------------------------------------
    !
    if(trim(cgformat) == 'XYUVC' .or. trim(cgformat) == 'LLUVC' .and. .not.llexpl) then
      call lfalecgen(clfic,zxloc,zyloc,zuloc,zvloc,zcloc,indta)
    else
      call lfalecgen(clfic,zxloc,zyloc,zcloc,indta)
    endif
  else
    !
    !-------------------------------------------------
    ! Le fichier n'est pas LFA. C'est un fichier ASCII.
    !-------------------------------------------------
    !
    write(*,fmt=*) ' '
    write(*,fmt=*) '  Fichier ASCII de données lu: ',trim(clfic)
    open(iule,file=clfic,form='formatted')
    !
    !-------------------------------------------------
    ! Lecture séquentielle.
    !-------------------------------------------------
    !
    indta=0
    do
      if(trim(cgformat) == 'XYUVC' .or. trim(cgformat) == 'LLUVC' .and. .not.llexpl) then
        !
        !-------------------------------------------------
        ! On isoligne la colonne 5: la couleur (C de XYUVC).
        !-------------------------------------------------
        !
        read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zgol3,zgol4,zctmp
      else
        !
        !-------------------------------------------------
        ! On isoligne la colonne 3.
        !-------------------------------------------------
        !
        read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zctmp
      endif
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
      ! Traitement de la ligne courante.
      !-------------------------------------------------
      !
    enddo
    !
    !-------------------------------------------------
    ! Fermeture du fichier d'entrée.
    !-------------------------------------------------
    !
    close(iule)
    !
    !-------------------------------------------------
    ! Impression.
    !-------------------------------------------------
    !
    write(*,fmt=*) indta,' lignes lues.'
    !
    !-------------------------------------------------
    ! Allocation.
    !-------------------------------------------------
    !
    allocate(zxloc(indta))
    allocate(zyloc(indta))
    allocate(zcloc(indta))
  endif
  !
  !-------------------------------------------------
  ! Ouverture du fichier d'entrée.
  !-------------------------------------------------
  !
  if(.not. lllfa) open(iule,file=clfic,form='formatted')
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  indta1=indta
  indta=0
  indta_lfa=0
  do
    if(lllfa) then
      indta_lfa=indta_lfa+1
      if(indta_lfa > indta1) exit
      zxtmp=zxloc(indta_lfa)
      zytmp=zyloc(indta_lfa)
      zctmp=zcloc(indta_lfa)
    else
      if(trim(cgformat) == 'XYUVC' .or. trim(cgformat) == 'LLUVC' .and. .not.llexpl) then
        !
        !-------------------------------------------------
        ! On isoligne la colonne 5: la couleur (C de XYUVC).
        !-------------------------------------------------
        !
        read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zgol3,zgol4,zctmp
      else
        !
        !-------------------------------------------------
        ! On isoligne la colonne 3.
        !-------------------------------------------------
        !
        read(iule,fmt=*,iostat=ios) zxtmp,zytmp,zctmp
      endif
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
      else
        !
        !-------------------------------------------------
        ! Cas non prévu.
        !-------------------------------------------------
        !
        write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
        call exit(1)
      endif
    endif
    !
    !-------------------------------------------------
    ! Traitement de la ligne courante.
    !-------------------------------------------------
    !
    if(rgxminl /= rindef .and. zxtmp < rgxminl) cycle
    if(rgxmaxl /= rindef .and. zxtmp > rgxmaxl) cycle
    if(rgyminl /= rindef .and. zytmp < rgyminl) cycle
    if(rgymaxl /= rindef .and. zytmp > rgymaxl) cycle
    indta=indta+1
    zxloc(indta)=zxtmp
    zyloc(indta)=zytmp
    zcloc(indta)=zctmp
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(iule)
  write(*,fmt=*) indta,' points au sein du zoom X et Y demandé.'
  if(indta == 0) then
    write(*,fmt=*)
    write(*,fmt=*) 'dd2gr/svg_isol/ERREUR: aucun point dans le zoom demandé !...'
    write(*,fmt=*)
    call exit(1)
  endif
  !
  !-------------------------------------------------
  ! Transfert du tableau dimensionné au nombre de lignes
  ! du fichier à celui dimensionné au nombre de points
  ! au sein du zoom demandé.
  !-------------------------------------------------
  !
  allocate(zx(indta))
  allocate(zy(indta))
  allocate(zc(indta))
  do jcourbes=1,indta
    zx(jcourbes)=zxloc(jcourbes)
    zy(jcourbes)=zyloc(jcourbes)
    zc(jcourbes)=zcloc(jcourbes)
  enddo
  deallocate(zxloc)
  deallocate(zyloc)
  deallocate(zcloc)
  !
  !-------------------------------------------------
  ! Interpolation sur une grille régulière.
  !-------------------------------------------------
  !
  !
  !-------------------------------------------------
  ! Calcul du nombre de points en X et Y sur l'image de sortie.
  !-------------------------------------------------
  !
  call repere_donnees_reg(zx,zy,indta,ipasx,ipasy)
  if(cginterpole == cgindef) then
    !
    !-------------------------------------------------
    ! L'utilisateur n'a pas imposé ce nombre de points.
    ! On le déduit de la taille des données.
    !-------------------------------------------------
    !
    if(ipasx > 0) then
      !
      !-------------------------------------------------
      ! On déduit ce nb de points de la régularité de la grille.
      !-------------------------------------------------
      !
      inx=ipasx
      iny=ipasy
    else
      inx=nint(sqrt(real(indta)))
      inx=min(400,inx)
      iny=inx/2
    endif
  else
    !
    !-------------------------------------------------
    ! L'utilisateur a imposé ce nombre de points.
    !-------------------------------------------------
    !
    read(cginterpole,fmt=*) inx,iny
  endif
  !
  !-------------------------------------------------
  ! Allocation du tableau recevant les données régulières.
  !-------------------------------------------------
  !
  if(allocated(ztab)) deallocate(ztab)
  allocate(ztab(inx,iny))
  !
  !-------------------------------------------------
  ! Initialisation des min/max.
  !-------------------------------------------------
  !
  rxmin=minval(zx)
  rxmax=maxval(zx)
  rymin=minval(zy)
  rymax=maxval(zy)
  !
  !-------------------------------------------------
  ! S'ils sont imposés par l'utilisateur, on force ces min/max à être ceux de
  ! l'utilisateur.
  !-------------------------------------------------
  !
  if(rgxminl /= rindef) rxmin=rgxminl
  if(rgxmaxl /= rindef) rxmax=rgxmaxl
  if(rgyminl /= rindef) rymin=rgyminl
  if(rgymaxl /= rindef) rymax=rgymaxl

  if(lginversex) call permuter(rxmin,rxmax)
  if(lginversey) call permuter(rymin,rymax)
  !
  !-------------------------------------------------
  ! Interpolation de la grille irrégulière vers la grille régulière.
  !-------------------------------------------------
  !
  write(*,fmt=*) ' '
  write(*,fmt=*) '  Interpolation vers une grille régulière ',inx,' x ',iny
  call interpole(rindef,lgextrapolation,zx,zy,zc,indta,inx,iny,ztab)
  !
  !-------------------------------------------------
  ! Recherche des extrêmes du champ interpolé.
  !-------------------------------------------------
  !
  rcmin=rindef
  rcmax=rindef
  ival=0
  zmoy=0.
  zrcm=0.
  do jx=1,inx
    do jy=1,iny
      if(ztab(jx,jy) /= rindef) then
        if(rcmin == rindef) then
          rcmin=ztab(jx,jy)
          rcmax=ztab(jx,jy)
        else
          rcmin=min(ztab(jx,jy),rcmin)
          rcmax=max(ztab(jx,jy),rcmax)
        endif
        !
        !-------------------------------------------------
        ! Cumul pour moyenne, écart-type, etc.
        !-------------------------------------------------
        !
        zmoy=zmoy+ztab(jx,jy)
        zrcm=zrcm+ztab(jx,jy)*ztab(jx,jy)
        ival=ival+1
      endif
    enddo
  enddo
  !
  !-------------------------------------------------
  ! On sauvegarde ces min/max, car après ils vont être modifiés (forcés si égaux
  ! entre eux), or sur l'affichage final des min/max on veut celle réelle du
  ! champ, avant forçage.
  !-------------------------------------------------
  !
  rcmin_reel=rcmin
  rcmax_reel=rcmax
  !
  !-------------------------------------------------
  ! Moyenne, écart-type.
  !-------------------------------------------------
  !
  rcmoy=zmoy/real(ival)
  rcect=sqrt(max(0.,zrcm/real(ival)-rcmoy*rcmoy))
  rcrcm=sqrt(zrcm/real(ival))
  !
  !-------------------------------------------------
  ! Impression des stats.
  !-------------------------------------------------
  !
  zcmin_irreg=minval(zc)
  zcmax_irreg=maxval(zc)
  write(*,fmt=*) '  Champ en entrée: '
  write(*,fmt=*) '    X : ',rxmin,' > ',rxmax
  write(*,fmt=*) '    Y : ',rymin,' > ',rymax
  write(*,fmt=*) '    V irrégulier : ',zcmin_irreg,' > ',zcmax_irreg
  write(*,fmt=*) '    V régulier   : ',rcmin,' > ',rcmax
  !
  !-------------------------------------------------
  ! Extrapolation sur une grille à un élément de plus, de telle façon
  ! que les isolignes touchent le bord du domaine de tracé.
  !-------------------------------------------------
  !
  inx2=inx+1
  iny2=iny+1
  if(allocated(ztab2)) deallocate(ztab2)
  allocate(ztab2(inx2,iny2))
  call extrapole_un(inx,iny,ztab,inx2,iny2,ztab2)
  write(*,fmt=*) '    V extrapolé  : ',minval(ztab2),' > ',maxval(ztab2)
  write(*,fmt=*) ' '
  deallocate(ztab)
  !
  !-------------------------------------------------
  ! Détermination des isolignes à tracer.
  !-------------------------------------------------
  !
  if(ziso1 == zindef) then
    !
    ! L'utilisateur ne fournit pas la valeur mini d'isolignage.
    !
    if(trim(clfic) == trim(cgfdta(1)) .and. cgvmin /= cgindef) then
      !
      ! On cherche à isoligner le champ qui a été colorisé, et pour ce champ un min a été imposé par l'utilisateur. On se sert de ce min pour les isolignes aussi.
      !
      read(cgvmin,fmt=*) rcmin
    endif
    zcmin=rcmin
  else ! zisol
    !
    ! L'utilisateur fournit la valeur mini d'isolignage.
    !
    zcmin=ziso1
  endif
  if(ziso2 == zindef) then
    !
    ! L'utilisateur ne fournit pas la valeur maxi d'isolignage.
    !
    if(trim(clfic) == trim(cgfdta(1)) .and. cgvmax /= cgindef) then
      !
      ! On cherche à isoligner le champ qui a été colorisé, et pour ce champ un max a été imposé par l'utilisateur. On se sert de ce max pour les isolignes aussi.
      !
      read(cgvmax,fmt=*) rcmax
    endif
    zcmax=rcmax
  else ! zisol
    !
    ! L'utilisateur fournit la valeur maxi d'isolignage.
    !
    zcmax=ziso2
  endif
  if(zec == zindef) then
    zec=abs(zcmax-zcmin)/15.
    !
    !-------------------------------------------------
    ! Arrondi à un chiffre significatif.
    !-------------------------------------------------
    !
    call arrr(zec,1,zectmp)
    zec=zectmp
  endif
  if(zec == 0.) then
    write(*,fmt=*) '  Pas d''isolignage.'
    return
  else
    write(*,fmt=*) '  On trace les isolignes tous les ',zec
  endif
  zwidth=1.0e-3*rlxsvg*zepais
  write(clwidth,fmt=*) zwidth ; clwidth=adjustl(adjustr(clwidth))
  !
  !-------------------------------------------------
  ! Combien de niveaux d'isolignes vont-ils être traités?
  !-------------------------------------------------
  !
  if(trim(cgtyppal) == 'LOG') then
    !
    !-------------------------------------------------
    ! Palette de couleurs logarithmique.
    ! Les isolignes vont être logarithmiques.
    !-------------------------------------------------
    !
    iisol=nint(log(zcmax/zcmin)/log(10.))
  else
    !
    !-------------------------------------------------
    ! Palette de couleurs linéaire.
    ! Les isolignes vont être équidistantes.
    !-------------------------------------------------
    !
    iisol=nint(abs(zcmax-zcmin)/zec)+3
  endif
  if(llrr) iisol=3
  do jisol=1,iisol
    if(trim(clcoul) == 'GRAD') then
      !
      !-------------------------------------------------
      ! Isolignes de couleur différente, GRADuelle.
      !-------------------------------------------------
      !
      zfrac=max(0.,min(1.,1.5*real(jisol-1)/real(iisol-1)-0.25))
      iisolcn(1)=200 ; iisolcn(2)=200 ; iisolcn(3)=200
      iisolcx(1)=000 ; iisolcx(2)=000 ; iisolcx(3)=000
      i_r_isol=max(0,min(255,nint(iisolcn(1)+(iisolcx(1)-iisolcn(1))*zfrac)))
      i_v_isol=max(0,min(255,nint(iisolcn(2)+(iisolcx(2)-iisolcn(2))*zfrac)))
      i_b_isol=max(0,min(255,nint(iisolcn(3)+(iisolcx(3)-iisolcn(3))*zfrac)))
      write(clcouli,fmt='(4(a,i3.3))') 'rgb(',i_r_isol,',',i_v_isol,',',i_b_isol,')'
    else
      clcouli=clcoul
    endif
    if(llrr) then
      if(jisol == 1) then
        zisol=0.1
      elseif(jisol == 2) then
        zisol=1.
      elseif(jisol == 3) then
        zisol=10.
      else
        write(*,fmt=*)
        write(*,fmt=*) 'dd2gr/ERREUR: avec #ISOLRR on ne peut avoir que ',iisol,' cas !...'
        write(*,fmt=*)
        call exit(1)
      endif
    elseif(trim(cgtyppal) == 'LOG') then
      zdepart=10.**(real(nint(log(zcmin)/log(10.))))
      zisol=zdepart*10.**real(jisol-1)
    else
      zdepart=zec*(nint(zcmin/zec-0.5))
      zisol=zdepart+zec*real(jisol-1)
    endif
    !
    ! lltrac: vrai si l'isoligne doit être tracée.
    !
    lltrac=zisol >= zcmin .and. zisol <= zcmax
    if(zec1 /= zindef) then
      if(zisol < zec1) lltrac=.false.
    endif
    if(zec2 /= zindef) then
      if(zisol > zec2) lltrac=.false.
    endif
    !if(zisol == 0.) lltrac=.false.
    if(lltrac) then
      !
      !-------------------------------------------------
      ! Cette valeur d'isoligne est bien à tracer.
      !-------------------------------------------------
      !
      write(*,fmt=*) '  Isolignage de la valeur ',zisol
      write(nulsvg,fmt=*) ' ' 
      write(nulsvg,fmt=*) '<!-- Isolignage de l''isoligne ',zisol,' -->'
      !
      !-------------------------------------------------
      ! L'algo d'isolignage est le "marching squares https://en.wikipedia.org/wiki/Marching_squares ".
      !-------------------------------------------------
      !
      !
      !-------------------------------------------------
      ! Taille du pavé.
      !-------------------------------------------------
      !
      zwidth_rect=rlxt/real(inx2-1)
      zheight_rect=rlyt/real(iny2-1)
      iprec=0
      do jx=1,inx2-1
        do jy=1,iny2-1
          !
          !-------------------------------------------------
          ! Coordonnées du coin haut gauche du pavé 2D.
          !-------------------------------------------------
          !
          zxcoin=rxt+zwidth_rect*(jx-1)
          zycoin=ryt+zheight_rect*(iny2-1-jy)
          !
          !-------------------------------------------------
          ! Calcul de la somme au sens des "marches carrées" (marching squares).
          !-------------------------------------------------
          !
          icount=0
          if(ztab2(jx+0,jy+0) > zisol) icount=icount+1
          if(ztab2(jx+1,jy+0) > zisol) icount=icount+2
          if(ztab2(jx+1,jy+1) > zisol) icount=icount+4
          if(ztab2(jx+0,jy+1) > zisol) icount=icount+8
          zx1=zindef
          if(icount == 0 .or. icount == 15) then
            !
            !-------------------------------------------------
            ! Pas d'isoligne dans cette case.
            !-------------------------------------------------
            !
            itrac=0
          elseif(icount == 1 .or. icount == 14) then
            itrac=1
            zx1=zxcoin
            zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
            zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
            zy2=zycoin+zheight_rect
          elseif(icount == 2 .or. icount == 13) then
            itrac=1
            zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
            zx2=zxcoin+zwidth_rect
            zy1=zycoin+zheight_rect
            zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
          elseif(icount == 3 .or. icount == 12) then
            itrac=1
            zx1=zxcoin
            zx2=zxcoin+zwidth_rect
            zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
            zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
            zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
          elseif(icount == 4 .or. icount == 11) then
            itrac=1
            zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
            zx2=zxcoin+zwidth_rect
            zy1=zycoin
            zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
            zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
          elseif(icount == 6 .or. icount == 9) then
            itrac=1
            zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
            zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
            zy1=zycoin
            zy2=zycoin+zheight_rect
          elseif(icount == 7 .or. icount == 8) then
            itrac=1
            zx1=zxcoin
            zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
            zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
            zy2=zycoin
          elseif(icount == 5) then
            itrac=1
            zmoy4=0.25*(ztab2(jx+1,jy+1)+ztab2(jx+1,jy)+ztab2(jx,jy)+ztab2(jx,jy+1))
            if(zmoy4 < zisol) then
              zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
              zx2=zxcoin+zwidth_rect
              zy1=zycoin
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
              !
              !-------------------------------------------------
              ! Tracé d'un segment d'isoligne, allant 
              ! de (zx1,zy1) à (zx2,zy2).
              ! <line x1="368.65714285714284" y1="89.099999999999994" x2="368.65714285714284" y2="540.53999999999996" style="stroke: grey; stroke-width: 0.42075000000000001;"/>
              !-------------------------------------------------
              !
              write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
              write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
              write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
              write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
              write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
              &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
              &,'" style="stroke: ',trim(clcouli) &
              &,trim(cldash) &
              &,'; stroke-width: ',trim(clwidth),';"/>'
              zx1=zxcoin
              zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
              zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
              zy2=zycoin+zheight_rect
            else
              zx1=zxcoin
              zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
              zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
              zy2=zycoin
              !
              !-------------------------------------------------
              ! Tracé d'un segment d'isoligne, allant 
              ! de (zx1,zy1) à (zx2,zy2).
              ! <line x1="368.65714285714284" y1="89.099999999999994" x2="368.65714285714284" y2="540.53999999999996" style="stroke: grey; stroke-width: 0.42075000000000001;"/>
              !-------------------------------------------------
              !
              write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
              write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
              write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
              write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
              write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
              &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
              &,'" style="stroke: ',trim(clcouli) &
              &,trim(cldash) &
              &,'; stroke-width: ',trim(clwidth),';"/>'
              zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
              zx2=zxcoin+zwidth_rect
              zy1=zycoin+zheight_rect
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
            endif
          elseif(icount == 10) then
            itrac=1
            zmoy4=0.25*(ztab2(jx+1,jy+1)+ztab2(jx+1,jy)+ztab2(jx,jy)+ztab2(jx,jy+1))
            if(zmoy4 < zisol) then
              zx1=zxcoin
              zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
              zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
              zy2=zycoin
              !
              !-------------------------------------------------
              ! Tracé d'un segment d'isoligne, allant 
              ! de (zx1,zy1) à (zx2,zy2).
              ! <line x1="368.65714285714284" y1="89.099999999999994" x2="368.65714285714284" y2="540.53999999999996" style="stroke: grey; stroke-width: 0.42075000000000001;"/>
              !-------------------------------------------------
              !
              write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
              write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
              write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
              write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
              write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
              &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
              &,'" style="stroke: ',trim(clcouli) &
              &,trim(cldash) &
              &,'; stroke-width: ',trim(clwidth),';"/>'
              zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
              zx2=zxcoin+zwidth_rect
              zy1=zycoin+zheight_rect
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
            else
              zx1=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy+1),ztab2(jx+1,jy+1),zisol)
              zx2=zxcoin+zwidth_rect
              zy1=zycoin
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
              zy2=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx+1,jy),ztab2(jx+1,jy+1),zisol)
              !
              !-------------------------------------------------
              ! Tracé d'un segment d'isoligne, allant 
              ! de (zx1,zy1) à (zx2,zy2).
              ! <line x1="368.65714285714284" y1="89.099999999999994" x2="368.65714285714284" y2="540.53999999999996" style="stroke: grey; stroke-width: 0.42075000000000001;"/>
              !-------------------------------------------------
              !
              write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
              write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
              write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
              write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
              write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
              &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
              &,'" style="stroke: ',trim(clcouli) &
              &,trim(cldash) &
              &,'; stroke-width: ',trim(clwidth),';"/>'
              zx1=zxcoin
              zx2=zxcoin+zwidth_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx+1,jy),zisol)
              zy1=zycoin+zheight_rect-zheight_rect*zerolin(icount,ztab2(jx,jy),ztab2(jx,jy+1),zisol)
              zy2=zycoin+zheight_rect
            endif
          else
            write(*,fmt=*)
            write(*,fmt=*) 'dd2gr/ERREUR: cas non prévu de icount !...'
            write(*,fmt=*) icount
            call exit(1)
          endif
          if(itrac == 1) then
            !
            !-------------------------------------------------
            ! Tracé d'un segment d'isoligne, allant 
            ! de (zx1,zy1) à (zx2,zy2).
            ! <line x1="368.65714285714284" y1="89.099999999999994" x2="368.65714285714284" y2="540.53999999999996" style="stroke: grey; stroke-width: 0.42075000000000001;"/>
            !-------------------------------------------------
            !
            write(clx1,fmt=*) zx1 ; clx1=adjustl(adjustr(clx1))
            write(clx2,fmt=*) zx2 ; clx2=adjustl(adjustr(clx2))
            write(cly1,fmt=*) zy1 ; cly1=adjustl(adjustr(cly1))
            write(cly2,fmt=*) zy2 ; cly2=adjustl(adjustr(cly2))
            write(nulsvg,fmt='(100a)') '<line x1="',trim(clx1),'" y1="',trim(cly1) &
            &,'" x2="',trim(clx2),'" y2="',trim(cly2) &
            &,'" style="stroke: ',trim(clcouli) &
            &,trim(cldash) &
            &,'; stroke-width: ',trim(clwidth),';"/>'
          endif
          !
          !-------------------------------------------------
          ! Cotation des isolignes.
          !-------------------------------------------------
          !
          imodul=inx2*iny2/5 ! tous les combien de points de grille on cote.
          if(zx1 /= zindef .and. iprec > imodul) then
            iprec=0
            !
            !-------------------------------------------------
            ! On va coter ce point (coter = écriture d'un label).
            !-------------------------------------------------
            !
            call cotation(zx1,zx2,zy1,zy2,zec,zisol,clcouli,ztail)
          endif
          iprec=iprec+1
          !
          !-------------------------------------------------
          ! Impression des extrema locaux.
          !-------------------------------------------------
          !
          if(jx > 1 .and. jy > 1) then
            if(ztab2(jx,jy) > ztab2(jx,jy-1) & 
              & .and. ztab2(jx,jy) > ztab2(jx,jy+1) &
              & .and. ztab2(jx,jy) > ztab2(jx-1,jy) &
              & .and. ztab2(jx,jy) > ztab2(jx+1,jy)) then
              clextr='H'
            elseif(ztab2(jx,jy) < ztab2(jx,jy-1) & 
              & .and. ztab2(jx,jy) < ztab2(jx,jy+1) &
              & .and. ztab2(jx,jy) < ztab2(jx-1,jy) &
              & .and. ztab2(jx,jy) < ztab2(jx+1,jy)) then
              clextr='L'
            else
              clextr='pas'
            endif
            if(trim(clextr) /= 'pas') then
              !
              !-------------------------------------------------
              ! Le point courant est un extremum local.
              !-------------------------------------------------
              !
              ztaille_leg=rgtaille_fonte*rfont_leg*0.55*ztail
              ixtxt=nint(zxcoin+0.0*zwidth_rect)
              iytxt=nint(zycoin+1.0*zheight_rect+0.5*ztaille_leg)
              write(clsvg_leg,fmt='(a,i5,a,i5,3a,g16.7,5a)') '<text x="' &
              & ,ixtxt,'" y="' &
              & ,iytxt,'" ',trim(cgfonte_texte),' font-size="',ztaille_leg &
              & ,'" text-anchor="middle" fill="',trim(clcouli),'" >' &
              & ,trim(clextr),'</text>'
              
              clsvg_legn=cl_nettoie_blancs(clsvg_leg)
              !write(nulsvg,fmt='(9a)') ' '
              !write(nulsvg,fmt='(9a)') '<!-- Ecriture d''un extremum local. -->'
              !write(nulsvg,fmt='(a)') trim(clsvg_legn)
            endif
          endif
        enddo ! jy
      enddo ! jx
    endif
  enddo ! jisol
  deallocate(zx)
  deallocate(zy)
  deallocate(zc)
enddo ! fin bcl jfic_isol sur les fichiers de données à tracer.
end
