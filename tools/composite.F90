subroutine composite(cdlegx,klev,knfic,phsl,pneb,pccb,knlabPOST_FORC, &
& paut,pegn,pfm,pqc,pcape,pcin,pbuoy,pindef,ppcl,ppcn,ppsl,ppsn,&
& ktypeprec &
& ,cdfcompo,kxcompo,kycompo,pcooz,cdindice,phcla,pprr)
! --------------------------------------------------------------
! **** ** Création d'une image composite de nébulosité / précip..
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
! En sortie:
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,j,k)
real(kind=8) :: phsl(knfic)
real(kind=8) :: phcla(knfic)
real(kind=8) :: pneb(knfic,klev)
real(kind=8) :: pccb(knfic,klev)
real(kind=8) :: paut(knfic,klev)
real(kind=8) :: pegn(knfic,klev)
real(kind=8) :: pfm(knfic,klev)
integer(kind=4) :: knlabPOST_FORC(knfic,klev)
real(kind=8) :: pqc(knfic,klev)
real(kind=8) :: pcape(knfic)
real(kind=8) :: pcin(knfic)
real(kind=8) :: pbuoy(knfic,klev)
real(kind=8) :: ppcl(knfic,klev+1)
real(kind=8) :: ppcn(knfic,klev+1)
real(kind=8) :: ppsl(knfic,klev+1)
real(kind=8) :: ppsn(knfic,klev+1)
real(kind=8) :: zp(knfic,klev+1)
integer(kind=4) :: ktypeprec(knfic,klev+1)
integer(kind=4), allocatable :: irvb(:,:,:),irvb_leg(:,:,:),irvb_final(:,:,:)
integer(kind=4) :: irvb_bord(3),irvb_fond_texte(3),irvb_pp_texte(3),irvb_loc(3)
character*(*) cdfcompo
character*(*) cdindice
character*(*) cdlegx
real(kind=8) :: pcooz(klev)
real(kind=8), allocatable :: zval(:),zx(:),zy(:)
integer(kind=4) :: iloc(3)
!
!-------------------------------------------------
! Initialisations.
!-------------------------------------------------
!
zzsurf=pcooz(klev)-0.5*(pcooz(klev-1)-pcooz(klev)) ! altitude de la surface (par rapport à la mer).
if(pfm(1,1) == pindef) then
  !
  !-------------------------------------------------
  ! On ne dipose pas du flux de masse.
  !-------------------------------------------------
  !
  llfm=.false.
else
  !
  !-------------------------------------------------
  ! On dipose de du flux de masse.
  !-------------------------------------------------
  !
  llfm=.true.
endif
!
!-------------------------------------------------
! Légende. On crée la légende d'un champ entre zxmin et zxmax,
! avec la palette clpal, légende dont la taille pixels est (ipixx,ipixy).
!-------------------------------------------------
!
ipixx=kxcompo
ipixy=40
allocate(irvb_leg(3,ipixx,ipixy))
if(llfm) then
  !
  ! Cas où le flux de masse est disponible: contourage du flux de masse.
  !
  zminfm=0. ; zmaxfm=0.12 ! min/max des flux de masse (en kg/m2/s).
  inbprint=0
  clpal='VIOLET-BLANC'
  clpal='ARC-EN-CIEL'
  clpal='OROG'
  clpal='BLANC-NOIR'
  clpal='VEG'
  clpal='CONTRASTE'
else
  !
  ! Autres cas. Contourage de la nébulosité.
  !
  zminfm=0. ; zmaxfm=1.
  inbprint=0
  clpal='OROG'
  clpal='VIOLET-BLANC'
  clpal='BLANC-NOIR'
endif
call img_pal_init
call img_legende(zminfm,zmaxfm,inbprint,clpal,ipixx,ipixy,irvb_leg)
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
iycoeur=kycompo-ipixy
allocate(irvb(3,kxcompo,iycoeur))
!
!-------------------------------------------------
! Initialisation à blanc.
!-------------------------------------------------
!
irvb=255
ixleg=30 ! nombre de pixels en X pour légender l'axe vertical.
iyleg=30 ! nombre de pixels en Y pour légender l'axe horizontal.
!
!-------------------------------------------------
! Allocation du lien entre pixels verticaux et z.
!-------------------------------------------------
!
allocate(zval(iycoeur-iyleg))
allocate(zx(iycoeur-iyleg))
allocate(zy(iycoeur-iyleg))
!
!-------------------------------------------------
! Traits noirs séparant la légende du graphique.
!-------------------------------------------------
!
!
! Trait horizontal.
!
do jx=ixleg,kxcompo
  do jc=1,3
    irvb(jc,jx,iycoeur-iyleg+1)=000
  enddo
enddo
!
! Trait vertical.
!
do jy=1,iycoeur-iyleg
  do jc=1,3
    irvb(jc,ixleg,jy)=000
  enddo
enddo
zmax=15. ! hauteur maxi en km.
!
!-------------------------------------------------
! Max des précip..
!-------------------------------------------------
!
zp=(ppsl+ppsn+ppcl+ppcn)*86400.
zmaxp=max(0.2,maxval(zp))
!
!-------------------------------------------------
! Remplissage de l'image par les nuages et les précip..
!-------------------------------------------------
!
zal=0.254125
iindef=-52421
iycla_prec=iindef
do jx=ixleg+1,kxcompo
  llcla=.false. ! vrai si on est dans la CLA.
  !
  !-------------------------------------------------
  ! A quel fichier LFA correspond le pixel courant?
  !-------------------------------------------------
  !
  ific=max(1,min(knfic,int((real(jx-(ixleg+1))+0.5)/(real(kxcompo-(ixleg+1))+1.)*real(knfic))+1))
  do jy=1,iycoeur-iyleg
    !
    !-------------------------------------------------
    ! On veut que l'image ait une coordonnée z, limitée à zmax.
    ! La relation ci-dessous, donnant z en fonction de l'ordonnée
    ! en pixels peut être choisie comme on veut:
    ! la cotation de la verticale la prend en compte dynamiquement.
    ! zcourant est une altitude par rapport à la mer, en km.
    !-------------------------------------------------
    !
    ! z = fonction linéaire de jy en pixels.
    !zcourant=(1.-(real(jy-1)+0.5)/(real(iycoeur-iyleg-1)+1.))*zmax+zzsurf
    !
    ! z = fonction quadratique de (1-jy) en pixels: crée un zoom sur les basses couches.
    zcourant=(((1.-(real(jy-1)+0.5)/(real(iycoeur-iyleg-1)+1.)))**1.5)*zmax+zzsurf
    !
    !-------------------------------------------------
    ! On renseigne les tableaux de coordonnée verticale,
    ! pour historiser la relation z = f(pixels y) ci-dessus.
    !-------------------------------------------------
    !
    zval(iycoeur-iyleg-jy+1)=zcourant
    zx(iycoeur-iyleg-jy+1)=real(ixleg)/2.
    zy(iycoeur-iyleg-jy+1)=real(jy)
    !
    !-------------------------------------------------
    ! Quel niveau correspond à cette hauteur zcourant?
    !-------------------------------------------------
    !
    do jlev=1,klev
      ilevtrac=jlev
      if(pcooz(jlev) < zcourant) exit
    enddo
    igris=210
    if(jy == 1) then
      ilevtrac_prec=ilevtrac
      ir=igris
      iv=igris
      ib=igris
    else
      if(ilevtrac /= ilevtrac_prec .or. (jy == iycoeur-iyleg .and. .not.llcla)) then
        !
        !-------------------------------------------------
        ! Changement de niveau modèle.
        !-------------------------------------------------
        !
        if((zcourant < (zzsurf+phcla(ific)/1000.) .and. .not.llcla) &
        & .or. (jy == iycoeur-iyleg .and. .not.llcla)) then
          !
          !-------------------------------------------------
          ! C'est la 1ère fois qu'on entre dans la CLA.
          !-------------------------------------------------
          !
          ir=177 ; iv=251 ; ib=023 ! jaune-vert.
          ir=000 ; iv=000 ; ib=000 ! noir.
          if(iycla_prec == iindef) then
            iycla_prec=jy
          elseif(jy /= iycla_prec .and. .not.llcla) then
            !
            !-------------------------------------------------
            ! On fait une ligne verticale pour créer une ligne
            ! continue matérialisant le sommet de la CLA.
            !-------------------------------------------------
            !
            do jy2=min(iycla_prec,jy),max(iycla_prec,jy)
              irvb(1,jx,jy2)=ir
              irvb(2,jx,jy2)=iv
              irvb(3,jx,jy2)=ib
            enddo
          endif
          llcla=.true.
          iycla_prec=jy
        else
          !
          !-------------------------------------------------
          ! On est au-dessus de la CLA, ou dedans, mais pas au sommet.
          !-------------------------------------------------
          !
          ir=igris ; iv=igris ; ib=igris ! gris.
        endif
        ilevtrac_prec=ilevtrac
      else
        ilevf=ilevtrac
        ilevv=ilevtrac
        if(.not. llfm) then
          !
          !-------------------------------------------------
          ! Le fichier courant ne contient pas d'information
          ! sur l'activité convective locale.
          ! Le fond d'écran est fonction de la nébulosité.
          !-------------------------------------------------
          !
          !-------------------------------------------------
          ! Couleur liée à la nébulosité.
          !-------------------------------------------------
          !
          zfrfm=max(0.,min(1.,(pneb(ific,ilevv)-zminfm)/(zmaxfm-zminfm)))
          call img_pal(clpal,'CONT','FRAC',zfrfm,irvb_loc)
          ir=irvb_loc(1)
          iv=irvb_loc(2)
          ib=irvb_loc(3)
        else
          !
          !-------------------------------------------------
          ! Le fichier contient de l'information
          ! sur l'activité convective locale.
          ! Le fond d'écran est fonction de cette activité.
          !-------------------------------------------------
          !
          if(pfm(ific,ilevv) <= 0.) then
            !
            !-------------------------------------------------
            ! Pas de nuage.
            !-------------------------------------------------
            !
            ir=255 ; iv=255 ; ib=255 ! blanc.
          else
            !
            !-------------------------------------------------
            ! Nuage. Couleur liée à FLUX_MASSE_CV_UD, 
            ! flux de masse convectif sous-maille des updrafts.
            !-------------------------------------------------
            !
            zfrfm=max(0.,min(1.,(pfm(ific,ilevv)-zminfm)/(zmaxfm-zminfm)))
            call img_pal(clpal,'CONT','FRAC',zfrfm,irvb_loc)
            ir=irvb_loc(1)
            iv=irvb_loc(2)
            ib=irvb_loc(3)
          endif
        endif
        if(.true.) then
          !
          !-------------------------------------------------
          ! Précipitations.
          !-------------------------------------------------
          !
          zrrc=(ppcl(ific,ilevf)+ppcn(ific,ilevf))*86400.
          zrrs=(ppsl(ific,ilevf)+ppsn(ific,ilevf))*86400.
          !
          ! Probabilité d'apparition d'un point de pluie.
          !
          zmaxprob=0.66 ! la proba ne peut excéder zmaxprob.
          !zproba=max(0.,min(zmaxprob,((zrrc+zrrs)-0.2)/pprr*zmaxprob))
          zproba=max(0.,min(zmaxprob,((zrrc+zrrs)-2.4)/pprr*zmaxprob))
          zal=alea(zal)
          if(zal < zproba) then
            !
            ! Le point courant est un point de pluie.
            !
            !
            ! Probabilité que ce point soit de précipitation sous-maille.
            !
            zproba_c=zrrc/(zrrc+zrrs)
            zal=alea(zal)
            if(zal < zproba_c) then
              !
              ! Précipitation sous-maille.
              !
              ir=0 ; iv=155 ; ib=155
            else
              !
              ! Précipitation résolue.
              !
              if(llfm) then
                !
                ! Lumière grise inversée par rapport au fond.
                !
                ilum_fond=(ir+iv+ib)/3
                ir=max(0,min(255,255-ilum_fond))
                iv=max(0,min(255,255-ilum_fond))
                ib=max(0,min(255,255-ilum_fond))
              else
                !
                ! Rouge.
                !
                ir=200 ; iv=0 ; ib=0
              endif
            endif
          endif
        endif
      endif
    endif
    !
    !-------------------------------------------------
    ! On affecte ce RVB final dans le tableau.
    !-------------------------------------------------
    !
    if(irvb(1,jx,jy) == 255 .and. irvb(2,jx,jy) == 255 .and. irvb(3,jx,jy) == 255) then
      irvb(1,jx,jy)=ir
      irvb(2,jx,jy)=iv
      irvb(3,jx,jy)=ib
    endif
  enddo
enddo
!
!-------------------------------------------------
! Ajout d'une bordure noire.
!-------------------------------------------------
!
irvb_bord(:)=0
call img_bordure(irvb,kxcompo,iycoeur,irvb_bord)
!
!-------------------------------------------------
! Graduation de l'axe des Y: z (km).
!-------------------------------------------------
!
irvb_fond_texte(:)=0
zopac_fond_texte=0.
irvb_pp_texte(:)=0
zopac_pp_texte=1.
!
! Axe régulier.
!
ival=iycoeur-iyleg
!
! On ne va passer qu'une partie des tableaux (zval,zx,zy) à la routine
! effectuant la graduation car on veut se laisser de la place 
! pour afficher l'unité.
! On cherche donc ival2 tel que zy(ival2) = 52. pixels.
!
ival2=ival
do jval=1,ival
  if(zy(jval) < 52.) exit
  ival2=jval
enddo
call img_graduation(zval,ival2,zx,zy,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,1,kxcompo,iycoeur,irvb)
deallocate(zval)
deallocate(zx)
deallocate(zy)
!
! Légende.
!
irvb_fond_texte=0
zopac_fond_texte=0.
irvb_pp_texte=0
zopac_pp_texte=1.
iloc(1)=10 ; iloc(2)=ixleg/2 ; iloc(3)=40
cltexte='z (km)'
call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,1,iloc,kxcompo,iycoeur,irvb)
if(knfic > 1) then
  !
  !-------------------------------------------------
  ! Graduation de l'axe des X: le temps.
  !-------------------------------------------------
  !
  irvb_fond_texte(:)=0
  zopac_fond_texte=0.
  irvb_pp_texte(:)=0
  zopac_pp_texte=1.
  !
  ! Axe régulier des l'HSL.
  !
  ival=2
  allocate(zval(ival))
  allocate(zx(ival))
  allocate(zy(ival))
  zval(1)=phsl(1) ; zval(2)=phsl(knfic) ! valeurs extrêmes du champ à graduer.
  zx(1)=real(ixleg) ; zx(2)=real(kxcompo) ! X extrêmes du champ à graduer.
  zy(1)=real(iycoeur-iyleg/2) ; zy(2)=zy(1) ! Y extrêmes du champ à graduer.
  !
  ! On va réduire la valeur maxi des réels à coter en X
  ! car on veut se laisser de la place pour afficher l'unité.
  ! On se limite à une fraction donnée de l'étendue temporelle.
  !
  zpix=85 ! on se réserve zpix pixels à droite pour la légende.
  zfrac=1.-zpix/real(kxcompo-ixleg)
  zval(2)=zval(1)+(zval(2)-zval(1))*zfrac
  zx(2)=zx(1)+(zx(2)-zx(1))*zfrac
  call img_graduation(zval,ival,zx,zy,irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,1,kxcompo,iycoeur,irvb)
  !
  ! Légende.
  !
  irvb_fond_texte=0
  zopac_fond_texte=0.
  irvb_pp_texte=0
  zopac_pp_texte=1.
  iloc(1)=10 ; iloc(2)=kxcompo-15 ; iloc(3)=iycoeur-iyleg/2
  iloc(1)=2
  cltexte=cdlegx
  call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,1,iloc,kxcompo,iycoeur,irvb)
endif
!
!-------------------------------------------------
! Ecriture de l'indice expérience.
!-------------------------------------------------
!
irvb_fond_texte=0
zopac_fond_texte=0.
irvb_pp_texte=0
zopac_pp_texte=1.
iloc(1)=10 ; iloc(2)=ixleg/2 ; iloc(3)=20
if(len_trim(cdindice) <= 60) then
  cltexte=trim(cdindice)
  call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
  &,irvb_pp_texte,zopac_pp_texte,1,iloc,kxcompo,iycoeur,irvb)
endif
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
allocate(irvb_final(3,kxcompo,kycompo))
do jc=1,3
  do jx=1,kxcompo
    do jy=1,kycompo
      if(jy <= iycoeur) then
        irvb_final(jc,jx,jy)=irvb(jc,jx,jy)
      else
        irvb_final(jc,jx,jy)=irvb_leg(jc,jx,jy-iycoeur)
      endif
    enddo
  enddo
enddo
print*,'  Write out composite image file : ',trim(cdfcompo)
call img_ecr(cdfcompo,kxcompo,kycompo,irvb_final)
end
