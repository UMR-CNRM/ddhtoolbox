program imgcoul
! --------------------------------------------------------------
! Changement de palette de couleur d'une image.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-09, J.M. Piriou.
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
implicit character*200 (c)
implicit logical (l)
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
INTEGER,ALLOCATABLE :: ifrac(:,:)
INTEGER :: IARG,igris
INTEGER :: IARGC,irvb_loc(3)
INTEGER :: INX,imin,imax
INTEGER :: INX1
INTEGER :: INY,jarg,iarg1,iarg2
INTEGER :: INY1
INTEGER :: IPOSX1
INTEGER :: IPOSX2
INTEGER :: IPOSY1
INTEGER :: IPOSY2,iopt
INTEGER :: JCOUL,ix,iy
INTEGER :: JX,ix_lit,iy_lit,ix_ecr,iy_ecr,ioctet
INTEGER :: JY,joctet,inpts,irvb_discret(0:255,3)
REAL :: ZRAPA,znorm,zsom,zsom2,zect,zmoy,zfrac
REAL :: zfreq(-3:258),zrep(0:255),zratio
INTEGER :: ifiltre(0:255)
logical llcroiss,llfreqs
integer irvb_fond_texte(3)
integer irvb_pp_texte(3)
integer iloc(3)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
clpal='VIOLET-BLANC'
llcroiss=.true.
llfreqs=.false.
llfreqe=.false.
clfreq='freq.dta'
clrep='rep.dta'
llnorm=.false.
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg == 0) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Changement de palette de couleur d''une image, par égalisation d''histogramme.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgcoul [-norm] [-heq] [-pPALETTE] [-r] [-freqs] [-freqe] IMG_ENTREE IMG_SORTIE'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'avec'
  write(*,'(9a)') '	-norm si on veut de la normalisation d''histogramme.'
  write(*,'(9a)') '	-heq si on veut l''égalisation d''histogramme.'
  write(*,'(9a)') '		défaut: égalisation d''histogramme.'
  write(*,'(9a)') '	PALETTE=GRIS, REF, CONTRASTE, AQUABLUE, etc...'
  write(*,'(9a)') '		défaut: ',trim(clpal)
  write(*,'(9a)') '	-r si on souhaite une palette inversée, i.e. avec couleurs chaudes associées aux faibles valeurs.'
  write(*,'(9a)') '	-freqs si on souhaite obtenir en sortie les courbes de fréquence / répartition de (R+V+B).'
  write(*,'(9a)') '	      La fréquence est alors sortie sur le fichier ',trim(clfreq),'.'
  write(*,'(9a)') '	      La répartition est alors sortie sur le fichier ',trim(clrep),'.'
  write(*,'(9a)') '	-freqe si on souhaite lire en entrée une cbe de répartition de (R+V+B).'
  write(*,'(9a)') '	      La répartition est alors lue sur le fichier ',trim(clrep),'.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'En entrée l''image est lue en niveau de gris (R+V+B),'
  write(*,'(9a)') 'puis cette somme (R+V+B) est traduite en une palette colorée,'
  write(*,'(9a)') 'les transitions entre classes de couleur se faisant'
  write(*,'(9a)') 'en fonction de la courbe de répartition du champ (R+V+B) d''entrée'
  write(*,'(9a)') 'purgé au préalable de ses diracs (bordures).'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgcoul:'
!
!-------------------------------------------------
! Saisie de la palette, si elle a été fournie par l'utilisateur.
!-------------------------------------------------
!
do jarg=1,iarg
  call getargp(jarg,clarg)
  if(clarg(1:2) == '-p') then
    !
    !-------------------------------------------------
    ! L'utilisateur fournit la palette.
    !-------------------------------------------------
    !
    clpal=clarg(3:)
  elseif(clarg(1:2) == '-r') then
    !
    !-------------------------------------------------
    ! L'utilisateur veut une palette inversée.
    !-------------------------------------------------
    !
    llcroiss=.false.
  elseif(trim(clarg) == '-freqs') then
    !
    !-------------------------------------------------
    ! L'utilisateur veut la courbe de fréquence.
    !-------------------------------------------------
    !
    llfreqs=.true.
  elseif(trim(clarg) == '-freqe') then
    !
    !-------------------------------------------------
    ! L'utilisateur veut la courbe de fréquence.
    !-------------------------------------------------
    !
    llfreqe=.true.
  elseif(trim(clarg) == '-norm') then
    llnorm=.true.
  elseif(trim(clarg) == '-heq') then
    llnorm=.false.
  elseif(jarg == iarg-1) then
    climg1=clarg
  elseif(jarg == iarg) then
    climg2=clarg
  else
    print*,'imgcoul/ERREUR: arguments erronés!...'
    stop 'call abort'
  endif
enddo
!
!-------------------------------------------------
! Arguments OK.
!-------------------------------------------------
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
print*,'	image ',trim(climg1)
print*,'		taille (',inx1,',',iny1,')'
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb2(3,inx1,iny1))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
allocate(ifrac(inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
call img_pal_init
!
!-------------------------------------------------
! Calcul de min, max, moy et ect.
!-------------------------------------------------
!
print*,'		étude du champ (R+V+B) brut'
zsom=0.
zsom2=0.
imin=irvb1(1,1,1)
imax=irvb1(1,1,1)
zfreq=0.
inpts=0
ifiltre=0
do jy=1,iny1
  do jx=1,inx1
    ifrac(jx,jy)=(irvb1(1,jx,jy)+irvb1(2,jx,jy)+irvb1(3,jx,jy))/3
    ifiltre(ifrac(jx,jy))=1
    imin=min(imin,ifrac(jx,jy))
    imax=max(imax,ifrac(jx,jy))
    zsom=zsom+real(ifrac(jx,jy))
    zsom2=zsom2+real(ifrac(jx,jy)*ifrac(jx,jy))
    zfreq(ifrac(jx,jy))=zfreq(ifrac(jx,jy))+1
    inpts=inpts+1
  enddo
enddo
zmoy=zsom/real(inpts)
zect=sqrt(zsom2/real(inpts)-zmoy*zmoy)
if(llfreqs) then
  !
  !-------------------------------------------------
  ! Sortie de la courbe de fréquence de (R+V+B).
  !-------------------------------------------------
  !
  open(77,file=clfreq,form='formatted')
  do joctet=0,255
    write(77,*) joctet,zfreq(joctet)
  enddo
  close(77)
  print*,'Fichier-courbe de fréquence généré: ',trim(clfreq)
endif
!
!-------------------------------------------------
! Affichage de ces stats.
!-------------------------------------------------
!
print*,'			min=',imin
print*,'			max=',imax
print*,'			moy=',zmoy
print*,'			ect=',zect
!
!-------------------------------------------------
! On va filtrer les diracs.
! Création du filtre.
!-------------------------------------------------
!
print*,'		diracs'
zratio=3.
do joctet=0,255
  if(zfreq(joctet) &
  &/(max( 1. ,0.1667*(zfreq(joctet-3)+zfreq(joctet-2)+zfreq(joctet-1)+zfreq(joctet+1)+zfreq(joctet+2)+zfreq(joctet+3)))) &
  & > zratio) then
    !
    !-------------------------------------------------
    ! La fréquence du point courant est plus de ratio fois
    ! plus grande que la moyenne de ses 6 voisins.
    ! C'est un dirac. On le filtre.
    !-------------------------------------------------
    !
    ifiltre(joctet)=0
    print*,'			dirac en (R+V+B) = ',joctet
  endif
enddo
!
!-------------------------------------------------
! Si tous les points sont des diracs, on ne les filtre pas!...
!-------------------------------------------------
!
if(maxval(ifiltre) == 0) then
  !
  !-------------------------------------------------
  ! Tous les points sont des diracs ==> pas de filtrage.
  !-------------------------------------------------
  !
  ifiltre=1
endif
!
!-------------------------------------------------
! Calcul de min, max, moy et ect du champ filtré.
!-------------------------------------------------
!
print*,'		étude du champ (R+V+B) filtré (hors diracs)'
zsom=0.
zsom2=0.
imin=800
imax=-800
zfreq=0.
inpts=0
do jy=1,iny1
  do jx=1,inx1
    if(ifiltre(ifrac(jx,jy)) == 1) then
      imin=min(imin,ifrac(jx,jy))
      imax=max(imax,ifrac(jx,jy))
      zsom=zsom+real(ifrac(jx,jy))
      zsom2=zsom2+real(ifrac(jx,jy)*ifrac(jx,jy))
      zfreq(ifrac(jx,jy))=zfreq(ifrac(jx,jy))+1.
      inpts=inpts+1
    endif
  enddo
enddo
zmoy=zsom/real(inpts)
zect=sqrt(zsom2/real(inpts)-zmoy*zmoy)
if(llfreqe) then
  !
  !-------------------------------------------------
  ! Lecture de la courbe de répartition de (R+V+B).
  !-------------------------------------------------
  !
  open(77,file=clrep,form='formatted')
  do joctet=0,255
    read(77,*) ioctetgol,zrep(joctet)
  enddo
  close(77)
  print*,'Fichier-courbe de répartition lu: ',trim(clrep)
else
  !
  !-------------------------------------------------
  ! Calcul de la cbe de répartition à partir de celle de fréquence.
  !-------------------------------------------------
  !
  znorm=1./real(inpts)
  zrep(0)=zfreq(0)*znorm
  do joctet=1,255
    zrep(joctet)=zrep(joctet-1)+zfreq(joctet)*znorm
  enddo
endif
if(llfreqs) then
  !
  !-------------------------------------------------
  ! Sortie de la courbe de répartition de (R+V+B).
  !-------------------------------------------------
  !
  open(77,file=clrep,form='formatted')
  do joctet=0,255
    write(77,*) joctet,zrep(joctet)
  enddo
  close(77)
  print*,'Fichier-courbe de répartition généré: ',trim(clrep)
endif
!
!-------------------------------------------------
! Seuils de normalisation.
!-------------------------------------------------
!
if(llnorm) then
  znile=0.04 ! dans le cas de la normalisation d'histogramme, les seuils sont les probabilité znile et (1.-znile).
  imin_nile=-9
  imax_nile=-9
  do joctet=1,255
    if(zrep(joctet) > znile .and. imin_nile == -9) imin_nile=joctet
    if(zrep(joctet) > 1.-znile .and. imax_nile == -9) imax_nile=joctet
  enddo
endif
!
!-------------------------------------------------
! Affichage de ces stats.
!-------------------------------------------------
!
print*,'			min=',imin
print*,'			max=',imax
print*,'			moy=',zmoy
print*,'			ect=',zect
if(llnorm) then
  iseuil1=nint(100.*znile)
  print*,'			',iseuil1,'% des valeurs (R+V+B)/3 sont < à ',imin_nile
  print*,'			',iseuil1,'% des valeurs (R+V+B)/3 sont > à ',imax_nile
endif
!
!-------------------------------------------------
! Obtention de la palette pour les 256 niveaux de gris d'entrée.
!-------------------------------------------------
!
do joctet=0,255
  if(.not.llcroiss) zfrac=1.-zfrac
  if(trim(clpal) == 'GRIS') then
    if(.not.llcroiss) then
      irvb_loc=255-joctet
    else
      irvb_loc=joctet
    endif
  else
    zfrac=real(joctet)/255.
    if(.not.llcroiss) zfrac=1.-zfrac
    call img_pal(clpal,'CONT','FRAC',zfrac,irvb_loc)
  endif
  do jcoul=1,3
    irvb_discret(joctet,jcoul)=irvb_loc(jcoul)
  enddo
enddo
!
!-------------------------------------------------
! Changement de palette.
!-------------------------------------------------
!
print*,'		changement vers la palette ',trim(clpal)
if(llnorm) then
  !
  !-------------------------------------------------
  ! Normalisation.
  !-------------------------------------------------
  !
  do jy=1,iny1
    do jx=1,inx1
      ioctet=max(0,min(255,int(256.*real(ifrac(jx,jy)-imin_nile)/real(imax_nile-imin_nile))))
      do jcoul=1,3
        irvb2(jcoul,jx,jy)=irvb_discret(ioctet,jcoul)
      enddo
    enddo
  enddo
else
  !
  !-------------------------------------------------
  ! Egalisation.
  !-------------------------------------------------
  !
  do jy=1,iny1
    do jx=1,inx1
      ioctet=max(0,min(255,nint(255.*real(zrep(ifrac(jx,jy))))))
      do jcoul=1,3
        irvb2(jcoul,jx,jy)=irvb_discret(ioctet,jcoul)
      enddo
    enddo
  enddo
endif
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',trim(climg2)
print*,' '
!
!-------------------------------------------------
! Désallocation des tableaux.
!-------------------------------------------------
!
deallocate(irvb1)
deallocate(irvb2)
deallocate(ifrac)
end
