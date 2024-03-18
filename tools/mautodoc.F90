subroutine indice_en_clair(cdnomf,kule,cdnomc)
! --------------------------------------------------------------
! **** ** Nom en clair associé à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdnomf: nom du fichier 1D.
!	kule: unité logique du fichier 1D. Ce fichier doit être ouvert au préalable.
! En sortie:
!	cdnom: nom en clair; c'est l'indice expérience s'il existe dans le fichier.
!		Sinon, le nom du fichier est retourné.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
character*(*) cdnomc
!
!-------------------------------------------------
! Nom en clair.
!-------------------------------------------------
!
call lfacas(kule,'INDICE EXPERIENCE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecc(kule,'INDICE EXPERIENCE',1,cdnomc,ilong,ierr)
else
  cdnomc=cdnomf
endif
end
subroutine latitude(kule,plat)
! --------------------------------------------------------------
! **** ** Latitude associée à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	cdnomf: nom du fichier 1D.
!	kule: unité logique du fichier 1D. Ce fichier doit être ouvert au préalable.
! En sortie:
!	plat: latitude (rad).
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
real(kind=8) :: ztab(1)
!
!-------------------------------------------------
! Nom en clair.
!-------------------------------------------------
!
call lfacas(kule,'LATITUDE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kule,'LATITUDE',1,ztab,ilong,ierr)
  plat=ztab(1)
else
  call lfacas(kule,'PGEMU',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'PGEMU',1,ztab,ilong,ierr)
    plat=asin(ztab(1))
  else
    write(*,fmt=*) 'mautodoc/ERROR: neither LATITUDE nor PGEMU in the file!...'
    stop 'call abort'
  endif
endif
end
subroutine rs_indice_nom(kindice,cdtexte)
! --------------------------------------------------------------
! Recherche du nom de la station de RS associée à un indicatif OMM donné.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kindice: indice OMM de la station.
! En sortie:
!	nom en clair de la station.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
cdtexte=' '
!
!-------------------------------------------------
! Nom du fichier-liste des stations RS.
!-------------------------------------------------
!
call getenv('RS',clnf)
inquire(file=clnf,exist=llexist)
if(llexist) then
  !
  !-------------------------------------------------
  ! Le fichier existe.
  !-------------------------------------------------
  !
  open(87,file=clnf,form='formatted')
  inomal=0
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  do
    read(87,fmt='(a)',iostat=ios) clc
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
      read(clc(1:5),fmt=*) iindice
      if(iindice == kindice) clcn=clc
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      print*,'Code réponse en lecture non prévu: ',ios
      stop 'call abort'
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(87)
  !
  !-------------------------------------------------
  ! Nom de la station.
  !-------------------------------------------------
  !
  clnom=clcn(10:60)
  clpays=clcn(61:111)
  cltexte=clnom(1:len_trim(clnom))//' ('//clpays(1:len_trim(clpays))//')'
  !
  !-------------------------------------------------
  ! On ôte les doubles blancs.
  !-------------------------------------------------
  !
  call otedbl(cltexte,clinterm)
  cdtexte=clinterm
endif
end
subroutine rs_proche(plon,plat,cdtexte)
! --------------------------------------------------------------
! Recherche de la station de RS la plus proche du point en entrée.
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
!	plon (degrés)
!	plat (degrés)
! En sortie:
!	nom en clair de la station
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
character*2 cldir(0:7)
data cldir/'E ','NE','N ','NW','W ','SW','S ','SE'/
cdtexte=' '
zcondr=atan(1.)/45.
!
!-------------------------------------------------
! Nom du fichier-liste des stations RS.
!-------------------------------------------------
!
call getenv('RS',clnf)
inquire(file=clnf,exist=llexist)
if(llexist) then
  !
  !-------------------------------------------------
  ! Le fichier existe.
  !-------------------------------------------------
  !
  open(87,file=clnf,form='formatted')
  inomal=0
  zx1=cos(plat*zcondr)*cos(plon*zcondr)
  zy1=cos(plat*zcondr)*sin(plon*zcondr)
  zz1=sin(plat*zcondr)
  zdistn=-1000.
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  do
    read(87,fmt='(a)',iostat=ios) clc
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
      read(clc(112:117),fmt=*) zlat
      read(clc(118:125),fmt=*) zlon
      zcos2=cos(zlat*zcondr)
      zsin2=sin(zlon*zcondr)
      zdist=zx1*zcos2*cos(zlon*zcondr) &
      & + zy1*zcos2*zsin2 &
      & + zz1*sin(zlat*zcondr)
      if(zdist > zdistn) then
        zdistn=zdist
        clcn=clc
      endif
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      print*,'Code réponse en lecture non prévu: ',ios
      stop 'call abort'
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(87)
  !
  !-------------------------------------------------
  ! Nom de la station la plus proche.
  !-------------------------------------------------
  !
  clnom=clcn(10:60)
  clpays=clcn(61:111)
  cdtexte=clnom(1:len_trim(clnom))//' ('//clpays(1:len_trim(clpays))//')'
  read(clcn(112:117),fmt=*) zlat
  read(clcn(118:125),fmt=*) zlon
  !
  !-------------------------------------------------
  ! Distance orthodromique.
  !-------------------------------------------------
  !
  zdistn=max(-1.,min(1.,zdistn))
  zdist=acos(zdistn)/4./atan(1.)*20000.
  !
  !-------------------------------------------------
  ! Azimuth du point depuis la station de RS.
  !-------------------------------------------------
  !
  zxdir=(plon-zlon)*cos(0.5*(plat+zlat)*zcondr)
  zydir=plat-zlat
  call recpol(zxdir,zydir,zrayon,zang)
  iang=modulo(nint(zang/atan(1.)),8) ! angle exprimé en 8èmes de tour, entre 0 et 7.
  if(iang < 0) iang=iang+8
  !
  !-------------------------------------------------
  ! Chaîne texte finale.
  !-------------------------------------------------
  !
  if(zdist < 1.) then
    !
    !-------------------------------------------------
    ! On est à moins d'un km de la station de RS.
    !-------------------------------------------------
    !
    write(cltexte,fmt='(9a)') 'at ',cdtexte(1:len_trim(cdtexte))
  else
    write(cltexte,fmt='(f6.0,9a)') zdist,' km ',cldir(iang),' from ',cdtexte(1:len_trim(cdtexte))
  endif
  !
  !-------------------------------------------------
  ! On ôte les doubles blancs.
  !-------------------------------------------------
  !
  call otedbl(cltexte,clinterm)
  cdtexte=clinterm
endif
end
subroutine lon_lat_nom(plon,plat,cdtexte)
! --------------------------------------------------------------
! Recherche du nom en clair associé à un (lon,lat) donné.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2004-08, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	plon (degrés)
!	plat (degrés)
! En sortie:
!	cdtexte: nom en clair de la station
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
character*2 cldir(0:7)
data cldir/'E ','NE','N ','NW','W ','SW','S ','SE'/
cdtexte=' '
zcondr=atan(1.)/45.
!
!-------------------------------------------------
! Nom du fichier-liste des noms en clair.
!-------------------------------------------------
!
call getenv('LON_LAT_NOM',clnf)
inquire(file=clnf,exist=llexist)
if(llexist) then
  !
  !-------------------------------------------------
  ! Le fichier existe.
  !-------------------------------------------------
  !
  open(87,file=clnf,form='formatted')
  inomal=0
  zx1=cos(plat*zcondr)*cos(plon*zcondr)
  zy1=cos(plat*zcondr)*sin(plon*zcondr)
  zz1=sin(plat*zcondr)
  zdistn=-1000.
  !
  !-------------------------------------------------
  ! Lecture séquentielle.
  !-------------------------------------------------
  !
  do
    read(87,fmt='(a)',iostat=ios) clc
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
      read(clc(001:009),fmt=*) zlon
      read(clc(010:018),fmt=*) zlat
      zcos2=cos(zlat*zcondr)
      zsin2=sin(zlon*zcondr)
      zdist=zx1*zcos2*cos(zlon*zcondr) &
      & + zy1*zcos2*zsin2 &
      & + zz1*sin(zlat*zcondr)
      if(zdist > zdistn) then
        zdistn=zdist
        clcn=clc(20:)
        zlonn=zlon
        zlatn=zlat
      endif
    else
      !
      !-------------------------------------------------
      ! Cas non prévu.
      !-------------------------------------------------
      !
      print*,'Code réponse en lecture non prévu: ',ios
      stop 'call abort'
    endif
  enddo
  !
  !-------------------------------------------------
  ! Fermeture du fichier d'entrée.
  !-------------------------------------------------
  !
  close(87)
  !
  !-------------------------------------------------
  ! Nom de la station la plus proche.
  !-------------------------------------------------
  !
  clnom=clcn
  cdtexte=clnom
  zlon=zlonn
  zlat=zlatn
  !
  !-------------------------------------------------
  ! Distance orthodromique.
  !-------------------------------------------------
  !
  zdistn=max(-1.,min(1.,zdistn))
  zdist=acos(zdistn)/4./atan(1.)*20000.
  !
  !-------------------------------------------------
  ! Azimuth du point depuis le lieu le plus proche trouvé.
  !-------------------------------------------------
  !
  zxdir=(plon-zlon)*cos(0.5*(plat+zlat)*zcondr)
  zydir=plat-zlat
  call recpol(zxdir,zydir,zrayon,zang)
  iang=modulo(nint(zang/atan(1.)),8) ! angle exprimé en 8èmes de tour, entre 0 et 7.
  if(iang < 0) iang=iang+8
  !
  !-------------------------------------------------
  ! Chaîne texte finale.
  !-------------------------------------------------
  !
  if(zdist < 1.) then
    !
    !-------------------------------------------------
    ! On est à moins d'un km de la station de RS.
    !-------------------------------------------------
    !
    write(cltexte,fmt='(9a)') 'at ',trim(cdtexte)
  else
    write(cltexte,fmt='(f6.0,9a)') zdist,' km ',cldir(iang),' from ',trim(cdtexte)
  endif
  !
  !-------------------------------------------------
  ! On ôte les doubles blancs.
  !-------------------------------------------------
  !
  call otedbl(cltexte,clinterm)
  cdtexte=clinterm
endif
end
subroutine localisation_en_clair(kule,cdloc)
! --------------------------------------------------------------
! **** ** Localisation en clair associée à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kule: unité logique du fichier 1D. Ce fichier doit être ouvert au préalable.
! En sortie:
!	cdloc: localisation en clair.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
character*(*) cdloc
real(kind=8) :: ztab(1)
!
!-------------------------------------------------
! Valeurs par défaut.
!-------------------------------------------------
!
zindef=8.25621e12
!
!-------------------------------------------------
! Latitude.
!-------------------------------------------------
!
call lfacas(kule,'LATITUDE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kule,'LATITUDE',1,ztab,ilong,ierr)
  zval=ztab(1)
  zpi=4.*atan(1.)
  zlat=180./zpi*zval
else
  call lfacas(kule,'PGEMU',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'PGEMU',1,ztab,ilong,ierr)
    zval=ztab(1)
    zlat=45./atan(1.)*asin(zval)
  else
    zlat=zindef
  endif
endif
!
!-------------------------------------------------
! Longitude.
!-------------------------------------------------
!
call lfacas(kule,'LONGITUDE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kule,'LONGITUDE',1,ztab,ilong,ierr)
  zval=ztab(1)
  zlon=45./atan(1.)*zval
  if(zlon > 180.) zlon=zlon-360.
else
  call lfacas(kule,'PGELAM',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'PGELAM',1,ztab,ilong,ierr)
    zval=ztab(1)
    zlon=45./atan(1.)*zval
    if(zlon > 180.) zlon=zlon-360.
  else
    zlon=zindef
  endif
endif
!
!-------------------------------------------------
! Lat-lon.
!-------------------------------------------------
!
if(zlat /= zindef) then
  !
  !-------------------------------------------------
  ! La latitude est disponible.
  !-------------------------------------------------
  !
  write(clorig,fmt='(2(a,f10.5))') 'lon=',zlon,' lat=',zlat
  !
  !-------------------------------------------------
  ! Si le fichier des lieux de RS est disponible,
  ! on cherche la station de RS la plus proche.
  !-------------------------------------------------
  !
  call lon_lat_nom(zlon,zlat,cllon_lat_nom)
  if(cllon_lat_nom /= ' ') then
    clorig=clorig(1:len_trim(clorig))//', '//cllon_lat_nom(1:len_trim(cllon_lat_nom))
  endif
else
  !
  !-------------------------------------------------
  ! La latitude est indisponible.
  !-------------------------------------------------
  !
  clorig=' '
endif
call otedbl(clorig,cdloc)
end
subroutine date_en_clair(kule,cddatc)
! --------------------------------------------------------------
! **** ** Date en clair associée à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kule: unité logique du fichier 1D. Ce fichier doit être ouvert au préalable.
! En sortie:
!	cddatc: date en clair.
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
character*(*) cddatc
real(kind=8) :: ztab(1)
!
!-------------------------------------------------
! Valeurs par défaut.
!-------------------------------------------------
!
zindef=8.1254785
!
!-------------------------------------------------
! Dates.
!-------------------------------------------------
!
call lfacas(kule,'KINDAT',cltype,ilong,ierr)
if(ierr == 0) then
  call lfaleci(kule,'KINDAT',1,kindat,ilong,ierr)
else
  call lfacas(kule,'NINDAT',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfaleci(kule,'NINDAT',1,kindat,ilong,ierr)
  else
    print*,'ms/ERROR: neither KINDAT nor NINDAT articles!...'
    stop 'call abort'
  endif
endif
call lfacas(kule,'KSSSSS',cltype,ilong,ierr)
if(ierr == 0) then
  call lfaleci(kule,'KSSSSS',1,ksssss,ilong,ierr)
else
  call lfacas(kule,'NSSSSS',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfaleci(kule,'NSSSSS',1,ksssss,ilong,ierr)
  else
    print*,'ms/ERROR: neither KINDAT nor NINDAT articles!...'
    stop 'call abort'
  endif
endif
call lfalecr(kule,'RSTATI',1,ztab,ilong,ierr)
zstati=ztab(1)
iqu=mod(kindat,100)
idat=kindat/100
imo=mod(idat,100)
ian=idat/100
zsssss=real(ksssss)
!
!-------------------------------------------------
! Génération d'une date en clair.
!-------------------------------------------------
!
call date_plus_ech(ian,imo,iqu,zsssss,zstati,cltit)
!
!-------------------------------------------------
! On élimine les double blancs.
!-------------------------------------------------
!
call otedbl(cltit,cltmp)
cltit=cltmp
!
!-------------------------------------------------
! Longitude.
!-------------------------------------------------
!
call lfacas(kule,'LONGITUDE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kule,'LONGITUDE',1,ztab,ilong,ierr)
  zlon=45./atan(1.)*ztab(1)
  if(zlon > 180.) zlon=zlon-360.
else
  call lfacas(kule,'PGELAM',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'PGELAM',1,ztab,ilong,ierr)
    zval=ztab(1)
    zlon=45./atan(1.)*zval
    if(zlon > 180.) zlon=zlon-360.
  else
    zlon=zindef
  endif
endif
if(zlon /= zindef) then
  !
  !-------------------------------------------------
  ! On peut fournir l'heure solaire moyenne.
  !-------------------------------------------------
  !
  zutc=(real(ksssss)+zstati)/3600. ! heure UTC.
  zhsm=zutc+zlon/15. ! correction de longitude.
  zborne=24.
  zhsm=zborne*(zhsm/zborne-real(nint(zhsm/zborne-0.5)))
  ihhsm=int(zhsm)
  imhsm=nint((zhsm-real(ihhsm))*60.)
  write(clhsm,fmt='(i2.2,a,i2.2,a)') ihhsm,':',imhsm,' LST'
  cddatc=cltit(1:len_trim(cltit))//' ('//clhsm(1:len_trim(clhsm))//')'
else
  !
  !-------------------------------------------------
  ! On ne peut pas fournir l'heure solaire moyenne.
  !-------------------------------------------------
  !
  cddatc=cltit
endif
end
subroutine hsl(kule,phsl)
! --------------------------------------------------------------
! **** ** Heure solaire locale associée à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!	kule: unité logique du fichier 1D. Ce fichier doit être ouvert au préalable.
! En sortie:
!	phsl: heure solaire locale (h).
! --------------------------------------------------------------
!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character(len=200) (c)
implicit logical (l)
implicit real(kind=8) (z,p)
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Déclarations.
!-------------------------------------------------
!
real(kind=8) :: ztab(1)
!
!-------------------------------------------------
! Valeurs par défaut.
!-------------------------------------------------
!
zindef=8.1254785
!
!-------------------------------------------------
! Dates.
!-------------------------------------------------
!
call lfacas(kule,'KINDAT',cltype,ilong,ierr)
if(ierr == 0) then
  call lfaleci(kule,'KINDAT',1,kindat,ilong,ierr)
else
  call lfacas(kule,'NINDAT',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfaleci(kule,'NINDAT',1,kindat,ilong,ierr)
  else
    print*,'ms/ERROR: neither KINDAT nor NINDAT articles!...'
    stop 'call abort'
  endif
endif
call lfacas(kule,'KSSSSS',cltype,ilong,ierr)
if(ierr == 0) then
  call lfaleci(kule,'KSSSSS',1,ksssss,ilong,ierr)
else
  call lfacas(kule,'NSSSSS',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfaleci(kule,'NSSSSS',1,ksssss,ilong,ierr)
  else
    print*,'ms/ERROR: neither KINDAT nor NINDAT articles!...'
    stop 'call abort'
  endif
endif
call lfalecr(kule,'RSTATI',1,ztab,ilong,ierr)
zstati=ztab(1)
iqu=mod(kindat,100)
idat=kindat/100
imo=mod(idat,100)
ian=idat/100
zsssss=real(ksssss)
!
!-------------------------------------------------
! Génération d'une date en clair.
!-------------------------------------------------
!
call date_plus_ech(ian,imo,iqu,zsssss,zstati,cltit)
!
!-------------------------------------------------
! On élimine les double blancs.
!-------------------------------------------------
!
call otedbl(cltit,cltmp)
cltit=cltmp
!
!-------------------------------------------------
! Longitude.
!-------------------------------------------------
!
call lfacas(kule,'LONGITUDE',cltype,ilong,ierr)
if(ierr == 0) then
  call lfalecr(kule,'LONGITUDE',1,ztab,ilong,ierr)
  zval=ztab(1)
  zlon=45./atan(1.)*zval
  if(zlon > 180.) zlon=zlon-360.
else
  call lfacas(kule,'PGELAM',cltype,ilong,ierr)
  if(ierr == 0) then
    call lfalecr(kule,'PGELAM',1,ztab,ilong,ierr)
    zval=ztab(1)
    zlon=45./atan(1.)*zval
    if(zlon > 180.) zlon=zlon-360.
  else
    zlon=zindef
  endif
endif
if(zlon /= zindef) then
  !
  !-------------------------------------------------
  ! On peut fournir l'heure solaire moyenne.
  !-------------------------------------------------
  !
  zutc=(real(ksssss)+zstati)/3600. ! heure UTC.
  zhsm=zutc+zlon/15. ! correction de longitude.
  zborne=24.
  phsl=zborne*(zhsm/zborne-real(nint(zhsm/zborne-0.5)))
else
  !
  !-------------------------------------------------
  ! On ne peut pas fournir l'heure solaire moyenne.
  !-------------------------------------------------
  !
  zutc=(real(ksssss)+zstati)/3600. ! heure UTC.
  phsl=zutc
endif
end
subroutine niveaux(kule,klev)
! --------------------------------------------------------------
! **** ** Nombre de niveaux associé à un fichier LFA issu du modèle 1D.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2002-06, J.M. Piriou.
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
implicit integer(kind=4) (i,k)
!
!-------------------------------------------------
! Nom en clair.
!-------------------------------------------------
!
call lfacas(kule,'KLEV',cltype,ilong,ierr)
if(ierr == 0) then
  call lfaleci(kule,'KLEV',1,klev,ilong,ierr)
else
  call lfacas(kule,'PTT0',cltype,ilong,ierr)
  if(ierr == 0) then
    klev=ilong
  else
    write(*,fmt=*) 'mautodoc/ERROR: neither KLEV nor PTT0 articles in the file!...'
    write(*,fmt=*) 
    stop 'call abort'
  endif
endif
end
#include"recpol.F90"
