program merid
implicit character *80 (c)
implicit logical (l)
!
! ideltm: ecartement des meridiens-paralleles en degres.
print*,'Ecartement des meridiens-paralleles en degres?'
read(*,*) ideltm
write(clfic,fmt='(''fdc_meridiens_'',i2.2)') ideltm
open(3,file=clfic,form='formatted')
!
! Fond de carte en sortie.
! Meridiens.
inppb=0
ilonn=-180
ilonx=180
! Paralleles.
if(ideltm == 20) then
  ilatn=-80
  ilatx=80
else
  ilatn=-90+ideltm
  ilatx=90-ideltm
endif
!
!-------------------------------------------------
! Tracé des méridiens.
!-------------------------------------------------
!
do jlon=ilonn,ilonx,ideltm
  call levp(zxout,zyout,inppb)
  do jlat=ilatn,ilatx
    zxout=float(jlon)
    zyout=float(jlat)
    call baip(zxout,zyout,inppb)
  enddo
enddo
zsaut=999.999
write(3,fmt='(2f9.3)') zsaut,zsaut
!
!-------------------------------------------------
! Tracé des parallèles.
!-------------------------------------------------
!
do jlat=ilatn,ilatx,ideltm
  call levp(zxout,zyout,inppb)
  do jlon=ilonn,ilonx
    zxout=float(jlon)
    zyout=float(jlat)
    call baip(zxout,zyout,inppb)
  enddo
enddo
write(3,fmt='(2f9.3)') zsaut,zsaut
end
subroutine levp(px,py,knppb)
! --------------------------------------------------------------------------
! **** *LEVP*  Lever de plume du trace d'une ligne brisee.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:           93-07, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree: px, py coordonnees du point precedemment trace.
! En entree/sortie: knppb en entree: 0 si la plume etait levee.
! sinon nombre de points ecrits
! depuis qu'elle etait baissee.
! knppb vaut toujours 0 en sortie.
!
ztmp=999.999
if(knppb.eq.0) then
  ! La plume etait deja levee: rien a faire de particulier!...
elseif(knppb.eq.1) then
  ! La plume etait baissee mais on n'avait ecrit qu'un point.
  ! Afin de na pas faire planter les logiciels qui tracent
  ! des segments de droite, on fournit le meme point que le
  ! precedent.
  write(3,fmt='(2F9.3)') px,py
  write(3,fmt='(2F9.3)') ztmp,ztmp
  knppb=0
else
  ! La plume etait baissee et on avait ecrit au moins 2 points.
  ! On leve la plume.
  knppb=0
  write(3,fmt='(2F9.3)') ztmp,ztmp
endif
return
end
subroutine baip(px,py,knppb)
! --------------------------------------------------------------------------
! **** *BAIP*  Baisser de plume du trace d'une ligne brisee.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! --------
! Externes: /
! ---------
! Auteur:           93-07, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree: px, py coordonnees du point a tracer.
! En entree/sortie: knppb nombre de points ecrits
! depuis qu'elle etait baissee.
! knppb est augmente de 1.
!
write(3,fmt='(2F9.3)') px,py
knppb=knppb+1
return
end
