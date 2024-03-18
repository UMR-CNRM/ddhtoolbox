subroutine genpcm(pclas,kclas,kulout)
! --------------------------------------------------------------
! **** *genpcm* Génération de palette de couleur metview.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2005-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
real pclas(kclas)
integer irvb(3,kclas+1)
!
!-------------------------------------------------
! Détermination de l'écart minimal entre deux classes.
!-------------------------------------------------
!
zec=abs(pclas(2)-pclas(1))
do jclas=3,kclas
  zec=min(zec,abs(pclas(jclas)-pclas(jclas-1)))
enddo
do jclas=1,kclas
  if(pclas(jclas) /= 0.) zec=min(zec,abs(pclas(jclas)))
enddo
!
!-------------------------------------------------
! Lecture séquentielle: on détermine le nombre
! de valeurs positives ou négatives.
!-------------------------------------------------
!
ll0=.false.
iclasneg=0
iclaspos=0
do jclas=1,kclas
  if(pclas(jclas) >= 0.) then
    iclaspos=iclaspos+1
  else
    iclasneg=iclasneg+1
  endif
  if(pclas(jclas) == 0.) ll0=.true.
enddo
!
!-------------------------------------------------
! Couleurs metview.
!-------------------------------------------------
!
if(pclas(1) < pclas(kclas)) then
  !
  !-------------------------------------------------
  ! Cas de classes croissantes.
  !-------------------------------------------------
  !
  call img_pal_init
  if(pclas(1)*pclas(kclas) < 0.) then
    !
    !-------------------------------------------------
    ! Cas de classes entourant 0.
    !-------------------------------------------------
    !
    if(ll0) then
      !
      !-------------------------------------------------
      ! Le 0 est l'une des classes.
      !-------------------------------------------------
      !
      do jclas=1,iclasneg
        clpal='AQUABLUE'
        if(iclasneg-1 > 0) then
          zfy=real(jclas-1)/real(iclasneg-1)
        else
          zfy=1.
        endif
        call img_pal(clpal,'CONT','FRAC',zfy,irvb(1,jclas))
      enddo
      do jclas=iclasneg+1,kclas+1
        clpal='JAUNE-ROUGE-ROSE'
        if(kclas+1-(iclasneg+1) > 0) then
          zfy=real(jclas-(iclasneg+1))/real(kclas+1-(iclasneg+1))
        else
          zfy=1.
        endif
        call img_pal(clpal,'CONT','FRAC',zfy,irvb(1,jclas))
      enddo
    else
      !
      !-------------------------------------------------
      ! Le 0 n'est pas l'une des classes.
      !-------------------------------------------------
      !
      !
      ! Valeurs négatives.
      !
      do jclas=1,iclasneg-1
        clpal='AQUABLUE'
        if(iclasneg >= 1) then
          zfy=real(jclas)/real(iclasneg+1)
        else
          zfy=0.9
        endif
        call img_pal(clpal,'CONT','FRAC',zfy,irvb(1,jclas))
      enddo
      !
      ! Valeurs voisines de 0.
      !
      irvb(1,iclasneg)=000 ; irvb(2,iclasneg)=255 ; irvb(3,iclasneg)=000
      !
      ! Valeurs positives.
      !
      do jclas=iclasneg+1,kclas+1
        if(kclas+1-iclasneg-1 > 0) then
          clpal='JAUNE-ROUGE-ROSE'
          if(kclas+1-(iclasneg+1) > 0) then
            zfy=real(jclas-(iclasneg+1))/real(kclas+1-(iclasneg+1))
          else
            zfy=1.
          endif
          call img_pal(clpal,'CONT','FRAC',zfy,irvb(1,jclas))
        else
          !
          ! Rose.
          !
          irvb(1,jclas)=255 ; irvb(2,jclas)=000 ; irvb(3,jclas)=255
        endif
      enddo
    endif
  else
    !
    !-------------------------------------------------
    ! Cas de classes n'entourant pas 0.
    !-------------------------------------------------
    !
    do jclas=1,kclas+1
      clpal='CONTRASTE'
      if(kclas+1-1 > 0) then
        zfy=real(jclas-1)/real(kclas+1-1)
      else
        zfy=1.
      endif
      call img_pal(clpal,'CONT','FRAC',zfy,irvb(1,jclas))
    enddo
  endif
else
  !
  !-------------------------------------------------
  ! Cas de classes décroissantes.
  !-------------------------------------------------
  !
  print*,'genpcm/ERREUR: le cas des classes décroissantes n''est pas prévu!...'
  print*,pclas(1),pclas(kclas),kclas
  stop 'call abort'
endif
do jclas=1,kclas
  write(clrvb,fmt='(a,g16.7,a,3(f5.3,a))') '#ISO=',pclas(jclas) &
  & ,'=',real(irvb(1,jclas))/255. &
  & ,'=',real(irvb(2,jclas))/255. &
  & ,'=',real(irvb(3,jclas))/255.
  call oteblancs(clrvb,clrvb2,ilsor)
  write(kulout,fmt='(a)') trim(clrvb2)
enddo
!
!-------------------------------------------------
! Nombre de décimales sur lesquelles vont être écrites
! les isolignes.
!-------------------------------------------------
!
zlog=log(abs(zec))/log(10.) ! log10 de zec.
idec=nint(zlog-0.5) ! partie entière de ce log10.
zratio=zec/10.**float(idec) ! mantisse de zec.
if(zratio < 1.) then
  idec=idec-1
elseif(zratio >= 10.) then
  idec=idec+1
endif
idec=max(0,-idec+1)
if(zec == 0.) idec=2 ! dans le cas où les min et max n'ont pas été fournis, on met par défaut deux chiffres après la virgule.
!print*,'  Isolignes cotées sur ',idec,' décimales.'
end
