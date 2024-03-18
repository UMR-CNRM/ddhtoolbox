program imgana
! --------------------------------------------------------------
! Anamorphose d'une image par sur ou sous-échantillonnage.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2000-01, J.M. Piriou.
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
INTEGER,ALLOCATABLE :: irvb_lit(:,:,:)
INTEGER,ALLOCATABLE :: irvb_ecr(:,:,:)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg < 3 .or. iarg > 4) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Anamorphose d''une image par sur ou sous-échantillonnage.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: imgana IMG1 [-xNX] [-yNY] IMGSORTIE'
  write(*,'(a)') ' '
  write(*,'(a)') 'où NX ou NY sont les nombres de points en X et Y désirés en sortie.'
  write(*,'(a)') 'Si on spécifie un seul des deux nombres en X ou Y, le rapport d''aspect est conservé.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Exemple: imgana entree.ppm -y600 sortie.ppm'
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
clppm1=' '
clppm3=' '
inx2=0
iny2=0
do jarg=1,iarg
  call getargp(jarg,clarg)
  if(clarg(1:2) == '-x') then
    read(clarg(3:),fmt=*) inx2
  elseif(clarg(1:2) == '-y') then
    read(clarg(3:),fmt=*) iny2
  elseif(clppm1 == ' ') then
    clppm1=clarg
  elseif(clppm3 == ' ') then
    clppm3=clarg
  else
    write(*,fmt=*) 'imgana/ERREUR: arguments!...'
    write(*,fmt=*) trim(clarg)
    stop 'call abort'
  endif
enddo
print*,'imgana:'
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(clppm1,inx1,iny1)
print*,'	image d''entrée: (',inx1,',',iny1,')'
zrapa=real(iny1)/real(inx1)
!
!-------------------------------------------------
! Calcul du nb de points en X à partir de celui en Y
! ou l'inverse.
!-------------------------------------------------
!
if(iny2 == 0) then
  iny2=nint(real(inx2)*zrapa)
elseif(inx2 == 0) then
  inx2=nint(real(iny2)/zrapa)
endif
!
!-------------------------------------------------
! Allocation de l'image de sortie.
!-------------------------------------------------
!
allocate(irvb_ecr(3,inx2,iny2))
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'	lecture de l''image 1...'
allocate(irvb_lit(3,inx1,iny1))
call img_lec(clppm1,inx1,iny1,irvb_lit)
!
!-------------------------------------------------
! Echantillonnage de l'image 2.
!-------------------------------------------------
!
irvb_ecr=0
do jy=1,iny2
  do jx=1,inx2
    !
    !-------------------------------------------------
    ! X.
    !-------------------------------------------------
    !
    zx_gauche=real(jx-1)/real(inx2)
    zx_droite=real(jx  )/real(inx2)
    ix_lit_gauche=max(1,min(inx1,1+int(zx_gauche*real(inx1))))
    ix_lit_droite=max(1,min(inx1,1+int(zx_droite*real(inx1))))
    if(ix_lit_droite < ix_lit_gauche) then
      write(*,fmt=*) 
      write(*,fmt=*) 'imgana/ERREUR: ix_lit_droite < ix_lit_gauche!...'
      write(*,fmt=*) ix_lit_droite,ix_lit_gauche
      stop 'call abort'
    endif
    !
    !-------------------------------------------------
    ! Y.
    !-------------------------------------------------
    !
    zy_haut=real(jy-1)/real(iny2)
    zy_bas=real(jy  )/real(iny2)
    iy_lit_haut=max(1,min(iny1,1+int(zy_haut*real(iny1))))
    iy_lit_bas=max(1,min(iny1,1+int(zy_bas*real(iny1))))
    if(iy_lit_bas < iy_lit_haut) then
      write(*,fmt=*) 
      write(*,fmt=*) 'imgana/ERREUR: iy_lit_bas < iy_lit_haut!...'
      write(*,fmt=*) iy_lit_bas,iy_lit_haut
      stop 'call abort'
    endif
    !
    !-------------------------------------------------
    ! On additionne dans le tableau de sortie.
    !-------------------------------------------------
    !
    do jy1=iy_lit_haut,iy_lit_bas
      do jx1=ix_lit_gauche,ix_lit_droite
        do jcoul=1,3
          irvb_ecr(jcoul,jx,jy)=irvb_ecr(jcoul,jx,jy)+irvb_lit(jcoul,jx1,jy1)
        enddo
      enddo
    enddo
    !
    !-------------------------------------------------
    ! On divise par le nombre de points cumulés.
    !-------------------------------------------------
    !
    do jcoul=1,3
      irvb_ecr(jcoul,jx,jy)=max(0,min(255, &
      & nint(real(irvb_ecr(jcoul,jx,jy))/real(iy_lit_bas-iy_lit_haut+1) &
      & /real(ix_lit_droite-ix_lit_gauche+1))))
    enddo
  enddo
enddo
deallocate(irvb_lit)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'	écriture de l''image (',inx2,',',iny2,') résultante...'
call img_ecr(clppm3,inx2,iny2,irvb_ecr)
print*,'	fichier écrit: ',clppm3(1:len_trim(clppm3))
print*,' '
end
