program imgfreq
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
REAL :: zfreq(3,0:255)
REAL :: ztonm(3)
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
if(iarg /= 1) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Obtention de la courbe de fréquence des composantes RVB de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgfreq IMG'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Le fichier de sortie sera freq.C.tmp.dta, avec C=R, V ou B.'
  write(*,'(9a)') ' '
  stop
endif
call getargp(1,climg1)
print*,'imgfreq:'
!
!-------------------------------------------------
! Lecture de la taille du img d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
print*,'	image ',climg1(1:len_trim(climg1))
print*,'		taille (',inx1,',',iny1,')'
!
!-------------------------------------------------
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
allocate(irvb1(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Calcul de la valeur du triplet RVB moyen.
!-------------------------------------------------
!
ztonm=0
do jy=1,iny1
  do jx=1,inx1
    do jc=1,3
      ztonm(jc)=ztonm(jc)+real(irvb1(jc,jx,jy))
    enddo
  enddo
enddo
ztonm=ztonm/(real(inx1)*real(iny1))
write(*,fmt=*) '		Couleur moyenne de l''image d''entrée: (R,V,B) = (',nint(ztonm(1)),',',nint(ztonm(2)),',',nint(ztonm(3)),')'
!
!-------------------------------------------------
! Cbe de fréquence.
!-------------------------------------------------
!
zfreq=0.
do jy=1,iny1
  do jx=1,inx1
    do jcoul=1,3
      zfreq(jcoul,irvb1(jcoul,jx,jy))=zfreq(jcoul,irvb1(jcoul,jx,jy))+1.
    enddo
  enddo
enddo
!
!-------------------------------------------------
! Sortie de la courbe de fréquence de (R+V+B).
!-------------------------------------------------
!
print*,'Fichiers-courbes de fréquence générés: '
do jcoul=1,3
  if(jcoul == 1) then
    clcoul='R'
  elseif(jcoul == 2) then
    clcoul='V'
  elseif(jcoul == 3) then
    clcoul='B'
  else
    write(*,fmt=*) 'imgfreq/ERREUR: couleur non attendue!...'
    write(*,fmt=*) jcoul
    stop 'call abort'
  endif
  write(clfreq,fmt='(9a)') 'freq.',trim(clcoul),'.tmp.dta'
  open(77,file=clfreq,form='formatted')
  do joctet=0,255
    write(77,*) joctet,zfreq(jcoul,joctet)
  enddo
  close(77)
  print*,'	',trim(clfreq)
enddo
end
