program imggomme
! --------------------------------------------------------------
! Changement de pallette de couleur d'une image.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-11, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------


!IMPLICIT NONE
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
CHARACTER*200 climg1,climg2,clpal,clopt,clfreq,clint
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
INTEGER :: JX,ix1,iy1,ix2,iy2,ioctet
INTEGER :: JY,joctet,inpts,irvb_discret(0:255,3)
REAL :: ZRAPA,znorm,zsom,zsom2,zect,zmoy,zfrac
REAL :: zfreq(-3:258),zrep(0:255),zratio
INTEGER :: ifiltre(0:255)
logical llcroiss,llfreq
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
clpal='CONTRASTE'
llcroiss=.true.
llfreq=.false.
clfreq='freq.tmp.dta'
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 6) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Gommage d''un rectangle de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imggomme IMG X1 Y1 X2 Y2 IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imggomme:'
call getargp(1,climg1)
call getargp(2,clint) ; read(clint,fmt=*) ix1
call getargp(3,clint) ; read(clint,fmt=*) iy1
call getargp(4,clint) ; read(clint,fmt=*) ix2
call getargp(5,clint) ; read(clint,fmt=*) iy2
call getargp(6,climg2)
if(ix1 >= ix2 .or. iy1 >= iy2) then
  print*,'ERREUR dans l''ordre des coordonnées du rectangle!...'
  stop 'call abort'
endif
!
!-------------------------------------------------
! Lecture de la taille du ppm d'entrée.
!-------------------------------------------------
!
call img_taille(climg1,inx1,iny1)
print*,'	image ',climg1(1:len_trim(climg1))
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
call img_lec(climg1,inx1,iny1,irvb1)
!
!-------------------------------------------------
! Piratage ad-hoc.
!-------------------------------------------------
!
call gomme1(ix1,iy1,ix2,iy2,inx1,iny1,irvb1,irvb2)
!
!-------------------------------------------------
! Ecriture du résultat sur fichier ppm.
!-------------------------------------------------
!
print*,'		écriture de l''image (',inx1,',',iny1,') résultante'
call img_ecr(climg2,inx1,iny1,irvb2)
print*,'		fichier écrit: ',climg2(1:len_trim(climg2))
print*,' '
end
subroutine gomme1(kx1,ky1,kx2,ky2,knx1,kny1,krvb1,krvb2)
! --------------------------------------------------------------
! Modification d'une image rectangulaire.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2001-11, J.M. Piriou.
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
INTEGER :: knx1,kny1,krvb1(3,knx1,kny1),krvb2(3,knx1,kny1)

do jy=1,kny1
  do jx=1,knx1
    !
    !-------------------------------------------------
    ! .
    !-------------------------------------------------
    !
    if(jx > kx1 .and. jx < kx2 .and. jy> ky1 .and. jy < ky2) then
      zpoidsx=real(jx-kx1)/real(kx2-kx1)
      zpoidsy=real(jy-ky1)/real(ky2-ky1)
      do jcoul=1,3
        krvb2(jcoul,jx,jy)=max(0,min(255, &
        & nint(0.5* &
        & (zpoidsx*real(krvb1(jcoul,kx2,jy))+(1.-zpoidsx)*real(krvb1(jcoul,kx1,jy)) &
        & +zpoidsy*real(krvb1(jcoul,jx,ky2))+(1.-zpoidsy)*real(krvb1(jcoul,jx,ky1))))))
      enddo
    else
      do jcoul=1,3
        krvb2(jcoul,jx,jy)=krvb1(jcoul,jx,jy)
      enddo
    endif
  enddo
enddo
end
