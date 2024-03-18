program imgpirate
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

!
!-------------------------------------------------
! Types implicites.
!-------------------------------------------------
!
implicit character*200 (c)
implicit logical (l)
INTEGER,ALLOCATABLE :: irvb1(:,:,:)
INTEGER,ALLOCATABLE :: irvb2(:,:,:)
CHARACTER*200 climg1,climg2,clpal,clopt,clfreq
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
climg2='gol.ppm'
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 3) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Détourage de l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgpirate FICPOLYGONE IMG IMGSORTIE'
  write(*,'(9a)') ' '
  stop
endif
print*,'imgpirate:'
call getargp(1,clpolygone)
call getargp(2,climg1)
call getargp(3,climg2)
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
call pirate3(clpolygone,inx1,iny1,irvb1,irvb2)
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
subroutine pirate3(cdpolygone,knx1,kny1,krvb1,krvb2)
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
INTEGER :: jx,jy,jcoul
integer, parameter :: jppol=200
real :: zxpol(jppol),zypol(jppol)
character*(*) cdpolygone

!
!-------------------------------------------------
! Lecture du polygone.
!-------------------------------------------------
!
!
!-------------------------------------------------
! Ouverture du fichier d'entrée.
!-------------------------------------------------
!
write(*,fmt=*) 'cdpolygone=',cdpolygone
iul1=22 ; open(iul1,file=cdpolygone,form='formatted')
!
!-------------------------------------------------
! Lecture séquentielle.
!-------------------------------------------------
!
ipol=0
do
  read(iul1,fmt=*,iostat=ios) zxpol(ipol+1),zypol(ipol+1)
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
    ipol=ipol+1
  else
    !
    !-------------------------------------------------
    ! Cas non prévu.
    !-------------------------------------------------
    !
    write(*,fmt=*) 'Code réponse en lecture non prévu: ',ios
    stop 'call abort'
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
close(iul1)
write(*,fmt=*) 'ipol=',ipol

krvb2=krvb1
do jy=1,kny1
  do jx=1,knx1
    zxt=real(jx)
    zyt=real(jy)
    call polygones(.false.,ipol,zxpol,zypol,zxt,zyt,zdist)
    if(zdist > 0.) then
      !
      !-------------------------------------------------
      ! Le point courant est hors du polygone.
      ! Les points situés à plus de 60 pixels
      ! du polygone sont mis à un fond fixe, 
      ! avec fondu du polygone vers ce fond fixe.
      !-------------------------------------------------
      !
      zopac=zlisse(zdist/60.)
      do jcoul=1,3
        krvb2(jcoul,jx,jy)=max(0,min(255, &
        & nint(zopac*200.+(1.-zopac)*real(krvb1(jcoul,jx,jy)))))
      enddo
    else
    endif
  enddo
enddo
end
#include"/home/piriou/ftn/polygones.F90"
