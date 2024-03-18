program jux
! --------------------------------------------------------------
! Combinaison de deux images ppm.
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

INTEGER,ALLOCATABLE :: irvb(:,:,:)
character*200 clmot(180)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
clcgau='255,255,255'
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
  write(*,'(9a)') 'Ecrit une croix en un lieu donné d''une image.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgcroix [-cRVB] IMGENTREE X Y IMGSORTIE'
  write(*,'(9a)') ' '
  write(*,'(9a)') '	-c couleur de la croix en RVB.'
  write(*,'(9a)') '	  Défaut: ',trim(clcgau)
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Exemple: imgcroix -c255,255,0 REF.jpg 200 352 ASC.gif'
  write(*,'(9a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
clx=' '
cly=' '
climg1=' '
climg2=' '
do jarg=1,iarg
  call getargp(jarg,clarg)
  if(clarg(1:2) == '-c') then
    clcgau=clarg(3:)
  elseif(climg1 == ' ') then
    climg1=clarg
  elseif(clx == ' ') then
    clx=clarg
  elseif(cly == ' ') then
    cly=clarg
  elseif(climg2 == ' ') then
    climg2=clarg
  endif
enddo
read(clx,fmt=*) ix
read(cly,fmt=*) iy
!
!-------------------------------------------------
! Lecture des valeurs RVB.
!-------------------------------------------------
!
call casc(clcgau,10,clmot,imot)
read(clmot(1),fmt=*) irgau
read(clmot(2),fmt=*) ivgau
read(clmot(3),fmt=*) ibgau
write(*,fmt=*) ' '
write(*,fmt=*) 'imgcroix:'
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
! Lecture de l'image 1.
!-------------------------------------------------
!
print*,'		lecture de l''image d''entrée'
allocate(irvb(3,inx1,iny1))
call img_lec(climg1,inx1,iny1,irvb)
!
!-------------------------------------------------
! Ecriture d'une croix.
!-------------------------------------------------
!
itaic=10 ! demi-taille de la croix en pixels.
ix_min=max(1,min(inx1,ix-itaic))
ix_max=max(1,min(inx1,ix+itaic))
iy_min=max(1,min(iny1,iy-itaic))
iy_max=max(1,min(iny1,iy+itaic))
!
!-------------------------------------------------
! Croix centrale.
!-------------------------------------------------
!
do jx=ix_min,ix_max
  irvb(1,jx,iy)=irgau
  irvb(2,jx,iy)=ivgau
  irvb(3,jx,iy)=ibgau
enddo
do jy=iy_min,iy_max
  irvb(1,ix,jy)=irgau
  irvb(2,ix,jy)=ivgau
  irvb(3,ix,jy)=ibgau
enddo
!
!-------------------------------------------------
! Bords du carré.
!-------------------------------------------------
!
!ibord=2
!do jx=ix_min,ix_max
!	do jy=iy_min,iy_max
!		if(jx < ix_min+ibord .or. jx > ix_max-ibord) then
!			irvb(1,jx,jy)=irgau
!			irvb(2,jx,jy)=ivgau
!			irvb(3,jx,jy)=ibgau
!		endif
!		if(jy < iy_min+ibord .or. jy > iy_max-ibord) then
!			irvb(1,jx,jy)=irgau
!			irvb(2,jx,jy)=ivgau
!			irvb(3,jx,jy)=ibgau
!		endif
!	enddo
!enddo
!
!-------------------------------------------------
! Ecriture du résultat sur fichier.
!-------------------------------------------------
!
call img_ecr(climg2,inx1,iny1,irvb)
print*,'	fichier écrit: ',trim(climg2)
print*,' '
end
#include"../../caracteres.F90"
