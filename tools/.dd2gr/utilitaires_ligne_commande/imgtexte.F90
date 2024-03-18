program imgtexte
! --------------------------------------------------------------
! 
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
integer irvb_fond_texte(3)
integer irvb_pp_texte(3)
integer iloc(3)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
zopac_fond_texte=0.7
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 5 .and. iarg /= 6) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Ecriture d''un texte sur l''image d''entrée.'
  write(*,'(9a)') ' '
  write(*,'(9a)') 'Utilisation: imgtexte IMG "TEXTE" Xtexte Ytexte IMGSORTIE [opac]'
  write(*,'(9a)') 'avec'
  write(*,'(a,f5.2)') '	opac l''opacité du fond du texte écrit (blanc) sur l''image d''entrée. Défaut: ',zopac_fond_texte
  write(*,'(9a)') ' '
  stop
endif
print*,'imgtexte:'
call getargp(1,climg1)
call getargp(2,cltexte)
call getargp(3,clxtexte)
read(clxtexte,fmt=*) ixtexte
call getargp(4,clytexte)
read(clytexte,fmt=*) iytexte
call getargp(5,climg2)
if(iarg == 6) then
  call getargp(6,clopac)
  read(clopac,fmt=*) zopac_fond_texte
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
print*,'		modification de l''image d''entrée'
!
!-------------------------------------------------
! On initialise l'image de sortie à l'image d'entrée.
!-------------------------------------------------
!
irvb2=irvb1
!
!-------------------------------------------------
! Couleurs de fond et pp du texte.
!-------------------------------------------------
!
irvb_fond_texte(1)=255 ; irvb_fond_texte(2)=255 ; irvb_fond_texte(3)=255
irvb_pp_texte(1)=0 ; irvb_pp_texte(2)=0 ; irvb_pp_texte(3)=0
!
!-------------------------------------------------
! Opacité du fond et du pp.
!-------------------------------------------------
!
zopac_pp_texte=1.
!
!-------------------------------------------------
! Fonte du texte.
!-------------------------------------------------
!
ifonte=1
!
!-------------------------------------------------
! Coordonnées du texte sur l'image de sortie.
!-------------------------------------------------
!
iloc(1)=10 ! texte à centrer.
iloc(2)=ixtexte ! X du texte.
iloc(3)=iytexte ! Y du texte.
call img_texte(cltexte,irvb_fond_texte,zopac_fond_texte &
&,irvb_pp_texte,zopac_pp_texte,ifonte,iloc,inx1,iny1,irvb2)
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
