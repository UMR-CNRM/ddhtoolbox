subroutine autodoc(kul,cdficd,cddate,cdorigine)
! --------------------------------------------------------------
! **** ** Génère une autodocumentation en clair sur ce fichier de DDH
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes:
! Auteur:   2021-07, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
!   kul : unité logique du fichier de DDH.
!   cdficd : nom du fichier de DDH.
! En sortie:
!   cddate: champ "#DATE="
!   cdorigine: champ "#ORIGINE="
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

INTEGER(KIND=4) idatef(11)
!
! Echéance.
!
idim1=1
call lfalecr(kul,'ECHEANCE',idim1,zech,igol,irep)
if(zech >= 259200.) then
  ! Cas de runs de plus de 3 jours >> commentaire en jours
  zecho=zech/86400.
  clzue=' days'
else
  ! Cas de runs de moins de 3 jours >> commentaire en heures
  zecho=zech/3600.
  clzue=' hours'
endif
!
! Affichage de l'echeance avec deux chiffres apres la virgule.
! Si l'echeance est voisine d'un entier a mieux que 10**-5 pres,
! on l'affiche au format entier.
!
call reecar(zecho,2,2,clze,ilze)
clechea=trim(clze)//trim(clzue)
call lfaleci(kul,'DATE',11,idatef,ilong,irep)
write(cddate,fmt='(a,i4,4(a,i2.2),2a)') '#DATE=',idatef(1),'-',idatef(2) &
&       ,'-',idatef(3),' ',idatef(4),':',idatef(5),' + ',trim(clechea)
call lfalecc(kul,'INDICE EXPERIENCE',1,clnamx,ilong,irep)
cdorigine='#ORIGINE='//trim(clnamx)//' : '//trim(cdficd)
end
