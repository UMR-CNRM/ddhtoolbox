!OPTIONS NODOUBLE
program convd
! --------------------------------------------------------------------------
! **** *lfi2lfa*  Conversion d'un fichier LFI de ddh en donnees formattees.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites: /
! ---------------------
! Arguments implicites: /
! ---------------------
! Methode:
! Methode:
! --------
! Externes: /
! ---------
! Auteur:           94-10, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! argument 1 de la ligne de commande: nom du fichier LFI d'entree.
! argument 2 de la ligne de commande: nom du fichier-liste des cham
! argument 3 de la ligne de commande: nom du fichier lfa de sortie.
! En sortie:
! --------------------------------------------------------------------------
implicit none
#include"ddhpar.h"
character*16 cladcou
character*17 cllisct(jpnomcl), cllis
character*200 clfe,clfs,clna,cltype,clsttu,clnamx,clsol
integer(kind=4) iulent,ilfs,iulsor,iprecree,ilfe,ilong,iret &
&   ,inimes,inbarp,inbari,ilfnom,irep,iposex,idom,jdom,ilisct0 &
&   ,ilisct,jlisct,illis,iok,jlisct2,ib,jree,jx,inomvs,jchamp
real(kind=8) zech,zseuil,zseuilx
logical llex,lllfa,llnomm,llerfa,llimst
!
real(kind=8) ztab(jplev)
real(kind=8) ztabt(jpprod)
!
real(kind=8) zsole(jpsol) ! tableau de sortie de LFILEC Sol.
real(kind=8) zsolt(jpsol,jpdom) ! tableau intermediaire sol.
real(kind=8) zsols(jpdom) ! tableau d'entree de lfaECRR sol.
!
integer(kind=4) idatef(11)
real(kind=8) zdocdom(11)
integer(kind=4) idocfi(17)
iulent=14
call getargp(1,clfe)
!
! Le fichier existe-t-il?
!
inquire(file=clfe,exist=llex)
if(.not.llex) then
  print*,'ddh-lfi2lfa/ERREUR: fichier d''entrée inexistant!...'
  stop
endif
!
! Ouverture du fichier lfa de sortie.
!
call getargp(2,clfs)
ilfs=len_trim(clfs)
print*,'Ouverture du fichier LFA de sortie ',clfs(1:ilfs)
iulsor=9
call lfaouv(iulsor,clfs,'W')
call lfapreci(iulsor,jpprecint)
iprecree=jpprecree
call lfaprecr(iulsor,iprecree)
!
! Le fichier est-il déja de type lfa?
!
!call lfatest(iulent,clfe,lllfa)
lllfa=.false.
if(lllfa) then
  !
  ! Le fichier est déjà lfa.
  ! On se contente de le recopier globalement.
  ! Ouverture du fichier lfa d'entrée.
  !
  ilfe=len_trim(clfe)
  print*,'Ouverture du fichier lfa d''entrée ',clfe(1:ilfe)
  call lfaouv(iulent,clfe,'R')
100    continue
  !
  ! Renseignements sur l'article suivant du fichier.
  !
  clna=' '
  call lfacas(iulent,clna,cltype,ilong,iret)
  if(iret == 0) then
    !
    ! On n'est pas en fin de fichier.
    ! Copie de l'article courant sur le fichier de sortie.
    !
    call lfacop(iulent,clna,iulsor)
    goto 100
  else
    !
    ! Fermeture du fichier d'entrée.
    !
    call lfafer(iulent)
  endif
else
  !
  ! Le fichier n'est pas lfa.
  ! Ouverture du fichier LFI de DDH.
  !
  llnomm=.true.
  clsttu='OLD'
  llerfa=.true.
  llimst=.false.
  inimes=0
  inbarp=15
  inbari=0
  ilfnom=len_trim(clfe)
  print*,'Ouverture du fichier LFI d''entrée ',clfe(1:ilfnom)
  call lfiouv(irep,iulent,llnomm,clfe,clsttu,llerfa,llimst, &
&   inimes,inbarp,inbari)
  !
  ! On rend non fatale toute erreur de lecture du fichier.
  !
  call lfierf(irep,iulent,.false.)
  !
  ! Lecture du premier article: indice en 4 lettres de l'experience.
  !
  call lficas(irep,iulent,clnamx,ilong,iposex,.false.)
  call lfaecrc(iulsor,'INDICE EXPERIENCE',clnamx,1)
  print*,'INDICE EXPERIENCE=',clnamx
  !
  ! Lecture du 2eme    article: DATE.
  !
  ilong=11
  cladcou='DATE'
  call lfiintlec(irep,iulent,cladcou,idatef(1),ilong)
  call lfaecri(iulsor,cladcou,idatef,ilong)
  print*,'DATE=',idatef
  !
  ! Lecture du 3eme    article: DOCFICHIER.
  !
  ilong=17
  cladcou='DOCFICHIER'
  call lfiintlec(irep,iulent,cladcou,idocfi(1),ilong)
  call lfaecri(iulsor,cladcou,idocfi,ilong)
  !
  ! Lecture article d'echeance.
  !
  ilong=1
  cladcou='ECHEANCE'
  call lfilec(irep,iulent,cladcou,zech,ilong)
  call lfaecrr(iulsor,cladcou,zech,ilong)
  print*,'ECHEANCE=',zech,' s, soit ',zech/86400.,' j.'
  !
  ! Articles de renseignement sur les domaines.
  !
  idom=idocfi(15)
  do jdom=1,idom
    write(cladcou,fmt='(a,i3.3)') 'DOCD',jdom
    ilong=11
    call lfilec(irep,iulent,cladcou,zdocdom(1),ilong)
    call lited(iulsor,jdom,'W',zdocdom,irep)
  enddo
  !
  ! On rend non fatale toute erreur de lecture du fichier.
  !
  call lfierf(irep,iulent,.false.)
  !
  ! Chargement de la liste de conversion.
  ! On obtient dans un premier temps la liste de TOUS les articles
  ! écrits sur le fichier LFI.
  !
  call etalae(iulent,cllisct,jpnomcl,ilisct0)
  print*,ilisct0,' champs presents dans le fichier LFI.'
  !
  ! On va ne garder de cette liste que les noms différents
  ! au sens DDH du terme, i.e. 001VQV0 et 002VQV0
  ! fourniront le même nom en sortie,
  ! et un nom tel que 'DATE' ne sera pas conservé.
  !
  ilisct=0
  do jlisct=2,ilisct0 ! on démarre à 2 car le premier est l'indice expérience.
    cllis=cllisct(jlisct)
    illis=len_trim(cllis)
    if(cllis(1:illis) /= 'DATE' &
&     .and.cllis(1:illis) /= 'DOCFICHIER' &
&     .and.cllis(4:7) /= 'S V0' &
&     .and.cllis(4:7) /= 'S V1' &
&     .and.cllis(4:10) /= 'S FVRAC' &
&     .and.cllis(4:8) /= 'SVGFS' &
&     .and.cllis(4:8) /= 'SFGFS' &
&     .and.cllis(1:4) /= 'DOCD' &
&     .and.cllis(1:illis) /= 'ECHEANCE') then
      !
      ! Il s'agit d'un article de champ réel DDH.
      ! On va vérifier qu'il n'a pas déjà été rencontré.
      !
      iok=1
      do jlisct2=1,ilisct
        if(cllisct(jlisct2)(4:16) == cllisct(jlisct)(4:16)) then
          !
          ! Cet article a déjà été rencontré.
          !
          iok=0
        endif
      enddo
      if(iok == 1) then
        !
        ! On le porte sur la liste de sortie.
        !
        ilisct=ilisct+1
        cllisct(ilisct)=cllisct(jlisct)
      endif
    endif
  enddo
  !
  ! On va lire tous les champs a traiter.
  !
  do jlisct=1,ilisct
    cllis=cllisct(jlisct)
    !
    ! La ligne du fichier-liste n'est pas de commentaire.
    !
    ib=0
    do jdom=1,idom
      !
      ! Ecriture du nom de l'article.
      !
      write(cladcou,fmt='(i3.3,a)') jdom,cllis(4:16)
      if(cladcou(4:4) == 'F') then
        ilong=idocfi(6)+1
      else
        ilong=idocfi(6)
      endif
      !
      ! Lecture sur le fichier LFI de DDH.
      !
      call lfilec(irep,iulent,cladcou,ztab,ilong)
      if(irep /= 0) then
        print*,'Probleme sur le champ ',cladcou,': irep=',irep,'!...'
        goto 755
      endif
      !
      ! Afin de ne pas faire "planter" les stations
      ! de travail, les reels trop petits
      ! ou grands en valeur absolue
      ! sont portes sur une valeur seuil donnee.
      !
      zseuil=1.e-20
      zseuilx=1./zseuil
      do jree=1,ilong
        if(abs(ztab(jree)) < zseuil.and.ztab(jree) /= 0.) then
          if(ztab(jree) < 0.) then
            ztab(jree)=-zseuil
          else
            ztab(jree)=zseuil
          endif
        elseif(abs(ztab(jree)) > zseuilx) then
          if(ztab(jree) < 0.) then
            ztab(jree)=-zseuilx
          else
            ztab(jree)=zseuilx
          endif
        endif
      enddo
      !
      ! Le champ existe dans le fichier LFI de DDH.
      ! On l'ecrit sur le tableau ztabt
      ! qui sera porte sur le lfa.
      !
      do jx=1,ilong
        ib=ib+1
        ztabt(ib)=ztab(jx)
      enddo
    enddo
    call lfaecrr(iulsor,cladcou(4:),ztabt,ib)
    ! print*,'Champ ',cladcou(4:),' ecrit sur fichier lfa.'
  755     continue
  enddo
  !
  ! ** Lecture des champs sol.
  !
  ! Variables initiales.
  !
  inomvs=idocfi(12) ! nombre de variables sol initiales ou finales.
  do jdom=1,idom
    write(cladcou,fmt='(i3.3,a)') jdom,'S V0'
    ilong=idocfi(12)
    call lfilec(irep,iulent,cladcou,zsole,ilong)
    do jchamp=1,inomvs
      zsolt(jchamp,jdom)=zsole(jchamp)
    enddo
  enddo
  !
  ! Ecriture sur le lfa.
  !
  do jchamp=1,inomvs
    do jdom=1,idom
      zsols(jdom)=zsolt(jchamp,jdom)
    enddo
    write(clsol,fmt='(a,i2.2,a)') 'S',jchamp,'_0'
    call lfaecrr(iulsor,clsol,zsols,idom)
  enddo
  !
  ! Variables finales.
  !
  do jdom=1,idom
    write(cladcou,fmt='(i3.3,a)') jdom,'S V1'
    ilong=idocfi(12)
    call lfilec(irep,iulent,cladcou,zsole,ilong)
    do jchamp=1,inomvs
      zsolt(jchamp,jdom)=zsole(jchamp)
    enddo
  enddo
  !
  ! Ecriture sur le lfa.
  !
  do jchamp=1,inomvs
    do jdom=1,idom
      zsols(jdom)=zsolt(jchamp,jdom)
    enddo
    write(clsol,fmt='(a,i2.2,a)') 'S',jchamp,'_1'
    call lfaecrr(iulsor,clsol,zsols,idom)
  enddo
  !
  ! Flux.
  !
  inomvs=idocfi(14) ! nombre de variables initiales ou finales.
  do jdom=1,idom
    write(cladcou,fmt='(i3.3,a)') jdom,'S FVRAC'
    ilong=idocfi(14)
    call lfilec(irep,iulent,cladcou,zsole,ilong)
    do jchamp=1,inomvs
      zsolt(jchamp,jdom)=zsole(jchamp)
    enddo
  enddo
  !
  ! Ecriture sur le lfa.
  !
  do jchamp=1,inomvs
    do jdom=1,idom
      zsols(jdom)=zsolt(jchamp,jdom)
    enddo
    write(clsol,fmt='(a,i2.2)') 'G',jchamp
    call lfaecrr(iulsor,clsol,zsols,idom)
  enddo
  !
  ! ** Lecture des variables sol "libres": vent à 10m, T2m, etc...
  !
  inomvs=idocfi(16) ! nombre de variables sol libres.
  do jdom=1,idom
    write(cladcou,fmt='(i3.3,a)') jdom,'SVGFS'
    ilong=inomvs
    call lfilec(irep,iulent,cladcou,zsole,ilong)
    do jchamp=1,inomvs
      zsolt(jchamp,jdom)=zsole(jchamp)
    enddo
  enddo
  !
  ! Ecriture sur le lfa.
  !
  do jchamp=1,inomvs
    do jdom=1,idom
      zsols(jdom)=zsolt(jchamp,jdom)
    enddo
    write(clsol,fmt='(a,i2.2)') 'SVGFS',jchamp
    call lfaecrr(iulsor,clsol,zsols,idom)
  enddo
  !
  ! ** Lecture des flux au sol "libres".
  !
  inomvs=idocfi(17) ! nombre de flux sol libres.
  do jdom=1,idom
    write(cladcou,fmt='(i3.3,a)') jdom,'SFGFS'
    ilong=inomvs
    call lfilec(irep,iulent,cladcou,zsole,ilong)
    do jchamp=1,inomvs
      zsolt(jchamp,jdom)=zsole(jchamp)
    enddo
  enddo
  !
  ! Ecriture sur le lfa.
  !
  do jchamp=1,inomvs
    do jdom=1,idom
      zsols(jdom)=zsolt(jchamp,jdom)
    enddo
    write(clsol,fmt='(a,i2.2)') 'SFGFS',jchamp
    call lfaecrr(iulsor,clsol,zsols,idom)
  enddo
  !
  ! Fermeture du fichier DDH d'entree
  !
  call lfifer(irep,iulent,'UNKNOWN')
endif
!
! Fermeture du lfa de sortie.
!
call lfafer(iulsor)
end
#include"lited.F90"
