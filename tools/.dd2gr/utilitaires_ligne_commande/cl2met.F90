program cl
! --------------------------------------------------------------
! **** *CL* Passage d'une liste de réels.
! --------------------------------------------------------------
! Sujet:
! Arguments explicites:
! Arguments implicites:
! Methode:
! Externes: call img*
! Auteur:   2005-01, J.M. Piriou.
! Modifications:
! --------------------------------------------------------------
! En entree:
! En sortie:
! --------------------------------------------------------------
implicit character*200 (c)
implicit logical (l)
parameter(jpclas=200)
real zclas(jpclas)
!
!-------------------------------------------------
! Initialisation par défaut.
!-------------------------------------------------
!
cldoc='c.doc'
!
!-------------------------------------------------
! Saisie de la ligne de commande.
!-------------------------------------------------
!
iarg=iargcp() ! nombre d'arguments.
if(iarg /= 1 .and. iarg /= 2) then
  !
  !-------------------------------------------------
  ! Le nombre d'arguments n'est pas celui requis
  ! pour exécuter. On fournit la documentation.
  !-------------------------------------------------
  !
  write(*,'(a)') ' '
  write(*,'(a)') 'Passage d''une liste de réels,'
  write(*,'(a)') 'représentant des frontières de classes,'
  write(*,'(a)') 'en une palette de couleurs pour metview.'
  write(*,'(a)') ' '
  write(*,'(a)') 'Utilisation: cl2met FIC_classes [FIC_doc]'
  write(*,'(2a)') 'Valeur par défaut de FIC_doc: ',trim(cldoc)
  write(*,'(a)') ' '
  stop
endif
!
!-------------------------------------------------
! Nombre d'arguments OK.
!-------------------------------------------------
!
print*,'cl2met:'
call getargp(1,clclas)
if(iarg == 2) call getargp(2,cldoc)
!
!-------------------------------------------------
! Lecture du fichier d'entrée.
!-------------------------------------------------
!
iul=22
open(iul,file=clclas,form='formatted')
iclas=0
do
  read(iul,fmt=*,iostat=ios) zclas(iclas+1)
  if(ios /= 0) exit
  iclas=iclas+1
  if(iclas == 1) then
    zmin=zclas(1)
    zmax=zmin
  else
    zmin=min(zmin,zclas(iclas))
    zmax=max(zmax,zclas(iclas))
  endif
enddo
close(iul)
!
!-------------------------------------------------
! On ne gardera une classe que si, arrondie à ichis chiffres
! significatifs près, elle est différente de la précédente:
! on effectue ce test afin de ne pas produire n valeurs
! de classes très voisines au voisinage d'un "Dirac".
! ichis vaut au moins 2, et vaudra plus (ichisp) si les max et min sont
! très voisins en relatif (ex: le champ varie entre 1.000000001
! et 1.000000002).
!-------------------------------------------------
!
if(zmin*zmax > 0.) then
  !
  !-------------------------------------------------
  ! Le min et le max sont de même signe.
  ! Plus leur écart relatif sera faible, plus le nombre
  ! de chiffres significatifs ichisp à considérer
  ! sera grand.
  !-------------------------------------------------
  !
  zamin=abs(zmin)
  zamax=abs(zmax)
  ichisp=1-nint(log(abs((zamin-zamax)/(real(iclas)*max(zamin,zamax))))/log(10.)-0.5)
else
  !
  !-------------------------------------------------
  ! Le min et le max sont de signes contraires.
  !-------------------------------------------------
  !
  ichisp=0
endif
ichis=max(2,ichisp)+1
!print*,'	Classes sur ',ichis,' chiffres significatifs.'
!
!-------------------------------------------------
! Boucle pour éliminier les classes trop voisines
! et arrondir les classes à ichis chiffres significatifs.
!-------------------------------------------------
!
iclas2=iclas
iclas=0
do jclas=1,iclas2
  zval=zclas(jclas)
  call arrondi(zval,ichis,zval_arr)
  if(iclas == 0) then
    !
    !-------------------------------------------------
    ! Première classe lue sur le fichier.
    !-------------------------------------------------
    !
    iclas=1
    zclas(iclas)=zval_arr
  else
    !
    !-------------------------------------------------
    ! Classes suivantes.
    !-------------------------------------------------
    !
    if(zval_arr /= zval_arr_prec) then
      iclas=iclas+1
      zclas(iclas)=zval_arr
    else
      print*,'	INFO: classe ',zclas(jclas),' écartée: trop proche de la précédente ',zval_arr_prec
    endif
  endif
  zval_arr_prec=zval_arr
enddo
!
!-------------------------------------------------
! Frontières metview.
!-------------------------------------------------
!
iulout=44
open(iulout,file=cldoc,form='formatted')
!
!-------------------------------------------------
! Appel du calcul de couleurs metview.
!-------------------------------------------------
!
call genpcm(zclas,iclas,iulout)
!
!-------------------------------------------------
! Tracé final.
!-------------------------------------------------
!
write(iulout,fmt='(a)') '#LABEL'
close(iulout)
print*,'	',iclas2,' classes en entrée, ',iclas,' classes en sortie.'
print*,'	Fichier généré: ',trim(cldoc)
end
#include"genpcm.F90"
#include"../../ftn/arrondi.F90"
