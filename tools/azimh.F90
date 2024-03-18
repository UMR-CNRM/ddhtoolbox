subroutine azimh(plat, pdelta, pfjour, pazim, phaut)
! --------------------------------------------------------------------------
! **** *AZIMH* Calcul de l'azimuth et de la hauteur du Soleil.
! --------------------------------------------------------------------------
! Sujet:
! ------
! Arguments explicites:
! ---------------------
! Arguments implicites:
! ---------------------
! Methode:
! --------
! Externes:
! ---------
! Auteur:   94-12, J.M. Piriou.
! -------
! Modifications:
! --------------------------------------------------------------------------
! En entree:
! PLAT        LATITUDE DU LIEU (RADIANS)
! PDELTA      DECLINAISON DU SOLEIL (RADIANS)
! PFJOUR      FRACTION DE JOUR SOLAIRE VRAI (0 à MINUIT, .5 à MIDI, ETC...)
! En sortie:
! PAZIM       AZIMUT DU SOLEIL (RADIANS)
! PHAUT       HAUTEUR DU SOLEIL (RADIANS)
! --------------------------------------------------------------------------
zpi = 4. * atan(1.)
! CALCUL DE L'ANGLE HORAIRE DU SOLEIL: 0 A MIDI, PI A MINUIT
zah = (pfjour - .5) * 2 * zpi
!
!-------------------------------------------------
! CALCUL DE LA POSITION DU SOLEIL A L'INSTANT CONSIDERE:
! CALCUL DU VECTEUR UNITAIRE DE LA DIRECTION DU SOLEIL.
!-------------------------------------------------
!
! COORDONNEE EST:
z1 = -cos(pdelta) * sin(zah)
!
! COORDONNEE NORD:
z2 = sin(pdelta) * cos(plat) - cos(pdelta) * cos(zah) * sin(plat)
!
! COORDONNEE VERTICALE:
z3 = sin(pdelta) * sin(plat) + cos(pdelta) * cos(zah) * cos(plat)
!
!-------------------------------------------------
! Calcul de la hauteur.
!-------------------------------------------------
!
zdistzen = acos(z3)
phaut = zpi / 2 - zdistzen
!
!-------------------------------------------------
! Calcul de l'azimuth.
!-------------------------------------------------
!
call recpol(z1, z2, zrayon, zang)
pazim=zpi/2.-zang
if(pazim.lt.0.) then
  pazim=pazim+2.*zpi
elseif(pazim.gt.2.*zpi) then
  pazim=pazim-2.*zpi
endif
end
