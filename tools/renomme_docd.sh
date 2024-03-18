#!/bin/bash
# --------------------------------------------------------------
# 
# --------------------------------------------------------------
# Sujet:
# Methode:
# Externes:
# Auteur:   2018-04, J.M. Piriou.
# Modifications:
# --------------------------------------------------------------
# En entrée:
# En sortie:
# --------------------------------------------------------------
proc=$(basename $0) # nom de la présente procédure.
pref=`tempo`/$proc.$RANDOM # préfixe des fichiers temporaires.
dirloc=`pwd` # chemin du répertoire local.
#
#-----------------------------------------------
# Traitement de la ligne de commande.
#-----------------------------------------------
#
set -evx
#
#-------------------------------------------------
# .
#-------------------------------------------------
#
rm -f *.ren *.manq *.dta *.doc
net
for f in DHFDLARPE+????
do
  renomme_docd $f $f.ren
  ddh_met_val_manq $f.ren $f.ren.manq
  ddhi $f.ren.manq -1HLON -2HLAT -3VP
done
