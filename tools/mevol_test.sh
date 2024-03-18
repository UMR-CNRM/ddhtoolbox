#!/bin/bash
# --------------------------------------------------------------
# 
# --------------------------------------------------------------
# Sujet:
# Methode:
# Externes:
# Auteur:   2002-03, J.M. Piriou.
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
# Graphe (temps,z,valeur).
#-----------------------------------------------
#
if [ "y" = "y" ] ; then
	cd $dirloc/mevol_composite_validation
	for dir in *
	do
		cd $dir
		for xcompo in 600 
		do
			mevol -compo -xcompo$xcompo O*lfa
			v mevol.composite.gif
		done
		cd ..
	done
fi
#
#-----------------------------------------------
# Test de production de data ASCII.
#-----------------------------------------------
#
if [ "y" = "y" ] ; then
	cd $dirloc
	#
	#-----------------------------------------------
	# Tempé.
	#-----------------------------------------------
	#
	exe="mevol PT O*lfa"
	echo $exe
	$exe
	v PT.tmp.doc
	echo "Taper RC pour continuer." ; read toto
	#
	#-----------------------------------------------
	# qv, niveaux pression.
	#-----------------------------------------------
	#
	exe="mevol -yPRE PQ O*lfa"
	echo $exe
	$exe
	v PQ.tmp.doc
	echo "Taper RC pour continuer." ; read toto
	#
	#-----------------------------------------------
	# Intégrale verticale de la CVGQ, en évolution, temps SYB.
	#-----------------------------------------------
	#
	exe="mevol -nl -ninteg -xSYB PCVGQ O*lfa"
	echo $exe
	$exe
	v PCVGQ.tmp.doc
	echo "Taper RC pour continuer." ; read toto
fi
