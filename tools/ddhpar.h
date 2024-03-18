	!                                                                         
	! Dimensions maximales des donnees de fichiers de DDH.                    
	!                                                                         
	INTEGER(KIND=4), parameter :: jplev=300 ! nombre maximal de niveaux de variables.
	INTEGER(KIND=4), parameter :: jplev2=jplev+1 ! nombre maximal de niveaux de flux.
	INTEGER(KIND=4), parameter :: jpdom=15000 ! nombre maximal de domaines.
	INTEGER(KIND=4), parameter :: jpnomct=50000 ! nombre de champs maxi au sein d'un fichier DDH-LFP.
	INTEGER(KIND=4), parameter :: jpnomcl=jpdom*jpnomct ! nombre de champs maxi au sein d'un fichier DDH-LFI.
	INTEGER(KIND=4), parameter :: jparg=500 ! nombre max. d'arguments sur la ligne de commande.
	INTEGER(KIND=4), parameter :: jpprod=jplev2*jpdom ! dimension maxi du tableau unidim. total.
	INTEGER(KIND=4), parameter :: jpdoc=11 ! longueur des articles de documentation des domaines.
	INTEGER(KIND=4), parameter :: jpsol=50 ! longueur maxi d'un tableau de champs sol.
	INTEGER(KIND=4), parameter :: jpsolt=jpsol*jpdom ! longueur maxi d'un tableau unidim. de champs so
	INTEGER(KIND=4), parameter :: jpprecint=4 ! precision des entiers en octets.
	INTEGER(KIND=4), parameter :: jpprecree=4 ! precision des reels en octets.
