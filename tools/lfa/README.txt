------------------------------------------------------------------------

LFA (Autodocumented Files Software):

------------------------------------------------------------------------

PURPOSE:  this  software  performs  file read/write of real, integer and
character data. Articles in the file are accessed by their NAME from the
fortran  or  UNIX  user interface; this allows the reader both to forget
about  physical order in the file, and to get safer I/O through type and
length checking.

FORTRAN  AND  UNIX  INTERFACE:  the  software can be called from fortran
codes, and also from UNIX system: "lfaminm file" from command line gives
for  example  the  minima  and  maxima  of  all  articles of a file. See
documentations documentation/lfa*.ps for further details.

FILE   PORTABILITY:   files   are  sequential  unformatted  ones.  Since
unformatted I/O is a copy from/to memory, files will be portable between
machines having the same data memory representation, for example between
machines matching the IEEE standard. 

------------------------------------------------------------------------

TO INSTALL THE LFA (Autodocumented Files Software):

1.  Put  '.'  (the  current  directory)  in  your PATH, with the highest
priority.

2.  Type  'install'. The script recognizes the machine type, and deduces
automatically command line options.

3. Put the lfa directory in your PATH.

4.  Read  the documentations documentation/lfa*.pdf, to know what are LFA
files and how to use them.

For questions or bug reports please contact


--------------------------------------------------------

	Jean-Marcel PIRIOU
	METEO-FRANCE/CNRM/GMAP
	42, avenue G. Coriolis
	F-31057 TOULOUSE Cedex 1

	Tel:+33 5 61 07 84 62
	Fax:+33 5 61 07 84 53
	Mailto:Jean-Marcel.Piriou@meteo.fr
	http://www.meteo.fr

--------------------------------------------------------
