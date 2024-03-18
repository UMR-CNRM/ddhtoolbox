#
#-----------------------------------------------
# Makefile area.
#-----------------------------------------------
#
LIBLFA="lfa/liblfa.a"
all: ddhi ddh2fbl ddhr ddht ddhbg ddhif ddhmech ddh-asc2lfa mot ddhb_makelists mevol ddhreu ddh2scm ms

io.o: io.F90
	$(FC) -c io.F90

img.o: img.F90
	$(FC) -c img.F90

img_fonte_9x15bold_rvb.o: img_fonte_9x15bold_rvb.F90
	$(FC) -c img_fonte_9x15bold_rvb.F90

img_fonte_9x15bold_taille.o: img_fonte_9x15bold_taille.F90
	$(FC) -c img_fonte_9x15bold_taille.F90

mevol: mevol.F90 mautodoc.F90 dates.F90 cooz.F90 coov.F90 calpaprs.F90 calpaprsf.F90 hydrostatique.F90 litecr.F90 zmodyx.F90 choisit_unite.F90 composite.F90 io.o img.o img_fonte_9x15bold_rvb.o img_fonte_9x15bold_taille.o fonctions.F90 
	$(FC) -o $@ mevol.F90 cooz.F90 coov.F90 calpaprs.F90 calpaprsf.F90 hydrostatique.F90 litecr.F90 zmodyx.F90 choisit_unite.F90 composite.F90 io.o img.o img_fonte_9x15bold_rvb.o img_fonte_9x15bold_taille.o $(LIBLFA)
	rm -f mevol.o

ms: ms.F90 fonctions.F90 mautodoc.F90 hydrostatique.F90 coord_vert.F90
	$(FC) -o $@ ms.F90 $(LIBLFA)

ddh2scm: ddh2scm.F90 fonctions.F90
	$(FC) -o $@ ddh2scm.F90 $(LIBLFA)

ddh-asc2lfa: ddh-asc2lfa.F90
	$(FC) -o $@ ddh-asc2lfa.F90 $(LIBLFA)

ddhmech: ddhmech.F90
	$(FC) -o $@ ddhmech.F90 $(LIBLFA)

ddhif: ddhif.F90
	$(FC) -o $@ ddhif.F90 $(LIBLFA)

ddhbg: ddhbg.F90 pridocf.F90 dates.F90 ddh_util.F90 resol_syst.F90
	$(FC) -o $@ ddhbg.F90 dates.F90 pridocf.F90 resol_syst.F90 $(LIBLFA)

ddhi: const_ther.F90 ddhi.F90 modulo.F90 dates.F90 lisc.F90 ddhi_subr.F90 lited.F90 emagrame.F90 tespv0.F90 autodoc.F90 
	$(FC) -o $@ const_ther.F90 ddhi.F90 modulo.F90 dates.F90 lisc.F90 ddhi_subr.F90 emagrame.F90 tespv0.F90 autodoc.F90 $(LIBLFA)

ddhreu: ddhreu.F90
	$(FC) -o $@ ddhreu.F90 $(LIBLFA)

ddh2fbl: ddh2fbl.F90
	$(FC) -o $@ ddh2fbl.F90 $(LIBLFA)

ddhr: ddhr.F90 pridocf.F90 modulo.F90 dates.F90 fillzero.F90 lited.F90
	$(FC) -o $@ ddhr.F90 pridocf.F90 modulo.F90 dates.F90 fillzero.F90 $(LIBLFA)

ddht: ddht.F90 dates.F90 modulo.F90 ddhpar.h ddht_yom_ent.h lited.F90
	$(FC) -o $@ ddht.F90 dates.F90 modulo.F90 $(LIBLFA)

mot: mot.F90 caracteres.F90 getargp.F90
	$(FC) -o $@ mot.F90 caracteres.F90 getargp.F90

ddhb_makelists: ddhb_makelists.F90 parse_fbl.F90 get_from_lisc.F90
	$(FC) -o $@ ddhb_makelists.F90

clean: 
	rm -f *.o *.a ddhi ddhreu ddh2fbl ddhr ddht ddhbg ddhif ddhmech ddh-asc2lfa mot ddhb_makelists mevol *.mod ddh2scm ms
