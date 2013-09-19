# Generic make file for LaTeX: requires GNU make

TEXFILE = thesis.tex

.PHONY: dvi ps pdf open clean


pdf:    $(TEXFILE:.tex=.pdf)
ps:     $(TEXFILE:.tex=.ps)
dvi:    $(TEXFILE:.tex=.dvi)
aux:    $(TEXFILE:.tex=.aux)

%.dvi: %.tex
	\latex $<;
	\bibtex $(TEXFILE:.tex=);
	\latex $<
	( \
	\latex $<; \
	while \grep -q "Rerun to get cross-references right." $(<:.tex=.log); \
	do \
	        \latex $<; \
	done \
	)


%.ps: %.dvi
#	\dvips -q $<      # para o caso de querer papel tipo 8.5"x11"
	\dvips -q -t a4 $<      # para sair em papel a4

%.pdf: %.ps
	\ps2pdf -dAutoRotatePages=/None -dPDFSETTINGS=/prepress $< ; \
	\evince $(TEXFILE:.tex=.pdf) &

clean:
	@\rm -f \
	*.aux \
	$(TEXFILE:.tex=.log) \
	$(TEXFILE:.tex=.lof) \
	$(TEXFILE:.tex=.lot) \
	$(TEXFILE:.tex=.toc) \
	$(TEXFILE:.tex=.out) \
	$(TEXFILE:.tex=.dvi) \
	$(TEXFILE:.tex=.pdf) \
	$(TEXFILE:.tex=.ps)  \
	$(TEXFILE:.tex=.bm)  \
	$(TEXFILE:.tex=.bbl) \
	$(TEXFILE:.tex=.blg) \
		*~

