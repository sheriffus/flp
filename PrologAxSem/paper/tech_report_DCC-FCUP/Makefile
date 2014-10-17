# Generic make file for LaTeX: requires GNU make


NAME = report

.PHONY: default

default: $(NAME).pdf


%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs %.fmt *.bib *.tex ../*.tex
	lhs2TeX  $< > $@

clean:
	$(RM) *.aux *.aux.bak *.bbl *.blg *.fdb_latexmk *.log *.ptb
	$(RM) $(NAME).tex $(NAME).pdf



# %.dvi: %.tex
# 	\latex $<;
# 	\bibtex $(TEXFILE:.tex=);
# 	\latex $<
# 	( \
# 	\latex $<; \
# 	while \grep -q "Rerun to get cross-references right." $(<:.tex=.log); \
# 	do \
# 	        \latex $<; \
# 	done \
# 	)


# %.ps: %.dvi
# #	\dvips -q $<      # para o caso de querer papel tipo 8.5"x11"
# 	\dvips -q -t a4 $<      # para sair em papel a4

# %.pdf: %.ps
# 	\ps2pdf -dAutoRotatePages=/None -dPDFSETTINGS=/prepress $< ; \
# 	\evince $(TEXFILE:.tex=.pdf) &

# clean:
# 	@\rm -f \
# 	*.aux \
# 	$(TEXFILE:.tex=.log) \
# 	$(TEXFILE:.tex=.lof) \
# 	$(TEXFILE:.tex=.lot) \
# 	$(TEXFILE:.tex=.toc) \
# 	$(TEXFILE:.tex=.out) \
# 	$(TEXFILE:.tex=.dvi) \
# 	$(TEXFILE:.tex=.pdf) \
# 	$(TEXFILE:.tex=.ps)  \
# 	$(TEXFILE:.tex=.bm)  \
# 	$(TEXFILE:.tex=.bbl) \
# 	$(TEXFILE:.tex=.blg) \
# 		*~

